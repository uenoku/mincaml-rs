#![feature(bind_by_move_pattern_guards)]
#![feature(type_ascription)]
#[macro_use]
extern crate lalrpop_util;
extern crate getopts;
#[macro_use]
extern crate inkwell;
#[macro_use]
extern crate failure;
#[macro_use]
extern crate log;
extern crate env_logger;
mod alpha;
mod arg_parse;
mod closure;
mod hp_alloc;
mod inline;
mod ir;
mod knormal;
mod llvmcodegen;
mod replace_ext;
mod syntax;
mod to_loop;
mod ty;
mod typing;
mod util;
lalrpop_mod!(pub parser); // synthesized by LALRPOP
use self::arg_parse::parse;
use crate::syntax::genvar;
use failure::Error;
use rpds::HashTrieMap;
use std;
use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::Read;
fn type_failed(x: &str) {
    println!("{:?}", x);
    println!("{:?}", parser::ExprParser::new().parse(&x));
    let p = parser::ExprParser::new().parse(&x).unwrap();
    let p = typing::f(p, &HashTrieMap::new());
    assert!(p.is_err());
}
fn type_check(x: &str) {
    println!("{:?}", x);
    println!("{:?}", parser::ExprParser::new().parse(&x));
    let p = parser::ExprParser::new().parse(&x).unwrap();
    let p = typing::f(p, &HashTrieMap::new());
    assert!(p.is_ok());
}

fn check(x: &str) {
    println!("{:?}", x);
    println!("{:?}", parser::ExprParser::new().parse(&x));
    assert!(parser::ExprParser::new().parse(&x).is_ok());
}

#[test]
fn test_int() {
    assert_eq!(parser::IntParser::new().parse("100"), Ok(100));
    assert_eq!(parser::IntParser::new().parse("-22"), Ok(-22));
    assert!(parser::IntParser::new().parse("(22)").is_err());
}

#[test]
fn test_simpleexpr() {
    assert!(parser::ExprParser::new().parse("25").is_ok());
    assert!(parser::ExprParser::new().parse("25.0").is_ok());
    assert!(parser::ExprParser::new().parse("-12.4").is_ok());
    assert!(parser::ExprParser::new().parse("false").is_ok());
}

#[test]
fn test_calc() {
    assert!(parser::ExprParser::new().parse("25 * 20").is_ok());
    assert!(parser::ExprParser::new().parse("25 + 0 * 12").is_ok());
    assert!(parser::ExprParser::new().parse("-12.4 +. 20 *. 2").is_ok());
    assert!(parser::ExprParser::new().parse("1.0/.2.0").is_ok());
}

#[test]
fn test_cond() {
    assert!(parser::ExprParser::new().parse("25 < 20 * 20").is_ok());
    assert!(parser::ExprParser::new().parse("25.2 <> 2.3").is_ok());
}

#[test]
fn test_if() {
    check("if true then 2.0 else if 2.0 <> 3.0 then 2.0 +. 3.0 /. 2.3 else 4.0 + 2.0");
    check("if true then 2.0 else if 2.0 <> 3.0 then 2.0 +. 3.0 /. 2.3 else 4.0 ");
    check("2 + if true then 1 else 4");
}
#[test]
fn test_let() {
    assert!(parser::ExprParser::new()
        .parse("let x = 1 in let y = 2 in x + y")
        .is_ok());
    assert!(parser::ExprParser::new()
        .parse("let x = 1 in let y = 2 in ")
        .is_err());
}
#[test]
fn test_fun() {
    assert!(parser::ExprParser::new()
        .parse("let rec add x y = x + y in 0 ")
        .is_ok());
    assert!(parser::ExprParser::new()
        .parse("let rec id x = x in 0 ")
        .is_ok());
}

#[test]
fn test_tuple() {
    assert!(parser::ExprParser::new()
        .parse("let rec make_tuple x y = (x,y) in 0 ")
        .is_ok());
    assert!(parser::ExprParser::new()
        .parse("let p = (x,y) in 3 ")
        .is_ok());
}

#[test]
fn test_let_tuple() {
    assert!(parser::ExprParser::new()
        .parse("let (x,y) = (1,2) in x ")
        .is_ok());
    assert!(parser::ExprParser::new()
        .parse("let (x)  = (1) in  3 ")
        .is_err());
}

#[test]
fn test_app() {
    check("let x = 1 in let y = 2 in x + y");
    check("let f = g 2 2.3 true (if true then 2 else 3.0) in f ");
    check("let a = f 2 + x in x");
    check("let a = f 2 + f 3 in x ");
}
#[test]
fn test_semicolon() {
    check("print_int 3;2")
}
#[test]
fn test_minrt() {
    check("let a = sqrt (fsqr v.(0) +. fsqr v.(1) +. fsqr v.(2)) in ()");
}
#[test]
fn test_type() {
    type_check("let a = Array.make 2 2 in a.(0) <- if true then 1 else 3 ; a");
}
pub fn add_bulidinfun(
    s: &str,
    ty: ty::Type,
    external: HashTrieMap<String, usize>,
    tyenv: &mut HashMap<usize, ty::Type>,
) -> HashTrieMap<String, usize> {
    let t = syntax::genvar();
    tyenv.insert(t, ty);
    external.insert(String::from(s), t)
}

pub struct Env {
    pub tyenv: HashMap<usize, ty::Type>,
}
pub fn get_ir(path: &String, alpha: bool) -> Result<(Vec<ir::Fundef>, Env), Error> {
    info!("input file = {}", path);
    let mut file = File::open(path).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let p = parser::ExprParser::new().parse(contents.as_str()).unwrap();
    info!("parse end");
    debug!("{:?}", p);
    let (p, external) = replace_ext::f(*p);
    let env = external;
    let mut tyenv = typing::f(Box::new(p.clone()), &env).unwrap();

    info!("type check end");

    let p = knormal::f(Box::new(p), &env, &mut tyenv);
    info!("knormaliz end");
    let p = if alpha { alpha::f(p) } else { *p };
    let p = closure::f(p, &env, &mut tyenv);

    info!("closure coversion end");
    let p = ir::f(p, &mut tyenv);
    let p = to_loop::f(p);
    info!("ir coversion end");
    Ok((p, Env { tyenv }))
}
fn main() -> Result<(), Error> {
    std::env::set_var("RUST_LOG", "info");
    env_logger::init();
    let args: Vec<String> = env::args().collect();
    let opts = parse(args).unwrap();
    let (mut p, mut env) = get_ir(&opts.filename, true)?;
    let mut builtin = HashMap::new();
    builtin.insert(
        String::from("create_array"),
        ty::Type::TyFun(
            vec![ty::Type::TyInt, ty::Type::TyInt],
            Box::new(ty::Type::TyPtr),
        ),
    );
    builtin.insert(
        String::from("create_tuple"),
        ty::Type::TyFun(vec![ty::Type::TyInt], Box::new(ty::Type::TyPtr)),
    );
    builtin.insert(
        String::from("create_array_float"),
        ty::Type::TyFun(
            vec![ty::Type::TyInt, ty::Type::TyFloat],
            Box::new(ty::Type::TyPtr),
        ),
    );

    match opts.globalname {
        Some(x) => {
            let mut extenv = HashMap::new();
            let mut hp = 2;
            let (glb, e) = get_ir(&x, false)?;
            let main = glb[0].clone().alloc(&mut hp, &mut extenv);
            for i in &mut p {
                if (i.name.0.as_str() == "main") {
                    let tmp: Vec<_> = main.blocks[1].inst.clone().into_iter().rev().collect();
                    for j in tmp {
                        i.blocks[1].inst.push_front(j);
                    }
                    i.blocks[1].inst.push_front(ir::Inst::Store {
                        ptr: knormal::Var::Constant(syntax::Const::CPtr(1)),
                        idx: knormal::Var::Constant(syntax::Const::CInt(0)),
                        src: knormal::Var::Constant(syntax::Const::CInt(hp as i32)),
                    });
                }
            }
            e.tyenv.into_iter().for_each(|(x, y)| {
                env.tyenv.insert(x, y);
            });
            debug!("{:?}", env.tyenv);
            llvmcodegen::f(p, env.tyenv, extenv, builtin, opts.filename).unwrap();
        }
        None => {
            info!("{:?}", p[0].clone().inline_all(&p));
            llvmcodegen::f(p, env.tyenv, HashMap::new(), builtin, opts.filename).unwrap();
        }
    };
    Ok(())
}
