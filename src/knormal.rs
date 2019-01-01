use crate::syntax;
use crate::syntax::Expr::*;
use crate::syntax::{genvar, Cmp, Const, Expr, Op};
use crate::ty;
use crate::ty::Type;
use rpds::HashTrieMap;
use std::collections::HashMap;
/*
 * Constantな値に対してはletで束縛せずにkNormalにしてみる
 * これによってForの定数探しとかだいぶ楽になるんやない？
 */
type BE = Box<KExpr>;

#[derive(Debug, Clone)]
pub enum Var {
    OpVar(String, usize),
    Constant(Const),
}

#[derive(Debug, Clone)]
pub struct Fundef {}
#[derive(Debug, Clone)]
pub enum KExpr {
    KVar(Var), // KConstant はKVarにマージ
    KOp(Op, Vec<Var>),
    KIf(Cmp, Var, Var, BE, BE),
    KLet((String, usize), BE, BE),
    KLetTuple(Vec<(String, usize)>, BE, BE),
    KLetRec(Fundef, BE),
    KApp(Var, Vec<Var>),
    KTuple(Vec<Var>),
}

fn knormalize_sub(
    expr: Box<Expr>,
    env: &HashTrieMap<String, usize>,
    tyenv: &mut HashMap<usize, Type>,
) -> (Vec<(String, usize, Box<KExpr>)>, Var) {
    let mut cls = vec![];
    let (k, t) = g(*expr, env, tyenv);
    let name = match *k {
        KExpr::KVar(v) => v,
        _ => {
            let name = syntax::genname();
            let ty = syntax::genvar();
            tyenv.insert(ty, t);
            cls.push((name.clone(), ty, k));
            Var::OpVar(name, ty)
        }
    };
    (cls, name)
}
fn vec_knormalize(
    args: Vec<Box<Expr>>,
    env: &HashTrieMap<String, usize>,
    tyenv: &mut HashMap<usize, Type>,
) -> (Vec<(String, usize, Box<KExpr>)>, Vec<Var>) {
    let mut ans = vec![];
    let mut cls = vec![];
    for i in args.into_iter() {
        let (ki, ti) = g(*i, env, tyenv);
        match *ki {
            KExpr::KVar(v) => ans.push(v),
            _ => {
                let name = syntax::genname();
                let ty = syntax::genvar();
                ans.push(Var::OpVar(name.clone(), ty));
                tyenv.insert(ty, ti);
                cls.push((name, ty, ki));
            }
        }
    }
    (cls, ans)
}
fn vec_to_expr(expr: Box<KExpr>, args: Vec<(String, usize, Box<KExpr>)>) -> Box<KExpr> {
    args.into_iter().fold(expr, |acc, (name, ty, expr)| {
        Box::new(KExpr::KLet((name, ty), expr, acc))
    })
}
fn infer_var(v: &Var, tyenv: &mut HashMap<usize, Type>) -> Type {
    match v {
        Var::Constant(c) => syntax::infer_const(c),
        Var::OpVar(_, d) => tyenv.get(&d).unwrap().clone(),
    }
}
fn g(
    e: Expr,
    env: &HashTrieMap<String, usize>,
    tyenv: &mut HashMap<usize, Type>,
) -> (Box<KExpr>, Type) {
    match e {
        EConst(c) => (
            Box::new(KExpr::KVar(Var::Constant(c.clone()))),
            syntax::infer_const(&c),
        ),
        EVar(d) => (
            Box::new(KExpr::KVar(Var::OpVar(
                d.clone(),
                env.get(&d).unwrap().clone(),
            ))),
            tyenv.get(env.get(&d).unwrap()).unwrap().clone(),
        ),
        EOp(op, args) => {
            let (cls, args) = vec_knormalize(args, env, tyenv);
            let ty = {
                match op {
                    Op::Array => {
                        // Array(number of elements, init)
                        infer_var(&args[1], tyenv)
                        // let (a,b) = &args[1];
                        // tyenv.get(b).unwrap().clone()
                    }
                    Op::Store => {
                        // Store(dest array, index, src)
                        infer_var(&args[2], tyenv)
                        // let (a, b) = &args[2];
                        // tyenv.get(b).unwrap().clone()
                    }
                    Op::Load => {
                        // Load(src array, index)
//                        let (a, b) = &args[0];
                        let a = infer_var(&args[0], tyenv);
                        ty::get_element(a)
                    }
                    _ => syntax::infer_op(&op),
                }
            };
            (vec_to_expr(Box::new(KExpr::KOp(op, args)), cls), ty)
        }
        EIf(e1, e2, e3) => match *e1 {
            EOp(Op::Cond(x), args) => {
                let (v, args) = vec_knormalize(args, env, tyenv);
                let l = args[0].clone();
                let r = args[1].clone();
                let (k2, ty) = g(*e2, env, tyenv);
                let (k3, _) = g(*e3, env, tyenv);
                (vec_to_expr(Box::new(KExpr::KIf(x, l, r, k2, k3)), v), ty)
            }
            _ => {
                let (v, args) = vec_knormalize(vec![e1], env, tyenv);
                let l = args[0].clone();
                let r = Var::Constant(Const::CBool(true));
                let (k2, ty) = g(*e2, env, tyenv);
                let (k3, _) = g(*e3, env, tyenv);
                (
                    vec_to_expr(Box::new(KExpr::KIf(Cmp::EQ, l, r, k2, k3)), v),
                    ty,
                )
            }
        },
        ELet((name, ty), e1, e2) => {
            let (k1, t1) = g(*e1, env, tyenv);
            let env = env.insert(name.clone(), ty);
            let (k2, t2) = g(*e2, &env, tyenv);
            (Box::new(KExpr::KLet((name, ty), k1, k2)), t2)
        }
        ,
        EApp(f,args) => {
            let (mut v1,kargs) = vec_knormalize(args,env,tyenv);
            let x = knormalize_sub(f,env,tyenv);
            v1.push(v2);
        }
        _ => unreachable!(),
    }
}
pub fn f(
    expr: Box<Expr>,
    env: &HashTrieMap<String, usize>,
    tyenv: &mut HashMap<usize, Type>,
) -> Box<KExpr> {
    let (e, ty) = g(*expr, env, tyenv);
    e
}
