use crate::syntax;
use crate::syntax::Expr::*;
use crate::syntax::{genvar, infer_function_ret, Cmp, Const, Expr, Op};
use crate::ty;
use crate::ty::Type;
use rpds::HashTrieMap;
use std::collections::{HashMap, HashSet};
/*
 * Constantな値に対してはletで束縛せずにkNormalにしてみる
 * これによってForの定数探しとかだいぶ楽になるんやない？
 */
type BE = Box<KExpr>;

#[derive(Clone, Debug)]
pub enum Var {
    OpVar(String, usize),
    Constant(Const),
    Ext(String, usize),
}
impl Var {
    pub fn alpha(self, alias: &HashMap<String, Var>) -> Self {
        match self {
            Var::OpVar(x, y) if alias.contains_key(&x) => alias.get(&x).unwrap().clone(),
            _ => self,
        }
    }
    pub fn set(&self) -> HashSet<String> {
        let mut ret = HashSet::new();
        match self {
            Var::OpVar(y, _) => ret.insert(y.clone()),
            Var::Ext(y, _) => ret.insert(y.clone()),
            _ => false,
        };
        ret
    }
    pub fn OpVar((x, ty): (String, usize)) -> Var {
        Var::OpVar(x, ty)
    }

    pub fn is_constant(&self) -> bool {
        match self {
            Var::Constant(_) => true,
            _ => false,
        }
    }
    pub fn get_name_opt(&self) -> Option<String> {
        match self {
            Var::OpVar(x, y) => Some(x.clone()),
            _ => None,
        }
    }
    pub fn is_name_eq(&self, e: &String) -> bool {
        match self {
            Var::OpVar(x, y) => *e == *x,
            _ => false,
        }
    }
    pub fn get_signedimm(&self) -> Option<i32> {
        match self {
            Var::Constant(Const::CInt(x)) => Some(*x),
            _ => Option::None,
        }
    }
    pub fn getimm(&self) -> Option<usize> {
        match self {
            Var::Constant(Const::CInt(x)) => Some(*x as usize),
            _ => Option::None,
        }
    }
}
pub fn get_var(x: &Var) -> (String, usize) {
    match x {
        Var::OpVar(y, x) | Var::Ext(y, x) => (y.clone(), *x),
        _ => unreachable!(),
    }
}

#[derive(Debug, Clone)]
pub struct Fundef {
    pub name: (String, usize),
    pub args: Vec<(String, usize)>,
    pub body: BE,
}

#[derive(Debug, Clone)]
pub enum KExpr {
    KVar(Var), // KConstant はKVarにマージ
    KOp(Op, Vec<Var>),
    KIf(Cmp, Var, Var, BE, BE),
    KLet((String, usize), BE, BE),
    KLetTuple(Vec<(String, usize)>, Var, BE),
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
    let (k, t) = g(&expr, env, tyenv);
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
    for i in args.iter() {
        let (ki, ti) = g(i, env, tyenv);
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
        Var::Ext(_, d) => tyenv.get(&d).unwrap().clone(),
    }
}
fn g(
    e: &Expr,
    env: &HashTrieMap<String, usize>,
    tyenv: &mut HashMap<usize, Type>,
) -> (Box<KExpr>, Type) {
    match e {
        EConst(c) => (
            Box::new(KExpr::KVar(Var::Constant(c.clone()))),
            syntax::infer_const(&c),
        ),
        EExt(d) => (
            Box::new(KExpr::KVar(Var::Ext(
                d.clone(),
                env.get(&d.to_string()).unwrap().clone(),
            ))),
            tyenv.get(env.get(&d.to_string()).unwrap()).unwrap().clone(),
        ),
        EVar(d) => (
            Box::new(KExpr::KVar(Var::OpVar(
                d.clone(),
                env.get(&d.to_string()).unwrap().clone(),
            ))),
            tyenv.get(env.get(&d.to_string()).unwrap()).unwrap().clone(),
        ),
        EOp(op, args) => {
            let (cls, args) = vec_knormalize(args.to_vec(), env, tyenv);
            let ty = {
                match op {
                    Op::Array => {
                        // Array(number of elements, init)
                        Type::TyArray(Box::new(infer_var(&args[1], tyenv)))
                    }
                    Op::Store => {
                        Type::TyUnit
                        // Store(dest array, index, src)
                        // infer_var(&args[2], tyenv)
                    }
                    Op::Load => {
                        // Load(src array, index)
                        let a = infer_var(&args[0], tyenv);
                        ty::get_element(a)
                    }
                    _ => syntax::infer_op(&op),
                }
            };
            (vec_to_expr(Box::new(KExpr::KOp(op.clone(), args)), cls), ty)
        }
        EIf(e1, e2, e3) => match *e1.clone() {
            EOp(Op::Cond(x), args) => {
                let (v, args) = vec_knormalize(args.to_vec(), env, tyenv);
                let l = args[0].clone();
                let r = args[1].clone();
                let (k2, ty) = g(&e2, env, tyenv);
                let (k3, _) = g(&e3, env, tyenv);
                (
                    vec_to_expr(Box::new(KExpr::KIf(x.clone(), l, r, k2, k3)), v),
                    ty,
                )
            }
            other => {
                let (v, l) = knormalize_sub(Box::new(other), env, tyenv);
                let r = Var::Constant(Const::CBool(true));
                let (k2, ty) = g(&e2, env, tyenv);
                let (k3, _) = g(&e3, env, tyenv);
                (
                    vec_to_expr(Box::new(KExpr::KIf(Cmp::EQ, l, r, k2, k3)), v),
                    ty,
                )
            }
        },
        ELet((name, ty), e1, e2) => {
            let (k1, t1) = g(&e1, env, tyenv);
            let env = env.insert(name.clone(), ty.clone());
            let (k2, t2) = g(&e2, &env, tyenv);
            (
                Box::new(KExpr::KLet((name.clone(), ty.clone()), k1, k2)),
                t2,
            )
        }
        EApp(f, args) => {
            let (mut v1, kargs) = vec_knormalize(args.to_vec(), env, tyenv);
            let (x, f) = knormalize_sub(f.clone(), env, tyenv);
            x.into_iter().for_each(|x| v1.push(x));
            (
                vec_to_expr(Box::new(KExpr::KApp(f.clone(), kargs)), v1),
                infer_function_ret(&infer_var(&f, tyenv)),
            )
        }
        ELetRec(fundef, e) => {
            let (name, ty) = &fundef.name;
            let env = env.insert(name.clone(), ty.clone());
            let (x, f) = g(&e, &env, tyenv);
            let env_ = fundef.args.iter().fold(env.clone(), |acc, (name, ty)| {
                HashTrieMap::insert(&acc, name.clone(), ty.clone())
            });
            let (body, _) = g(&fundef.body, &env_, tyenv);
            (
                Box::new(KExpr::KLetRec(
                    Fundef {
                        name: fundef.name.clone(),
                        body: body,
                        args: fundef.args.clone(),
                    },
                    x,
                )),
                f,
            )
        }
        ETuple(elements) => {
            let (v1, kelements) = vec_knormalize(elements.to_vec(), env, tyenv);
            let ty = kelements
                .to_vec()
                .iter()
                .map(|x| infer_var(x, tyenv))
                .collect();
            (
                vec_to_expr(Box::new(KExpr::KTuple(kelements.to_vec())), v1),
                Type::TyTuple(ty),
            )
        }
        ELetTuple(binds, e1, e2) => {
            let (k1, t1) = g(&e1, env, tyenv);
            let name = syntax::genname();
            let env_ = binds.iter().fold(env.clone(), |acc, (name, ty)| {
                HashTrieMap::insert(&acc, name.clone(), ty.clone())
            });
            match *k1 {
                KExpr::KVar(Var::OpVar(name, ty)) => {
                    let (k2, t2) = g(&e2, &env_, tyenv);
                    (
                        Box::new(KExpr::KLetTuple(binds.to_vec(), Var::OpVar(name, ty), k2)),
                        t2,
                    )
                }
                _ => {
                    let ty = genvar();
                    let env_ = env_.insert(name.clone(), ty);
                    tyenv.insert(ty, t1);
                    let (k2, t2) = g(&e2, &env_, tyenv);
                    (
                        Box::new(KExpr::KLet(
                            (name.clone(), ty),
                            k1,
                            Box::new(KExpr::KLetTuple(binds.to_vec(), Var::OpVar(name, ty), k2)),
                        )),
                        t2,
                    )
                }
            }
        }
    }
}
pub fn f(
    expr: Box<Expr>,
    env: &HashTrieMap<String, usize>,
    tyenv: &mut HashMap<usize, Type>,
) -> Box<KExpr> {
    let (e, ty) = g(&expr, env, tyenv);
    e
}
