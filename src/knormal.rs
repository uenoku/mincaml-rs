use crate::syntax;
use crate::syntax::Expr::*;
use crate::syntax::{Const, Expr, Op};
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
pub enum Cmp {
    EQ,
    NE,
    LT,
    GT,
    LE,
    GE,
}

#[derive(Debug, Clone)]
pub enum Var {
    OpVar(String),
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
    KApp(BE, Vec<Var>),
    KTuple(Vec<Var>),
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
                ans.push(Var::OpVar(name.clone()));
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
fn g(
    e: Expr,
    env: &HashTrieMap<String, usize>,
    tyenv: &mut HashMap<usize, Type>,
) -> (Box<KExpr>, Type) {
    match e {
        EConst(c) => (
            Box::new(KExpr::KVar(Var::Constant(c.clone()))),
            syntax::infer_const(c),
        ),
        EOp(op, args) => {
            let (cls, args) = vec_knormalize(args, env, tyenv);
            let opc = op.clone();
            let ty = {
                match opc {
                    Op::Array => {
                        let (a, b, c) = &cls[1];
                        tyenv.get(b).unwrap().clone()
                    }
                    Op::Store => {
                        let (a, b, c) = &cls[2];
                        tyenv.get(b).unwrap().clone()
                    }
                    Op::Load => {
                        let (a, b, c) = &cls[0];
                        ty::get_element(tyenv.get(&b).unwrap().clone())
                    }
                    _ => syntax::infer_op(opc),
                }
            };
            (vec_to_expr(Box::new(KExpr::KOp(op, args)), cls), ty)
        }
        _ => unreachable!(),
    }
}
pub fn f(expr: Box<Expr>, tyenv: &mut HashMap<usize, Type>) -> KExpr {
    KExpr::KVar(Var::OpVar("u".to_string()))
}
