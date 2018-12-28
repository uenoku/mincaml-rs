use crate::syntax::Expr::*;
use crate::syntax::{Const, Expr, Var};
use crate::ty::Type;
use crate::ty::Type::*;

use rpds::HashTrieMap;
use std::collections::HashMap;
pub enum TypingError {
    UnifyError(Type, Type),
    OccurenceError(usize),
}

fn occurence_check(ty: Type, v: usize) -> Result<(), TypingError> {
    Ok(())
}
fn unify(l: Type, r: Type, mut unifier: &mut HashMap<usize, Type>) -> Result<(), TypingError> {
    match (l.clone(), r.clone()) {
        (TyInt, TyInt) => Ok(()),
        (TyBool, TyBool) => Ok(()),
        (TyUnit, TyUnit) => Ok(()),
        (TyFloat, TyFloat) => Ok(()),
        (TyVar(u), TyVar(v)) if u == v => Ok(()),
        (TyVar(u), TyVar(v)) if u != v && unifier.contains_key(&u) && unifier.contains_key(&v) => {
            // 両方にunifyをする
            let u = unifier.get(&u).unwrap().clone();
            let v = unifier.get(&v).unwrap().clone();
            unify(u, v, unifier)
        }
        (TyVar(u), TyVar(v)) if u != v && unifier.contains_key(&u) => {
            // v := uとする
            let u = unifier.get(&u).unwrap().clone();
            occurence_check(u.clone(), v)?;
            unifier.insert(v, u);
            Ok(())
        }
        (TyFun(args1, ret1), TyFun(args2, ret2)) if args1.len() == args2.len() => {
            for i in 0..args1.len() {
                unify(args1[i].clone(), args2[i].clone(), &mut unifier)?;
            }
            unify(*ret1, *ret2, unifier)
        }
        // (TyTuple(t1), TyTuple(t2)) if t1.len() == t2.len() => {
        //     for i in 0..t1.len() {
        //         unify(&t1[i], &t2[i],unifier)?;
        //     }
        //     Ok(())
        // },
        // (TyArray(l), TyArray(r)) => {
        //     unify(&l,&r,unifier)
        // },
        _ => Err(TypingError::UnifyError(l.clone(), r.clone())),
    }
}
fn g(expr: Box<Expr>, env: HashTrieMap<String, usize>, unifier: &mut HashMap<usize, Type>) -> Type {
    match *expr {
        EConst(Const::CInt(_)) => Type::TyInt,
        EConst(Const::CFloat(_)) => (Type::TyFloat),
        EConst(Const::CBool(_)) => (Type::TyBool),
        EConst(Const::CUnit) => (Type::TyUnit),
        EVar(v) => (Type::TyVar(*env.get(&v.name).unwrap())),
        _ => (Type::TyInt),
    }
}
pub fn f(expr: Box<Expr>) -> Box<Expr> {
    expr
}
