use crate::syntax::Expr::*;
use crate::syntax::{genvar, Const, Expr, Op, Var};
use crate::ty;
use crate::ty::Type;
use crate::ty::Type::*;
use crate::util;
use crate::util::{concat, concat_com, destruct, from_vec, map};
use rpds::{HashTrieMap, List};
use std::collections::HashMap;

#[derive(Debug)]
pub enum TypingError {
    UnifyError((Type, Type)),
    OccurenceCheckError(usize),
    UnboundedVariableError(Var),
    MainIsNotVoid,
}
fn occurence_check(ty: &Type, v: &usize) -> Result<(), TypingError> {
    match ty {
        TyVar(u) if *u == *v => Err(TypingError::OccurenceCheckError(*u)),
        TyArray(u) => occurence_check(&u, v),
        TyFun(args, ret) => {
            occurence_check(&ret, v)?;
            args.iter().try_for_each(|x| occurence_check(x, v))
        }
        TyTuple(args) => args.iter().try_for_each(|x| occurence_check(x, v)),
        _ => Ok(()),
    }
}

fn subst(ty: &Type, v: &usize, to: &Type) -> Type {
    let f = |x: &Vec<Type>| x.iter().map(|y| subst(y, v, to)).collect();
    match ty {
        TyVar(u) if *u == *v => to.clone(),
        TyArray(u) => TyArray(Box::new(subst(&u, v, to))),
        TyFun(args, ret) => TyFun(f(&args), Box::new(subst(&ret, v, to))),
        TyTuple(args) => TyTuple(f(&args)),
        _ => ty.clone(),
    }
}
fn gen_constraints(lhs: Vec<Type>, rhs: Vec<Type>, c: List<(Type, Type)>) -> List<(Type, Type)> {
    let mut a = vec![];
    for i in 0..lhs.len() {
        a.push((lhs[i].clone(), rhs[i].clone()))
    }
    concat_com(from_vec(&a), c)
}
fn unify(
    constrains: List<(Type, Type)>,
    unifier: &mut HashMap<usize, Type>,
) -> Result<(), TypingError> {
    if constrains.first().is_none() {
        return Ok(());
    };
    let (fst, rest) = destruct(constrains).unwrap();
    match fst {
        (TyInt, TyInt) => unify(rest, unifier),
        (TyBool, TyBool) => unify(rest, unifier),
        (TyUnit, TyUnit) => unify(rest, unifier),
        (TyFloat, TyFloat) => unify(rest, unifier),
        (TyVar(x), TyVar(y)) if x == y => unify(rest, unifier),
        (TyVar(x), rhs) => {
            occurence_check(&rhs, &x)?;
            // restの全てのxの出現をrhsに置き換える

            let rhs_cls = rhs.clone();
            let c = map(
                rest.clone(),
                Box::new(move |(l, r)| (subst(&l, &x, &rhs_cls), subst(&r, &x, &rhs_cls))),
            );
            // unifierの全てのxの出現をrhsに置き換える
            unifier
                .iter_mut()
                .for_each(|(key, val)| *val = subst(&val, &x, &rhs));
            if !unifier.contains_key(&x) {
                unifier.insert(x, rhs);
            }
            unify(c, unifier)?;
            Ok(())
        }
        (lhs, TyVar(x)) => unify(rest.push_front((TyVar(x), lhs)), unifier),
        (TyFun(args1, ret1), TyFun(args2, ret2)) if args1.len() == args2.len() => unify(
            gen_constraints(args1, args2, rest.push_front((*ret1, *ret2))),
            unifier,
        ),
        (TyTuple(t1), TyTuple(t2)) if t1.len() == t2.len() => {
            unify(gen_constraints(t1, t2, rest), unifier)
        }
        (TyArray(l), TyArray(r)) => unify(rest.push_front((*l, *r)), unifier),
        _ => Err(TypingError::UnifyError(fst.clone())),
    }
}

fn g(
    expr: &Expr,
    env: &HashTrieMap<String, usize>,
    constraints: &mut Vec<(Type, Type)>,
) -> Result<Type, TypingError> {
    macro_rules! uni {
        ($e1:expr,$src:expr,$dst:expr) => {{
            let t1 = g(&$e1, env, constraints)?;
            constraints.push((t1, $src));
            Ok($dst)
        }};
    }
    macro_rules! bin {
        ($e1:expr,$e2:expr,$src:expr,$dst:expr) => {{
            let t1 = g(&$e1, env, constraints)?;
            let t2 = g(&$e2, env, constraints)?;
            constraints.push((t1, $src));
            constraints.push((t2, $src));
            Ok($dst)
        }};
    }
    macro_rules! no_constraints {
        ($e1:expr) => {
            Ok($e1)
        };
    }
    macro_rules! sub {
        ($e1:expr) => {
            g(&$e1.clone(), env, constraints)?;
        };
    }
    match expr {
        EConst(Const::CInt(_)) => no_constraints!(Type::TyInt),
        EConst(Const::CFloat(_)) => no_constraints!(Type::TyFloat),
        EConst(Const::CBool(_)) => no_constraints!(Type::TyBool),
        EConst(Const::CUnit) => no_constraints!(Type::TyUnit),
        EConst(Const::CPtr(_)) => no_constraints!(Type::TyPtr),

        EExt(u) => no_constraints!(Type::TyVar(env.get(u).unwrap().clone())),
        EVar(v) => {
            let x = env.get(v);
            match x {
                Some(y) => no_constraints!(Type::TyVar(y.clone())),
                None => Err(TypingError::UnboundedVariableError(v.to_string())),
            }
        }
        // 厳しい
        EOp(op, v) => match (op, v.as_slice()) {
            (Op::Add, [e1, e2]) => bin!(e1, e2, TyInt, TyInt),
            (Op::Sub, [e1, e2]) => bin!(e1, e2, TyInt, TyInt),
            (Op::Mul, [e1, e2]) => bin!(e1, e2, TyInt, TyInt),
            (Op::Div, [e1, e2]) => bin!(e1, e2, TyInt, TyInt),
            (Op::FAdd, [e1, e2]) => bin!(e1, e2, TyFloat, TyFloat),
            (Op::FSub, [e1, e2]) => bin!(e1, e2, TyFloat, TyFloat),
            (Op::FMul, [e1, e2]) => bin!(e1, e2, TyFloat, TyFloat),
            (Op::FDiv, [e1, e2]) => bin!(e1, e2, TyFloat, TyFloat),
            (Op::Cond(_), [e1, e2]) => bin!(e1, e2, TyVar(genvar()), TyBool),
            (Op::Not, [e1]) => uni!(e1, TyBool, TyBool),
            (Op::Neg, [e1]) => uni!(e1, TyInt, TyInt),
            (Op::FNeg, [e1]) => uni!(e1, TyFloat, TyFloat),
            (Op::Array, [e1, e2]) => {
                let (t1) = sub!(e1);
                let (t2) = sub!(e2);
                constraints.push((t1.clone(), TyInt));
                Ok((TyArray(Box::new(t2))))
            }
            (Op::Load, [e1, e2]) => {
                let (t1) = sub!(e1);
                let (t2) = sub!(e2);
                let alpha = ty::alpha();
                constraints.push((t1, TyArray(Box::new(alpha.clone()))));
                constraints.push((t2, TyInt));
                Ok(alpha)
            }
            (Op::Store, [e1, e2, e3]) => {
                let (t1) = sub!(e1);
                let (t2) = sub!(e2);
                let (t3) = sub!(e3);
                constraints.push((t1, TyArray(Box::new(t3.clone()))));
                constraints.push((t2, TyInt));
                Ok(TyUnit)
            }
            _ => unreachable!(),
        },
        EIf(e1, e2, e3) => {
            let (t1) = sub!(e1);
            let (t2) = sub!(e2);
            let (t3) = sub!(e3);
            constraints.push((t1, TyBool));
            constraints.push((t2.clone(), t3));
            Ok(t2)
        }
        ELet((v, ty), e1, e2) => {
            let (t1) = sub!(e1);
            let env = HashTrieMap::insert(env, v.to_string(), ty.clone());
            let (t2) = g(&e2, &env, constraints)?;
            constraints.push((TyVar(ty.clone()), t1));
            Ok(t2)
        }
        ELetTuple(v, e1, e2) => {
            let (t1) = sub!(e1);
            let vars: Vec<Type> = v.iter().map(|(x, y)| TyVar(*y)).collect();
            let env = v.into_iter().fold(env.clone(), |acc, (name, ty)| {
                HashTrieMap::insert(&acc, name.clone(), ty.clone())
            });
            let (t2) = g(&e2, &env, constraints)?;
            constraints.push((t1, TyTuple(vars)));
            Ok(t2)
        }
        ETuple(v) => {
            let mut ts = vec![];
            for i in v.into_iter() {
                let (ti) = g(&i, env, constraints)?;
                ts.push(ti);
            }
            Ok((TyTuple(ts)))
        }
        ELetRec(fundef, e) => {
            // Let多相入れてえ
            let (name, ty) = &fundef.name;
            let env = HashTrieMap::insert(env, name.clone(), ty.clone());
            let (t1) = g(&e, &env, constraints)?;
            let mut vars: Vec<Type> = fundef.args.iter().map(|(x, y)| TyVar(y.clone())).collect();

            let env_ = fundef.args.iter().fold(env.clone(), |acc, (name, ty)| {
                HashTrieMap::insert(&acc, name.clone(), ty.clone())
            });
            let (t2) = g(&fundef.body, &env_, constraints)?;
            constraints.push((TyVar(ty.clone()), TyFun(vars, Box::new(t2.clone()))));
            Ok(t1)
        }
        EApp(f, args) => {
            let alpha = TyVar(genvar());
            let (t1) = sub!(f);
            let mut ts = vec![];
            for i in args.iter() {
                let (ti) = sub!(i);
                ts.push(ti);
            }
            constraints.push((t1, TyFun(ts, Box::new(alpha.clone()))));
            Ok(alpha)
        }
    }
}
pub fn f(
    expr: Box<Expr>,
    env: &HashTrieMap<String, usize>,
) -> Result<HashMap<usize, Type>, TypingError> {
    let mut unifier = HashMap::new();
    let mut constrains = Vec::new();
    let ty = g(&expr, &env, &mut constrains)?;

    let constrains = util::from_vec(&constrains);
    unify(constrains, &mut unifier)?;
    let ty = unifier.iter().fold(ty, |acc, (key, v)| subst(&acc, key, v));

    Ok(unifier)
    // match ty {
    //     ty::Type::TyUnit | ty::Type::TyVar(_)  => Ok(unifier),
    //     _ => {
    //         Err(TypingError::MainIsNotVoid)
    //     }
    // }
}
