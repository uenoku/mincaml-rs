use crate::syntax::Expr::*;
use crate::syntax::{genvar, Const, Expr, Op, Var};
use crate::ty;
use crate::ty::Type;
use crate::ty::Type::*;

use crate::util::{concat, concat_com, destruct, from_vec, map};
use rpds::{HashTrieMap, List};
use std::collections::HashMap;

#[derive(Debug)]
pub enum TypingError {
    UnifyError((Type, Type)),
    OccurenceCheckError(usize),
    UnboundedVariableError(Var),
}
fn occurence_check(ty: Type, v: usize) -> Result<(), TypingError> {
    match ty {
        TyVar(u) if u == v => Err(TypingError::OccurenceCheckError(u)),
        TyArray(u) => occurence_check(*u, v),
        TyFun(args, ret) => {
            occurence_check(*ret, v);
            args.into_iter().try_for_each(|x| occurence_check(x, v))
        }
        TyTuple(args) => args.into_iter().try_for_each(|x| occurence_check(x, v)),
        _ => Ok(()),
    }
}

fn subst(ty: Type, v: usize, to: Type) -> Type {
    let f = |x: Vec<Type>| x.into_iter().map(|y| subst(y, v, to.clone())).collect();
    match ty {
        TyVar(u) if u == v => to,
        TyArray(u) => TyArray(Box::new(subst(*u, v, to))),
        TyFun(args, ret) => TyFun(f(args), Box::new(subst(*ret, v, to))),
        TyTuple(args) => TyTuple(f(args)),
        _ => ty,
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
            occurence_check(rhs.clone(), x);
            let rhs_cls = rhs.clone();
            // restの全てのxの出現をrhsに置き換える
            let c = map(
                rest.clone(),
                Box::new(move |(l, r)| {
                    (subst(l, x, rhs_cls.clone()), subst(r, x, rhs_cls.clone()))
                }),
            );
            // unifierの全てのxの出現をrhsに置き換える
            unifier
                .iter_mut()
                .for_each(|(key, val)| *val = subst(val.clone(), x, rhs.clone()));
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
    expr: Box<Expr>,
    env: &HashTrieMap<String, usize>,
) -> Result<(Type, List<(Type, Type)>), TypingError> {
    macro_rules! uni {
        ($e1:expr,$src:expr,$dst:expr) => {{
            let (t1, c1) = g($e1.clone(), env)?;
            let c = c1.push_front((t1, $src));
            Ok(($dst, c))
        }};
    }
    macro_rules! bin {
        ($e1:expr,$e2:expr,$src:expr,$dst:expr) => {{
            let (t1, c1) = g($e1.clone(), env)?;
            let (t2, c2) = g($e2.clone(), env)?;
            let c = concat_com(c1, c2)
                .push_front((t1, $src))
                .push_front((t2, $src));
            Ok(($dst, c))
        }};
    }
    macro_rules! no_constraints {
        ($e1:expr) => {
            Ok(($e1, List::new()))
        };
    }
    macro_rules! sub {
        ($e1:expr) => {
            g($e1.clone(), env)?;
        };
    }
    match *expr {
        EConst(Const::CInt(_)) => no_constraints!(Type::TyInt),
        EConst(Const::CFloat(_)) => no_constraints!(Type::TyFloat),
        EConst(Const::CBool(_)) => no_constraints!(Type::TyBool),
        EConst(Const::CUnit) => no_constraints!(Type::TyUnit),
        EVar(v) => {
            println!("{}", v);
            let x = env.get(&v);
            match x {
                Some(y) => no_constraints!(Type::TyVar(*y)),
                None => Err(TypingError::UnboundedVariableError(v)),
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
            (Op::EQ, [e1, e2]) => bin!(e1, e2, TyVar(genvar()), TyBool),
            (Op::NE, [e1, e2]) => bin!(e1, e2, TyVar(genvar()), TyBool),
            (Op::LE, [e1, e2]) => bin!(e1, e2, TyVar(genvar()), TyBool),
            (Op::LT, [e1, e2]) => bin!(e1, e2, TyVar(genvar()), TyBool),
            (Op::GE, [e1, e2]) => bin!(e1, e2, TyVar(genvar()), TyBool),
            (Op::GT, [e1, e2]) => bin!(e1, e2, TyVar(genvar()), TyBool),
            (Op::Not, [e1]) => uni!(e1, TyBool, TyBool),
            (Op::Neg, [e1]) => uni!(e1, TyInt, TyInt),
            (Op::FNeg, [e1]) => uni!(e1, TyFloat, TyFloat),
            (Op::Array, [e1, e2]) => {
                let (t1, c1) = g(e1.clone(), env)?;
                let (t2, c2) = g(e2.clone(), env)?;
                let c = concat_com(c1, c2).push_front((t1.clone(), TyInt));
                Ok((TyArray(Box::new(t2)), c))
            }
            (Op::Load, [e1, e2]) => {
                let (t1, c1) = g(e1.clone(), env)?;
                let (t2, c2) = g(e2.clone(), env)?;
                let alpha = ty::alpha();
                let c = concat_com(c1, c2)
                    .push_front((t1, TyArray(Box::new(alpha.clone()))))
                    .push_front((t2, TyInt));
                Ok((alpha, c))
            }
            (Op::Store, [e1, e2, e3]) => {
                let (t1, c1) = g(e1.clone(), env)?;
                let (t2, c2) = g(e2.clone(), env)?;
                let (t3, c3) = g(e3.clone(), env)?;
                let c = concat_com(c1, concat_com(c2, c3))
                    .push_front((t1, TyArray(Box::new(t3.clone()))))
                    .push_front((t2, TyInt));
                Ok((t3, c))
            }
            _ => unreachable!(),
        },
        EIf(e1, e2, e3) => {
            let (t1, c1) = sub!(e1);
            let (t2, c2) = sub!(e2);
            let (t3, c3) = sub!(e3);
            let c = concat_com(concat_com(c1, c2), c3);
            let c = c.push_front((t1, TyBool)).push_front((t2.clone(), t3));
            Ok((t2, c))
        }
        ELet((v, ty), e1, e2) => {
            let (t1, c1) = sub!(e1);
            let env = HashTrieMap::insert(env, v, ty);
            let (t2, c2) = g(e2.clone(), &env)?;
            Ok((t2, concat_com(c1, c2).push_front((TyVar(ty), t1))))
        }
        ELetTuple(v, e1, e2) => {
            let (t1, c1) = sub!(e1);
            let vars: Vec<Type> = v.iter().map(|(x, y)| TyVar(*y)).collect();
            let env = v.into_iter().fold(env.clone(), |acc, (name, ty)| {
                HashTrieMap::insert(&acc, name, ty)
            });
            let (t2, c2) = g(e2.clone(), &env)?;
            let c = concat_com(c1, c2).push_front((t1, TyTuple(vars)));
            Ok((t2, c))
        }
        ETuple(v) => {
            let mut cs = List::new();
            let mut ts = vec![];
            for i in v.into_iter() {
                let (ti, ci) = g(i, env)?;
                cs = concat_com(cs, ci);
                ts.push(ti);
            }
            Ok((TyTuple(ts), cs))
        }
        ELetRec(fundef, e) => {
            // Let多相入れてえ
            let (name, ty) = fundef.name;
            let env = HashTrieMap::insert(env, name.clone(), ty);
            let (t1, c1) = g(e, &env)?;
            let mut vars: Vec<Type> = fundef.args.iter().map(|(x, y)| TyVar(*y)).collect();

            let env_ = fundef
                .args
                .into_iter()
                .fold(env.clone(), |acc, (name, ty)| {
                    HashTrieMap::insert(&acc, name, ty)
                });
            let (t2, c2) = g(fundef.body.clone(), &env_)?;
            let c = concat_com(c1, c2).push_front((TyVar(ty), TyFun(vars, Box::new(t2.clone()))));
            Ok((t1, c))
        }
        EApp(f, args) => {
            let alpha = TyVar(genvar());
            let (t1, c1) = sub!(f);
            let mut cs = c1;
            let mut ts = vec![];
            for i in args.iter() {
                let (ti, ci) = sub!(i);
                cs = concat_com(ci, cs);
                ts.push(ti);
            }
            let cs = cs.push_front((t1, TyFun(ts, Box::new(alpha.clone()))));
            Ok((alpha, cs))
        }
        _ => unreachable!(),
    }
}
pub fn f(expr: Box<Expr>) -> Result<HashMap<usize, Type>, TypingError> {
    let global_hardcode = vec![
        "floor",
        "not",
        "int_of_float",
        "print_char",
        "print_int",
        "read_int",
        "read_float",
        "reduction",
        "kernel_cos",
        "kernel_sin",
        "kernel_atan",
        "create_array",
        "float_of_int",
        "sqrt",
    ];
    let mut unifier = HashMap::new();
    let env = global_hardcode
        .into_iter()
        .fold(HashTrieMap::new(), |acc, i| {
            HashTrieMap::insert(&acc, i.to_string(), genvar())
        });
    let (_, constrains) = g(expr, &env)?;
    unify(constrains, &mut unifier)?;
    Ok(unifier)
}
