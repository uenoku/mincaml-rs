use crate::knormal;
use crate::knormal::KExpr::*;
use crate::knormal::Var;
use crate::syntax::{genvar, infer_function_ret, Cmp, Const, Expr, Op};
use crate::ty::Type;
use crate::util::concat;
use rpds::{HashTrieMap, HashTrieSet, List};
use std::collections::{HashMap, HashSet};
type BE = Box<CExpr>;

#[derive(Debug, Clone)]
pub struct Fundef {
    pub name: (String, usize),
    pub args: Vec<(String, usize)>,
    pub formal_fv: Vec<(String, usize)>,
    pub body: BE,
}
#[derive(Debug, Clone)]
pub struct Closure {
    entry: String,
    actual_fv: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum CExpr {
    CVar(Var), // KConstant はKVarにマージ
    COp(Op, Vec<Var>),
    CIf(Cmp, Var, Var, BE, BE),
    CLet((String, usize), BE, BE),
    CLetTuple(Vec<(String, usize)>, Var, BE),
    CTuple(Vec<Var>),
    CMakeCls((String, usize), Closure, BE),
    CAppCls(String, Vec<Var>),
    CAppDir(String, Vec<Var>),
}

pub fn proj_only_var(e: &Var) -> HashSet<String> {
    match e {
        Var::OpVar(e, _) => {
            let mut tmp = HashSet::new();
            tmp.insert(e.clone());
            tmp
        }
        _ => HashSet::new(),
    }
}
// これ大丈夫なんか？
pub fn fv(e: &CExpr) -> HashSet<String> {
    macro_rules! of_vec {
        ($elements:expr) => {
            $elements
                .into_iter()
                .fold(HashSet::<String>::new(), |acc, i| {
                    acc.union(&proj_only_var(&i)).cloned().collect()
                })
        };
    }
    match e {
        CExpr::CVar(v) => proj_only_var(v),
        CExpr::COp(a, b) => of_vec!(b),
        CExpr::CIf(cmp, x, y, e1, e2) => {
            let f1 = fv(e1);
            let f2 = fv(e2);
            ([x, y])
                .iter()
                .fold(f1.union(&f2).cloned().collect(), |acc, i| {
                    acc.union(&proj_only_var(&i)).cloned().collect()
                })
        }
        CExpr::CLet((name, _), e1, e2) => {
            let f1 = fv(&e1);
            let mut f2 = fv(&e2);
            f2.remove(name);
            f1.union(&f2).cloned().collect()
        }
        CExpr::CLetTuple(binds, e1, e2) => {
            let mut f2 = fv(&e2);
            binds.iter().for_each(|x| {
                f2.remove(&x.0);
            });
            f2
            //f1.union(&f2).cloned().collect()
        }
        CExpr::CTuple(elements) => of_vec!(elements),
        CExpr::CAppDir(f, args) => of_vec!(args),
        CExpr::CAppCls(f, args) => {
            let mut a = of_vec!(args);
            a.insert(f.to_string());
            a
        }
        CExpr::CMakeCls((f, _), cls, e) => {
            let mut f1 = fv(&e);
            cls.actual_fv.iter().for_each(|x| {
                f1.insert(x.to_string());
            });
            f1.remove(f);
            f1
        }
    }
}
fn g(
    e: knormal::KExpr,
    env: &HashTrieMap<String, usize>,
    tyenv: &mut HashMap<usize, Type>,
    known: &HashTrieSet<String>,
) -> (CExpr, List<Fundef>) {
    macro_rules! sub {
        ($e1:expr,$env:expr) => {{
            let (a, b) = g(*$e1.clone(), $env, tyenv, known);
            (Box::new(a), b)
        }};
    }
    match e {
        KVar(v) => (CExpr::CVar(v), List::new()),
        KOp(a, b) => (CExpr::COp(a, b), List::new()),
        KIf(cmp, x, y, e1, e2) => {
            let (e1, t1) = sub!(e1, env);
            let (e2, t2) = sub!(e2, env);
            (CExpr::CIf(cmp, x, y, e1, e2), concat(t1, t2))
        }
        KLet((name, ty), e1, e2) => {
            let (e1, t1) = sub!(e1, env);
            let (e2, t2) = sub!(e2, &env.insert(name.clone(), ty));
            (CExpr::CLet((name, ty), e1, e2), concat(t1, t2))
        }
        KLetTuple(binds, e1, e2) => {
            //let (e1, t1) = sub!(e1, env);
            let env_ = binds.iter().fold(env.clone(), |acc, (name, ty)| {
                HashTrieMap::insert(&acc, name.clone(), ty.clone())
            });
            let (e2, t2) = sub!(e2, &env_);
            (CExpr::CLetTuple(binds, e1, e2), t2)
        }
        KTuple(elements) => (CExpr::CTuple(elements), List::new()),
        KApp(Var::OpVar(f, _), args) if !known.contains(&f) => {
            (CExpr::CAppCls(f, args), List::new())
        }
        KApp(f, args) => (CExpr::CAppDir(knormal::get_var(&f), args), List::new()),
        KLetRec(fundef, e) => {
            let (name, ty) = &fundef.name;
            let env = env.insert(name.clone(), ty.clone());
            let env_ = fundef.args.iter().fold(env.clone(), |acc, (name, ty)| {
                HashTrieMap::insert(&acc, name.clone(), ty.clone())
            });
            let known_ = known.insert(name.clone());
            let (body, body_t) = g(*fundef.body.clone(), &env_, tyenv, &known_);
            let mut zs = fv(&body);
            fundef.args.iter().for_each(|x| {
                zs.remove(&x.0);
            });
            let (known_, body, body_t) = if zs.is_empty() {
                (known_, body, body_t)
            } else {
                let (body, body_t) = sub!(fundef.body, &env_);
                info!("found closure name: {}", name);
                (known.clone(), *body, body_t)
            };
            let mut zs = fv(&body);
            fundef.args.iter().for_each(|x| {
                zs.remove(&x.0);
            });
            zs.remove(name);
            let zs: Vec<String> = zs.iter().cloned().collect();
            let zst: Vec<(String, usize)> = zs
                .iter()
                .map(|x| (x.clone(), env.get(x).unwrap().clone()))
                .collect();
            let table = body_t.push_front(Fundef {
                name: (name.clone(), ty.clone()),
                args: fundef.args.clone(),
                formal_fv: zst,
                body: Box::new(body),
            });
            let (e2, t2) = g(*e.clone(), &env, tyenv, &known_);
            if fv(&e2).contains(name) {
                (
                    CExpr::CMakeCls(
                        (name.clone(), ty.clone()),
                        Closure {
                            entry: name.clone(),
                            actual_fv: zs,
                        },
                        Box::new(e2),
                    ),
                    concat(t2, table),
                )
            } else {
                (e2, concat(t2, table))
            }
        }
        _ => unreachable!(),
    }
}

pub fn f(
    e: knormal::KExpr,
    env: &HashTrieMap<String, usize>,
    tyenv: &mut HashMap<usize, Type>,
) -> (CExpr, List<Fundef>) {
    let known: HashTrieSet<String> = env
        .iter()
        .fold(HashTrieSet::new(), |acc, (x, y)| acc.insert(x.clone()));
    g(e, env, tyenv, &known)
}
