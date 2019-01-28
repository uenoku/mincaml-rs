use crate::syntax::Expr::*;
use crate::syntax::{genvar, Expr, Fundef};
use rpds::{HashTrieMap, HashTrieSet};
use std::collections::HashMap;

fn g(expr: Expr, env: &HashTrieSet<String>, global_env: &mut HashMap<String, usize>) -> Expr {
    macro_rules! sub {
        ($e1:expr) => {
            Box::new(g($e1, env, global_env))
        };
    }

    match expr {
        EConst(u) => EConst(u),
        EExt(v) => EExt(v),
        EVar(u) if env.contains(&u) => EVar(u),
        EVar(u) => {
            if !global_env.contains_key(&u) {
                info!("extern: {}", u);
            }
            global_env.insert(u.clone(), genvar());
            EExt(u)
        }
        EOp(op, u) => EOp(
            op,
            u.into_iter()
                .map(|x| Box::new(g(*x, env, global_env)))
                .collect(),
        ),
        EIf(e1, e2, e3) => EIf(sub!(*e1), sub!(*e2), sub!(*e3)),
        ELet((v, ty), e1, e2) => ELet(
            (v.clone(), ty),
            sub!(*e1),
            Box::new(g(*e2, &env.insert(v), global_env)),
        ),
        EApp(f, args) => EApp(sub!(*f), args.into_iter().map(|x| sub!(*x)).collect()),
        ETuple(v) => ETuple(v.into_iter().map(|x| sub!(*x)).collect()),
        ELetTuple(binds, e1, e2) => {
            let e1 = sub!(*e1);
            let env = binds.iter().fold(env.clone(), |acc, (name, ty)| {
                HashTrieSet::insert(&acc, name.clone())
            });
            let e2 = Box::new(g(*e2, &env, global_env));
            ELetTuple(binds, e1, e2)
        }
        ELetRec(fundef, e) => {
            let (name, _) = &fundef.name;
            let env = env.insert(name.clone());
            let e = Box::new(g(*e, &env, global_env));
            let env = fundef.args.iter().fold(env.clone(), |acc, (name, ty)| {
                HashTrieSet::insert(&acc, name.clone())
            });
            let body = Box::new(g(*fundef.body, &env, global_env));
            ELetRec(
                Fundef {
                    body: body,
                    args: fundef.args,
                    name: fundef.name,
                },
                e,
            )
        }
    }
}
pub fn f(e: Expr) -> (Expr, HashTrieMap<String, usize>) {
    let mut gl = HashMap::new();
    let e = g(e, &HashTrieSet::new(), &mut gl);
    let env = gl
        .into_iter()
        .fold(HashTrieMap::new(), |acc, (name, u)| acc.insert(name, u));
    (e, env)
}
