use crate::knormal;
use crate::knormal::{KExpr, Var};
use crate::syntax::genname;
use rpds::HashTrieMap;
use std::collections::HashMap;
pub fn alpha_string(e: String, env: &HashTrieMap<String, String>) -> String {
    match env.get(&e) {
        Some(y) => y.clone(),
        None => e,
    }
}
pub fn alpha_var(e: Var, env: &HashTrieMap<String, String>) -> Var {
    match e {
        Var::OpVar(x, y) => Var::OpVar(alpha_string(x, env), y),
        _ => e,
    }
}
pub fn alpha(e: KExpr, env: &HashTrieMap<String, String>) -> KExpr {
    macro_rules! sub {
        ($e1:expr,$env:expr) => {{
            Box::new(alpha(*$e1, $env))
        }};
    }
    macro_rules! newname {
        ($name:expr) => {{
            let mut newname = $name.clone();
            newname.push_str(".");
            newname.push_str(&genname());
            newname
        }};
    }
    match e {
        KExpr::KVar(v) => KExpr::KVar(alpha_var(v, env)),
        KExpr::KOp(op, args) => {
            KExpr::KOp(op, args.into_iter().map(|x| alpha_var(x, env)).collect())
        }
        KExpr::KIf(cmp, x, y, e1, e2) => KExpr::KIf(
            cmp,
            alpha_var(x, env),
            alpha_var(y, env),
            Box::new(alpha(*e1, env)),
            Box::new(alpha(*e2, env)),
        ),
        KExpr::KLet((name, ty), e1, e2) => {
            let mut newname = newname!(name);
            KExpr::KLet(
                (newname.clone(), ty),
                sub!(e1, env),
                sub!(e2, &env.insert(name, newname)),
            )
        }
        KExpr::KApp(f, args) => KExpr::KApp(
            alpha_var(f, env),
            args.into_iter().map(|x| alpha_var(x, env)).collect(),
        ),
        KExpr::KTuple(args) => KExpr::KTuple(args.into_iter().map(|x| alpha_var(x, env)).collect()),
        KExpr::KLetTuple(binds, e1, e2) => {
            let k1 = sub!(e1, env);
            let env_ = binds.iter().fold(env.clone(), |acc, (name, ty)| {
                let newname = newname!(name);
                HashTrieMap::insert(&acc, name.clone(), newname.clone())
            });
            let binds = binds
                .into_iter()
                .map(|(x, ty)| (env_.get(&x).unwrap().clone(), ty))
                .collect();
            let k2 = sub!(e2, &env_);
            (KExpr::KLetTuple(binds, k1, k2))
        }
        KExpr::KLetRec(fundef, e) => {
            let (name, ty) = &fundef.name;
            let newname = newname!(name);
            let env = HashTrieMap::insert(&env, name.clone(), newname.clone());
            let binds = fundef.args;
            let env_ = binds.iter().fold(env.clone(), |acc, (name, ty)| {
                let newname = newname!(name);
                HashTrieMap::insert(&acc, name.clone(), newname.clone())
            });
            let binds = binds
                .into_iter()
                .map(|(x, ty)| (env_.get(&x).unwrap().clone(), ty))
                .collect();
            let body = sub!(fundef.body, &env_);
            let e = sub!(e, &env);
            KExpr::KLetRec(
                knormal::Fundef {
                    name: (newname, *ty),
                    body: body,
                    args: binds,
                },
                e,
            )
        }
    }
}
pub fn f(e: Box<KExpr>) -> KExpr {
    alpha(*e, &HashTrieMap::new())
}
