use crate::ir;
use crate::knormal;
use crate::syntax;
use rpds::{HashTrieMap, HashTrieSet, List};
/*
現在の仮定のもとでどっちに行くか分からないかつ片方でwriteして片方でwriteしない領域がある場合終わり
*/
struct Analyzer<'a> {
    functions: &'a Vec<ir::Fundef>,
    fun: &'a ir::Fundef,
}
#[derive(Debug, Clone, Hash, Eq, PartialEq, PartialOrd)]
pub enum Value {
    Unit,
    Int(i32),
    Iter(i32),
    Float(u32),
    Bool(bool),
    Global(String),
    Arg(usize),
    Elm(Box<Value>, Box<Value>),
    Trace_add(Box<Value>, Box<Value>),
    Unknown,
}

// ptrがlabel以降writtenされないとするとenvが成立してptrの中身がansである
#[derive(Debug, Clone)]
struct Hypo {
    order: usize, // 到達する順番
    label: String,
    env: HashTrieMap<String, Value>,
    value: Value,
}
pub enum AnalyzerError {
    Contradiction,
    WARNotDependent(Value),
    UnrechablePath,
}
pub struct Hypothesis {
    body: HashTrieMap<Value, Hypo>,
}
impl Hypothesis {
    pub fn get(
        &self,
        s: &knormal::Var,
        args: &Vec<Value>,
        fun: &ir::Fundef,
    ) -> Result<Value, AnalyzerError> {
        match s {
            knormal::Var::OpVar(x, ty) => {
                for (ptr, hypo) in self.body.iter() {
                    match hypo.env.get(x) {
                        Some(y) => {
                            return Ok(y.clone());
                        }
                        None => (),
                    }
                }
                match fun.get_arg_idx(x) {
                    Some(y) => {
                        return Ok(args[y].clone());
                    }
                    None => (),
                }

                Err(AnalyzerError::UnrechablePath)
            }
            knormal::Var::Constant(y) => match y {
                syntax::Const::CBool(i) => Ok(Value::Bool(*i)),
                syntax::Const::CInt(i) => Ok(Value::Int(*i)),
                syntax::Const::CFloat(i) => Ok(Value::Float(i.to_bits())),
                syntax::Const::CUnit => Ok(Value::Unit),
                syntax::Const::CPtr(i) => Ok(Value::Unknown),
            },
            knormal::Var::Ext(x, y) => Ok(Value::Global(x.to_string())),
        }
    }
    pub fn add_env(self, s: Value, ans: Value) -> Self {
        self
    }
}
impl<'a> ir::Inst {
    pub fn get_value (
        &self,
        args: &Vec<Value>,
        h: Hypothesis,
        fun: &ir::Fundef,
        prev: Option<String>,
        ana: &Analyzer<'a>,
        written: HashTrieSet<Value>
    ) -> Result<(Value, Hypothesis), AnalyzerError> {
        // dstがあるものにのみ考える
        match self {
            ir::Inst::BinaryRR {
                opcode,
                dst,
                lhs,
                rhs,
            } => {
                let l_value = h.get(lhs, args, fun)?;
                let r_value = h.get(rhs, args, fun)?;
                Ok(match opcode {
                    ir::OpBinaryRR::Add => match (l_value, r_value) {
                        (Value::Iter(i), Value::Int(j)) => Value::Iter(i + j),
                        (Value::Int(i), Value::Iter(j)) => Value::Iter(i + j),
                        (l_value, r_value) => {
                            Value::Trace_add(Box::new(l_value), Box::new(r_value))
                        }
                    },
                    ir::OpBinaryRR::Sub => match (l_value, r_value) {
                        (Value::Iter(i), Value::Int(j)) => Value::Iter(i - j),
                        _ => Value::Unknown,
                    },
                    _ => Value::Unknown,
                })
            }
            ir::Inst::Load { dst, ptr, idx } => {
                let ptr = h.get(ptr, args, fun)?;
                let idx = h.get(idx, args, fun)?;
                Ok(Value::Elm(Box::new(ptr), Box::new(idx)))
            }
            ir::Inst::Mv { dst, src } => h.get(src, args, fun),
            ir::Inst::Phi(ir::Phi { dst, args }) => match prev {
                None => unreachable!(),
                Some(y) => {
                    for (i, j) in args {
                        if y == j {
                            return h.get(i, args, fun);
                        }
                    }
                    unreachable!()
                }
            },
            // ir::Inst::CallDir { dst, label, ar } => {
            //     let mut new_args = vec![];
            //     let ar = ar.clone().into_iter().try_for_each(|x| {
            //         new_args.push(h.get(x, args, fun)?);
            //         Ok(())
            //     });
            //     ana.call(dst, new_args, h, written);
            // }

            _ => unreachable!(),
        }
    }
    // pub fn update_hypo(&self, args:Vec<Value>,h:Hypothesis, written:HashTrieSet<Value>) -> {
    // }
}
impl<'a> Analyzer<'a> {
    pub fn dfs(
        &self,
        dst: &String,
        bb: &ir::Block,
        args: Vec<Value>,
        h: Hypothesis,
        written: HashTrieSet<Value>,
        prev: &String,
    ) -> Result<Hypothesis,  AnalyzerError> {
        // retがhogeならばVec<Value>には書き込んでない
        // retがhoge以外ならばVec<Value>に書き込んでる
        // 問答無用にValueに書き込んでる

        // blockに対してHypothesisを更新していく
        // branchに対してはHypothesisをマージする
        for i in &bb.inst {
            match i {

            }
        }
        Err(AnalyzerError::UnrechablePath)
    }
    pub fn call(
        &self,
        dst : &String,
        args: Vec<Value>,
        h: Hypothesis,
        written: HashTrieSet<Value>,
    ) -> Result<Hypothesis, AnalyzerError> {
        Ok(h)
    }
}
