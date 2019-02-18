use crate::ir;
use crate::knormal;
use crate::syntax;
use std::collections::{HashMap, VecDeque};
impl ir::Inst {
    pub fn alloc(
        self,
        counter: &mut usize,
        extenv: &mut HashMap<String, usize>,
        extlen: &mut HashMap<String, usize>,
    ) -> Self {
        match self.clone() {
            ir::Inst::BinaryRR {
                opcode,
                dst,
                lhs,
                rhs,
            } => match opcode {
                ir::OpBinaryRR::Array => match lhs {
                    knormal::Var::Constant(syntax::Const::CInt(i)) => {
                        let c = *counter;
                        *counter += i as usize;
                        extenv.insert(dst.0.clone(), c);
                        extlen.insert(dst.0.clone(), i as usize);
                        ir::Inst::ArrayAlloc {
                            src: rhs,
                            dst: dst,
                            len: i,
                            ptr: c,
                        }
                    }
                    _ => self,
                },
                _ => self,
            },
            ir::Inst::CallDir { dst, args, label } if label.0.as_str() == "create_tuple" => {
                let c = *counter;
                *counter += args[0].getimm().unwrap();
                extenv.insert(dst.clone().unwrap().0, c);
                extlen.insert(dst.clone().unwrap().0, args[0].getimm().unwrap());
                ir::Inst::Mv {
                    src: knormal::Var::Constant(syntax::Const::CPtr(c as i32)),
                    dst: dst.unwrap(),
                }
            }
            _ => self,
        }
    }
}
impl ir::Block {
    pub fn alloc(
        self,
        init: &mut usize,
        extenv: &mut HashMap<String, usize>,
        extlen: &mut HashMap<String, usize>,
    ) -> Self {
        let inst: VecDeque<_> = self
            .inst
            .into_iter()
            .map(|x| x.alloc(init, extenv, extlen))
            .collect();
        ir::Block { inst: inst, ..self }
    }
}
impl ir::Fundef {
    pub fn alloc(
        self,
        init: &mut usize,
        extenv: &mut HashMap<String, usize>,
        extlen: &mut HashMap<String, usize>,
    ) -> Self {
        let blocks: Vec<_> = self
            .blocks
            .into_iter()
            .map(|x| x.alloc(init, extenv, extlen))
            .collect();
        ir::Fundef {
            blocks: blocks,
            ..self
        }
    }
}
