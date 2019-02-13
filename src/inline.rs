use crate::ir;
use crate::ir::Name;
use crate::knormal;
use crate::to_loop;
use crate::to_loop::{rename, Functions};
use std::collections::{HashMap, VecDeque};
impl knormal::Var {
    pub fn suffix(&self, cnt: usize) -> Self {
        match self {
            knormal::Var::OpVar(x, ty) => knormal::Var::OpVar(to_loop::rename(x, cnt), *ty),
            _ => self.clone(),
        }
    }
}

pub fn suffix_name_opt(z: &Option<Name>, num: usize) -> Option<Name> {
    match z {
        Some((x, y)) => Some((rename(x, num), *y)),
        None => None,
    }
}
pub fn suffix_name((x, y): &Name, num: usize) -> Name {
    (rename(x, num), *y)
}
impl ir::Inst {
    pub fn suffix(self, num: usize) -> Self {
        match &self {
            ir::Inst::BinaryRR {
                opcode,
                dst,
                lhs,
                rhs,
            } => ir::Inst::BinaryRR {
                opcode: opcode.clone(),
                dst: suffix_name(dst, num),
                lhs: lhs.suffix(num),
                rhs: rhs.suffix(num),
            },
            ir::Inst::BinaryRI {
                opcode,
                dst,
                lhs,
                rhs,
            } => ir::Inst::BinaryRI {
                opcode: opcode.clone(),
                dst: suffix_name(dst, num),
                lhs: lhs.suffix(num),
                rhs: *rhs,
            },
            ir::Inst::Unary { opcode, dst, src } => ir::Inst::Unary {
                opcode: opcode.clone(),
                dst: suffix_name(dst, num),
                src: src.suffix(num),
            },
            ir::Inst::Store { ptr, idx, src } => ir::Inst::Store {
                ptr: ptr.suffix(num),
                idx: idx.suffix(num),
                src: src.suffix(num),
            },
            ir::Inst::Mv { src, dst } => ir::Inst::Mv {
                src: src.suffix(num),
                dst: suffix_name(dst, num),
            },
            ir::Inst::Load { dst, ptr, idx } => ir::Inst::Load {
                dst: suffix_name(dst, num),
                idx: idx.suffix(num),
                ptr: ptr.suffix(num),
            },
            _ => self,
        }
    }
}
// pub enum PtrVar {
//     Global(String, Vec<to_loop::Index>),
//     Arg(usize),
//     Local(String, Vec<to_loop::Index>),
// }
impl ir::Fundef {
    // pub fn memo(&self, mem:&mut HashMap<String, PtrVar>, ptr:&knormal::Var , iter:&String) -> PtrVar {
    //     match ptr {
    //         knormal::Var::OpVar(x, y) if mem.contains_key(x)=> {
    //             mem.get(x).clone().unwrap()
    //         }
    //         knormal::Var::OpVar(x, y) => {
    //             let inst = self.get_instruction(x);
    //             match inst {
    //                 Some(Inst::Load) => y.global_load(self, iter),
    //                 None =>
    //                 {
    //                     let idx = self.get_arg_idx(x);
    //                     match idx {
    //                         Some(idx) => Some((Ptr::Arg(idx), vec![])),
    //                         None => Unknown,
    //                     }
    //                 }
    //             }
    //         }
    //         knormal::Var::Ext(x, y) => Some((Ptr::Global(x.to_string()), vec![])),

    //     }
    // }
    pub fn inline_all(self, functions: &Functions) -> Self {
        let mut blocks = vec![];
        self.inline_all_sub(
            functions,
            &mut blocks,
            &mut VecDeque::new(),
            &mut "dummy".to_string(),
            &mut 0,
            None,
            None,
        );
        ir::Fundef {
            name: self.name,
            args: self
                .args
                .into_iter()
                .map(|(x, y)| (rename(&x, 0), y))
                .collect(),
            formal_fv: vec![],
            entry: "entry".to_string(),
            blocks: blocks,
        }
    }
    pub fn inline_all_sub(
        &self,
        functions: &Functions,
        blocks: &mut Vec<ir::Block>,
        inst: &mut VecDeque<ir::Inst>,
        current_label: &mut String,
        cnt: &mut usize,
        cont: Option<String>,
        dst: Option<(String, usize)>,
    ) {
        let num = cnt.clone();

        for block in &self.blocks {
            let new_label = to_loop::rename(&block.label, num);
            current_label.clone_from(&new_label);
            for i in &block.inst {
                match i {
                    ir::Inst::CallDir { dst, label, args } if *label.0 == self.name.0 => {
                        inst.push_back(ir::Inst::CallDir {
                            dst: (match dst {
                                Some((x, y)) => Some((rename(&x, num), *y)),
                                None => None,
                            }),
                            label: label.clone(),
                            args: args.into_iter().map(|x| x.suffix(num)).collect(),
                        });
                    }
                    ir::Inst::CallDir { dst, label, args }
                        if to_loop::get_function_ref_opt(functions, &label.0).is_some()
                            && to_loop::have_some_wr(&label.0, functions) =>
                    {
                        let f = to_loop::get_function_ref_opt(functions, &label.0).unwrap();
                        let new_args: Vec<_> = args.into_iter().map(|x| x.suffix(num)).collect();
                        let mut unique = "entry".to_string();
                        *cnt += 1;
                        unique.push_str(".");
                        unique.push_str(&cnt.to_string());
                        blocks.push(ir::Block {
                            label: current_label.to_string(),
                            inst: inst.clone(),
                            last: ir::ControlFlow::Jump(unique),
                            phis: block.phis.clone(),
                        });
                        inst.clear();
                        let mut la = current_label.clone();
                        la.push_str(".cont");
                        let mut alias = HashMap::new();
                        for (idx, (i, j)) in f.args.iter().enumerate() {
                            alias.insert(i.clone(), new_args[idx].clone());
                        }
                        let f_new = f.clone().subst(&alias);
                        f_new.inline_all_sub(
                            functions,
                            blocks,
                            inst,
                            current_label,
                            cnt,
                            Some(la.clone()),
                            suffix_name_opt(dst, num),
                        );
                        current_label.clone_from(&la);
                    }
                    x => {
                        inst.push_back(x.clone().suffix(num));
                    }
                }
            }
            let last = match &block.last {
                ir::ControlFlow::Branch(x, y, z) => {
                    ir::ControlFlow::Branch(x.suffix(num), rename(y, num), rename(z, num))
                }
                ir::ControlFlow::Jump(z) => ir::ControlFlow::Jump(rename(z, num)),
                ir::ControlFlow::Return(Some(z)) => match &dst {
                    Some(x) => {
                        inst.push_back(ir::Inst::Mv {
                            src: z.clone(),
                            dst: x.clone(),
                        });
                        ir::ControlFlow::Jump(cont.clone().unwrap())
                    }
                    None => ir::ControlFlow::Return(Some(z.clone())),
                },
                ir::ControlFlow::Return(None) => match &dst {
                    Some(x) => ir::ControlFlow::Jump(cont.clone().unwrap()),
                    None => ir::ControlFlow::Return(None),
                },
            };
            blocks.push(ir::Block {
                label: current_label.to_string(),
                inst: inst.clone(),
                last: last,
                phis: block.phis.clone(),
            });
            inst.clear();
        }
    }
}
