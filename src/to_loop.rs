use crate::ir;
use crate::knormal;
static ENTRY_INDEX: usize = 1;

impl ir::Block {
    pub fn get_instruction(&self, x: &String) -> Option<ir::Inst> {
        for i in &self.inst {
            match i.dest() {
                Some((y, z)) if *x == y => return Some(i.clone()),
                _ => (),
            }
        }
        None
    }
    pub fn get_last_instruction(&self) -> Option<ir::Inst> {
        self.inst.iter().cloned().last()
    }
    // 飛ぶ先の任意のブロックで命令が行われずlastがJump|Returnである
    pub fn is_tail_block(&self, fun: &ir::Fundef) -> bool {
        match &self.last {
            ir::ControlFlow::Jump(y) if self.inst.is_empty() => {
                fun.get_block_ref(y).is_tail_block(fun)
            }
            ir::ControlFlow::Return(_) if self.inst.is_empty() => true,
            _ => false,
        }
    }
    pub fn get_self_rec_call(
        &self,
        current_fun_name: &String,
        fun: &ir::Fundef,
    ) -> Option<ir::Inst> {
        match &self.last {
            ir::ControlFlow::Return(Some(knormal::Var::OpVar(x, y))) => {
                let inst = self.get_instruction(&x);
                match inst {
                    None => None,
                    Some(y) => match &y {
                        ir::Inst::CallDir { dst, label, args } if *label.0 == *current_fun_name => {
                            info!("{}", label.0);
                            Some(y)
                        }
                        _ => None,
                    },
                }
            }

            ir::ControlFlow::Return(None) => {
                let inst = self.get_last_instruction();
                info!("last {:?}", inst);
                match inst {
                    None => None,
                    Some(y) => match &y {
                        ir::Inst::CallDir { dst, label, args } if *label.0 == *current_fun_name => {
                            info!("{}", label.0);
                            Some(y)
                        }
                        _ => None,
                    },
                }
            }
            ir::ControlFlow::Jump(y) if fun.get_block_ref(y).is_tail_block(fun) => {
                let inst = self.get_last_instruction();
                info!("last {:?}", inst);
                match inst {
                    None => None,
                    Some(y) => match &y {
                        ir::Inst::CallDir { dst, label, args } if *label.0 == *current_fun_name => {
                            info!("{}", label.0);
                            Some(y)
                        }
                        _ => None,
                    },
                }
            }
            _ => None,
        }
    }

    pub fn is_self_rec_call(&self, current_fun_name: &String, fun: &ir::Fundef) -> bool {
        self.get_self_rec_call(current_fun_name, fun).is_some()
    }
}
type Blocks = Vec<ir::Block>;
impl ir::Fundef {
    pub fn get_instruction(&self, name: &String) -> ir::Inst {
        for i in &self.blocks {
            match i.get_instruction(name) {
                Some(y) => return y,
                None => (),
            }
        }
        unreachable!()
    }
    pub fn get_block_ref(&self, name: &String) -> &ir::Block {
        for i in &self.blocks {
            if *name == *i.label {
                return i;
            }
        }
        unreachable!()
    }
    pub fn is_break_cond(&self, s: &String) -> bool {
        false
    }
    // pub fn get_idx(&self) -> (ir::Name,i32) {
    //     // entry'は1!
    //     let phis = &self.blocks[ENTRY_INDEX].phis;
    //     for i in phis {
    //     }
    // }
    pub fn replace_self_rec_block(self) -> Self {
        let mut new_blocks = vec![];
        let mut phis = vec![];
        for mut i in self.blocks.clone() {
            info!("{}", i.label);
            match i.get_self_rec_call(&self.name.0, &self) {
                None => new_blocks.push(i),
                Some(y) => match y {
                    ir::Inst::CallDir { dst, label, args } => {
                        info!("found loop {} ", &self.name.0);
                        phis.push((i.label.clone(), args));
                        i.inst.pop_back();
                        new_blocks.push({
                            ir::Block {
                                last: ir::ControlFlow::Jump(String::from("entry'")),
                                ..i.clone()
                            }
                        });
                    }
                    _ => unreachable!(),
                },
            }
        }
        if phis.is_empty() {
            ir::Fundef {
                blocks: new_blocks,
                ..self
            }
        } else {
            let mut entry = &mut new_blocks[ENTRY_INDEX];
            let newargs: Vec<_> = self
                .args
                .iter()
                .map(|(x, y)| {
                    let mut tmp = x.clone();
                    tmp.push_str(".toloop");
                    (tmp, *y)
                })
                .collect();
            let mut inst = &mut entry.inst;
            let mut phis_inst = vec![vec![]; self.args.len()];
            for (label, args) in phis {
                for (idx, j) in args.iter().enumerate() {
                    phis_inst[idx].push((j.clone(), label.clone()));
                }
            }
            for i in 0..self.args.len() {
                let arg = &self.args[i];
                let narg = &newargs[i];
                phis_inst[i].push((
                    knormal::Var::OpVar(narg.clone().0, narg.clone().1),
                    "entry".to_string(),
                ));
                inst.push_front(ir::Inst::Phi(ir::Phi {
                    dst: arg.clone(),
                    args: phis_inst[i].clone(),
                }));
                entry.phis.push(ir::Phi {
                    dst: arg.clone(),
                    args: phis_inst[i].clone(),
                })
            }
            ir::Fundef {
                blocks: new_blocks,
                args: newargs,
                ..self
            }
        }
    }
}
