use crate::ir;
use crate::knormal;
use crate::syntax;
use crate::to_loop;
use crate::ty;
use std::collections::VecDeque;
static SUBNUM: usize = 3;
static CHILD_ARG: i32 = 1090000;

impl ir::Fundef {
    // main用, child用に増える
    // 最初にやるやつは0にぶっこむからレジスタの初期化とかもやるようにする(zero, one, fp ...)
    pub fn pararellize(&self, acc: Vec<(String, usize, usize)>) -> (Self, Vec<Self>) {
        // let (iter, offset, cond) = self.get_loop_idx().unwrap();
        // // 各種チェック(TODO)

        // // f1の中からf2を呼ぶ
        let f1 = self.clone();
        let f2 = self.clone();
        // let f_name =

        let mut new_inst = VecDeque::new();
        for (idx, i) in self.args.iter().enumerate() {
            let (a, b) = i.clone();
            new_inst.push_back(ir::Inst::Store {
                src: knormal::Var::OpVar(a, b),
                ptr: knormal::Var::Constant(syntax::Const::CPtr(CHILD_ARG + idx as i32)),
                idx: knormal::Var::Constant(syntax::Const::CInt(0)),
            });
        }
        for i in 0..SUBNUM {
            new_inst.push_back(ir::Inst::CallDir {
                dst: None,
                label: ("join".to_string(), 0), // 大丈夫だっけa
                args: vec![knormal::Var::Constant(syntax::Const::CInt(i as i32))],
            });
        }
        new_inst.push_back(ir::Inst::CallDir {
            dst: None,
            label: f2.name,
            args: vec![],
        });
        for i in 0..SUBNUM {
            for j in &acc {
                new_inst.push_back(ir::Inst::CallDir {
                    dst: None,
                    label: ("fetch_and_acc".to_string(), 0), // 大丈夫だっけa
                    args: vec![
                        knormal::Var::Constant(syntax::Const::CInt(i as i32)),
                        knormal::Var::Constant(syntax::Const::CPtr(j.2 as i32)),
                    ],
                });
            }

            // for j in &acc {
            //     let name = to_loop::rename(&"acc_tmp".to_string(), syntax::genvar());
            //     new_inst.push_back(ir::Inst::CallDir {
            //         dst: Some((name, ty::FLOAT)),
            //         label: ("fetch_float".to_string(), 0), // 大丈夫だっけa
            //         args: vec![
            //             knormal::Var::Constant(syntax::Const::CInt(i as i32)),
            //             knormal::Var::Constant(syntax::Const::CInt(j.2 as i32)),
            //         ],
            //     });
            // }
        }

        (f1, vec![])
    }
}
