use crate::ir;
use crate::knormal;
use crate::syntax;
use crate::to_loop;
use crate::ty;
use std::collections::{HashMap, VecDeque};
pub static SUBNUM: usize = 4;
pub static CHILD_ARG: i32 = 124000;
pub static CORE_NUM: usize = SUBNUM + 1;

pub fn f(
    p: Vec<ir::Fundef>,
    extenv: &mut HashMap<String, usize>,
    tyenv: &mut HashMap<usize, ty::Type>,
) -> (Vec<ir::Fundef>, Vec<Vec<ir::Fundef>>) {
    let p: Vec<_> = p.into_iter().map(|x| x.replace_self_rec_block()).collect();
    let mut tmp = vec![];
    let mut tmp2 = vec![vec![]; CORE_NUM];
    for i in &p {
        let x = i.get_loop_idx();
        //       let hoge = i.only_iter_is_to_loop();
        //        let t = i.collect_wrinfo(&p);
        if x.is_some() {
            info!("{}", i.name.0);
            match i.is_loop_dependent(&p) {
                Some(y) => {
                    let mut accs: Vec<_> = y
                        .into_iter()
                        .map(|(x, y)| (x.clone(), y, *extenv.get(&x).unwrap() + y))
                        .collect();
                    let (main, mainf, childs) = i.pararellize(accs, tyenv);

                    info!("{:?} {:?}", main, childs);
                    tmp.push(main);
                    tmp.push(mainf);
                    for j in 0..SUBNUM {
                        tmp2[j].push(childs[j].clone());
                    }
                }
                None => tmp.push(i.clone()),
            }
        } else {
            tmp.push(i.clone())
        }
    }
    (tmp, tmp2)
}
impl ir::Fundef {
    // main用, child用に増える
    // 最初にやるやつは0にぶっこむからレジスタの初期化とかもやるようにする(zero, one, fp ...)
    pub fn pararellize(
        &self,
        acc: Vec<(String, usize, usize)>,
        tyenv: &mut HashMap<usize, ty::Type>,
    ) -> (Self, Self, Vec<Self>) {
        let (iter, offset, cond) = self.get_loop_idx().unwrap();
        // // 各種チェック(TODO)

        // // f1の中からf2を呼ぶ
        let f1 = self.clone();
        let f2 = self.clone();
        let mut childs = vec![];
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
                label: ("forkf".to_string(), 4), // 大丈夫だっけ
                args: vec![knormal::Var::Constant(syntax::Const::CInt(i as i32))],
            });
        }
        let mut main_f = f2.name.clone().0;
        main_f.push_str(".c");
        new_inst.push_back(ir::Inst::CallDir {
            dst: None,
            label: (main_f.clone(), ty::VOIDFUNC),
            args: vec![],
        });
        // for i in 0..SUBNUM {
        //     new_inst.push_back(ir::Inst::CallDir {
        //         dst: None,
        //         label: ("joinf".to_string(), 2), // 大丈夫だっけ
        //         args: vec![knormal::Var::Constant(syntax::Const::CInt(i as i32))],
        //     });
        // }
        // for i in 0..SUBNUM {
        //     for j in &acc {
        //         new_inst.push_back(ir::Inst::CallDir {
        //             dst: None,
        //             label: ("fetch_and_acc".to_string(), 3), // 大丈夫だっけ
        //             args: vec![
        //                 knormal::Var::Constant(syntax::Const::CInt(i as i32)),
        //                 knormal::Var::Constant(syntax::Const::CPtr(j.2 as i32)),
        //             ],
        //         });
        //     }
        // }

        for i in 0..SUBNUM {
            new_inst.push_back(ir::Inst::CallDir {
                dst: None,
                label: ("joinf".to_string(), 2), // 大丈夫だっけ
                args: vec![knormal::Var::Constant(syntax::Const::CInt(i as i32))],
            });
            for j in &acc {
                new_inst.push_back(ir::Inst::CallDir {
                    dst: None,
                    label: ("fetch_and_acc".to_string(), 3), // 大丈夫だっけ
                    args: vec![
                        knormal::Var::Constant(syntax::Const::CInt(i as i32)),
                        knormal::Var::Constant(syntax::Const::CPtr(j.2 as i32)),
                    ],
                });
            }
        }
        let mut iter_arg = iter.clone();
        iter_arg.push_str(".toloop");
        for coreid in 0..(SUBNUM + 1) {
            let mut f2 = f2.clone();
            let mut entry_inst = f2.blocks[0].inst.clone();
            // childはaccの初期化
            if coreid != SUBNUM {
                for j in &acc {
                    entry_inst.push_back(ir::Inst::Store {
                        src: knormal::Var::Constant(syntax::Const::CFloat(0.0f32)),
                        ptr: knormal::Var::Constant(syntax::Const::CPtr(j.2 as i32)),
                        idx: knormal::Var::Constant(syntax::Const::CInt(0i32)),
                    });
                }
            }

            //　引数をロードする
            for (idx, i) in f2.args.clone().into_iter().enumerate() {
                if i.0 != iter_arg {
                    entry_inst.push_back(ir::Inst::Load {
                        dst: i,
                        ptr: knormal::Var::Constant(syntax::Const::CPtr(CHILD_ARG + idx as i32)),
                        idx: knormal::Var::Constant(syntax::Const::CInt(0)),
                    });
                } else {
                    entry_inst.push_back(ir::Inst::Load {
                        dst: (String::from("iter.temporary"), i.1.clone()),
                        ptr: knormal::Var::Constant(syntax::Const::CPtr(CHILD_ARG + idx as i32)),
                        idx: knormal::Var::Constant(syntax::Const::CInt(0)),
                    });
                    entry_inst.push_back(ir::Inst::BinaryRR {
                        opcode: ir::OpBinaryRR::Add,
                        dst: i.clone(),
                        lhs: knormal::Var::OpVar(String::from("iter.temporary"), i.1.clone()),
                        rhs: knormal::Var::Constant(syntax::Const::CInt(offset * coreid as i32)),
                    });
                }
            }
            let pair = self.get_idx_pair(&iter).unwrap();
            for (idx1, block) in f2.clone().blocks.iter().enumerate() {
                for (idx2, y) in block.inst.iter().enumerate() {
                    match y.dest() {
                        // 実装ヤバスギ
                        Some((s, _)) if *s == pair => {
                            f2.blocks[idx1].inst.push_front(ir::Inst::BinaryRR {
                                opcode: ir::OpBinaryRR::Add,
                                dst: (pair.clone(), ty::INT),
                                lhs: knormal::Var::OpVar(iter.clone(), ty::INT),
                                rhs: knormal::Var::Constant(syntax::Const::CInt(
                                    offset * CORE_NUM as i32,
                                )),
                            });
                            f2.blocks[idx1].inst.swap(0, idx2 + 1);
                            f2.blocks[idx1].inst.pop_front();
                        }
                        _ => (),
                    }
                }
            }
            // これはphiになってるはず
            let entry_block = ir::Block {
                inst: entry_inst,
                ..f2.blocks[0].clone()
            };
            let mut blocks = f2.blocks;
            blocks[0] = entry_block;
            // let mut n = f2.name.0.clone();
            // n.push_str("main");
            // if coreid != SUBNUM {
            //     n.push_str(&coreid.to_string());
            // }
            childs.push(ir::Fundef {
                name: (
                    if coreid == SUBNUM {
                        main_f.clone()
                    } else {
                        "main".to_string()
                    },
                    ty::VOIDFUNC,
                ),
                args: vec![],
                blocks: blocks,
                ..f2
            });
        }
        (
            ir::Fundef {
                blocks: vec![ir::Block {
                    label: "entry".to_string(),
                    last: ir::ControlFlow::Return(None),
                    phis: vec![],
                    inst: new_inst,
                }],
                ..f1
            },
            childs.last().unwrap().clone(),
            childs,
        )
    }
}
