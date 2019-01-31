use crate::closure;
use crate::closure::CExpr;
use crate::knormal;
use crate::syntax;
use crate::ty;

use std::collections::{HashMap, HashSet, VecDeque};

type Set<T> = HashSet<T>;

type Label = String;

#[derive(Debug, Clone)]
pub enum ControlFlow {
    Return(Option<Var>),
    Branch(Var, Label, Label),
    Jump(Label),
}
#[derive(Debug, Clone)]
pub struct Phi {
    pub args: Vec<(Var, Label)>,
    pub dst: Name,
}
impl Phi {
    fn alpha(self, alias: &HashMap<String, knormal::Var>) -> Self {
        Phi {
            dst: self.dst,
            args: self
                .args
                .into_iter()
                .map(|(x, y)| (x.alpha(alias), y))
                .collect(),
        }
    }
}
type Var = knormal::Var;

#[derive(Debug, Clone)]
pub enum OpBinaryRR {
    // register,register
    Add,
    Sub,
    Mul,
    Div,
    FDiv,
    FMul,
    FSub,
    FAdd,
    Cond(syntax::Cmp),
    Array,
}

#[derive(Debug, Clone)]
pub enum OpBinaryRI {
    // register, immediate
    ShiftR,
    ShiftL,
}

#[derive(Debug, Clone)]
pub enum OpUnary {
    // register
    Neg,
    Not,
    FNeg,
}
pub type Name = (String, usize);

#[derive(Debug, Clone)]
pub enum Inst {
    BinaryRR {
        opcode: OpBinaryRR,
        dst: Name,
        lhs: Var,
        rhs: Var,
    },
    BinaryRI {
        opcode: OpBinaryRI,
        dst: Name,
        lhs: Var,
        rhs: usize,
    },
    Unary {
        opcode: OpUnary,
        dst: Name,
        src: Var,
    },
    Store {
        ptr: Var,
        idx: Var,
        src: Var,
    },
    // Mv {
    //     src: Var,
    //     dst: Name,
    // },
    Load {
        dst: Name,
        ptr: Var,
        idx: Var,
    },
    CallDir {
        dst: Option<Name>,
        label: Name,
        args: Vec<Var>,
    },
    CallCls {
        dst: Option<Name>,
        label: Name,
        args: Vec<Var>,
    },
    Phi(Phi),
}

impl Inst {
    fn alpha(self, alias: &HashMap<String, knormal::Var>) -> Self {
        match self {
            Inst::Phi(x) => Inst::Phi(x.alpha(alias)),
            Inst::CallCls { dst, label, args } => Inst::CallCls {
                dst,
                label,
                args: args.into_iter().map(|x| x.alpha(alias)).collect(),
            },
            Inst::CallDir { dst, label, args } => Inst::CallCls {
                dst,
                label,
                args: args.into_iter().map(|x| x.alpha(alias)).collect(),
            },
            Inst::Load { dst, ptr, idx } => Inst::Load {
                dst,
                ptr: ptr.alpha(alias),
                idx: idx.alpha(alias),
            },
            Inst::Store { ptr, idx, src } => Inst::Store {
                ptr: ptr.alpha(alias),
                idx: idx.alpha(alias),
                src: src.alpha(alias),
            },
            Inst::Unary { opcode, dst, src } => Inst::Unary {
                opcode,
                dst,
                src: src.alpha(alias),
            },
            Inst::BinaryRI {
                opcode,
                dst,
                lhs,
                rhs,
            } => Inst::BinaryRI {
                opcode,
                dst,
                lhs: lhs.alpha(alias),
                rhs: rhs,
            },
            Inst::BinaryRR {
                opcode,
                dst,
                lhs,
                rhs,
            } => Inst::BinaryRR {
                opcode,
                dst,
                lhs: lhs.alpha(alias),
                rhs: rhs.alpha(alias),
            },
        }
    }
    fn dest(&self) -> Option<Name> {
        match self.clone() {
            Inst::Phi(Phi { dst, args }) => Some(dst),
            Inst::CallCls { dst, label, args } => dst,
            Inst::CallDir { dst, label, args } => dst,
            Inst::Load { dst, ptr, idx } => Some(dst),
            Inst::Unary { opcode, dst, src } => Some(dst),
            //Inst::Mv { dst, src } => Some(dst),
            Inst::BinaryRI {
                opcode,
                dst,
                lhs,
                rhs,
            } => Some(dst),
            Inst::BinaryRR {
                opcode,
                dst,
                lhs,
                rhs,
            } => Some(dst),
            _ => None,
        }
    }
    fn operand(&self) -> Vec<Var> {
        match self.clone() {
            Inst::Phi(Phi { dst, args }) => args.iter().map(|(x, y)| x).cloned().collect(),
            Inst::CallCls { dst, label, args } => args,
            Inst::CallDir { dst, label, args } => args,
            Inst::Load { dst, ptr, idx } => vec![ptr],
            Inst::Store { ptr, idx, src } => vec![ptr, src],
            Inst::Unary { opcode, dst, src } => vec![src],
            //            Inst::Mv { dst, src } => vec![src],
            Inst::BinaryRI {
                opcode,
                dst,
                lhs,
                rhs,
            } => vec![lhs],
            Inst::BinaryRR {
                opcode,
                dst,
                lhs,
                rhs,
            } => vec![rhs],
        }
    }
    fn kill(&self) -> Set<String> {
        let mut ret = Set::new();
        match &self.dest() {
            Some((x, y)) => ret.insert(x.clone()),
            None => false,
        };
        ret
    }
    fn gen(&self) -> Set<String> {
        let mut ret = Set::new();
        for i in &self.operand() {
            let v = i.set();
            ret: HashSet<_> = ret.union(&v).cloned().collect();
        }
        ret
    }
}

#[derive(Clone, Debug)]
pub struct Block {
    pub label: Label,
    pub inst: VecDeque<Inst>,
    pub last: ControlFlow,
    pub phis: Vec<Phi>,
}
impl ControlFlow {
    pub fn alpha(self, alias: &mut HashMap<String, knormal::Var>) -> Self {
        match self {
            ControlFlow::Return(Some(x)) => ControlFlow::Return(Some(x.alpha(alias))),
            ControlFlow::Branch(x, y, z) => ControlFlow::Branch(x.alpha(alias), y, z),
            _ => self,
        }
    }
}
impl Block {
    pub fn alpha(self, alias: &mut HashMap<String, knormal::Var>) -> Self {
        let inst: VecDeque<_> = self.inst.into_iter().map(|x| x.alpha(alias)).collect();
        Block {
            label: self.label,
            inst: inst,
            last: self.last.alpha(alias),
            phis: self.phis.into_iter().map(|x| x.alpha(alias)).collect(),
        }
    }
}
#[derive(Clone, Debug)]
pub struct Fundef {
    pub name: (String, usize),
    pub args: Vec<(String, usize)>,
    pub formal_fv: Vec<(String, usize)>,
    pub entry: Label,
    pub blocks: Vec<Block>,
}

pub fn cls_to_ir(
    f: closure::Fundef,
    tyenv: &mut HashMap<usize, ty::Type>,
    alias: &mut HashMap<String, knormal::Var>,
) -> Fundef {
    let e = f.body;
    let mut label = String::from("entry");
    let (name, ty) = f.name.clone();
    info!("{} {:?}", name, tyenv.get(&ty).unwrap());
    let ty = match tyenv.get(&ty).unwrap() {
        ty::Type::TyFun(x, y) => match **y {
            ty::Type::TyUnit => None,
            _ => Some(*y.clone()),
        },
        _ => unreachable!(),
    };

    let mut blocks = Vec::new();
    let mut inst = VecDeque::new();
    let mut phis = Vec::new();
    match ty {
        Some(x) => {
            let n = syntax::genname();
            let ty = syntax::genvar();
            tyenv.insert(ty, x);
            let ret = (n, ty);
            g(
                *e,
                tyenv,
                &mut label,
                &mut inst,
                &mut blocks,
                &mut phis,
                alias,
                Some(ret.clone()),
            );
            blocks.push(Block {
                label: label,
                inst: inst,
                last: ControlFlow::Return(Some(Var::OpVar(ret))),
                phis: phis,
            });
            let blocks = blocks.into_iter().map(|x| x.alpha(alias)).collect();
            Fundef {
                name: f.name,
                args: f.args,
                formal_fv: f.formal_fv,
                entry: String::from("entry"),
                blocks: blocks,
            }
        }
        None => {
            g(
                *e,
                tyenv,
                &mut label,
                &mut inst,
                &mut blocks,
                &mut phis,
                alias,
                None,
            );
            blocks.push(Block {
                label: label,
                inst: inst,
                last: ControlFlow::Return(None),
                phis: phis,
            });

            let blocks = blocks.into_iter().map(|x| x.alpha(alias)).collect();
            Fundef {
                name: f.name,
                args: f.args,
                formal_fv: f.formal_fv,
                entry: String::from("entry"),
                blocks: blocks,
            }
        }
    }
}
pub fn f(functions: Vec<closure::Fundef>, tyenv: &mut HashMap<usize, ty::Type>) -> Vec<Fundef> {
    let mut alias = HashMap::new();
    let res: Vec<_> = functions
        .into_iter()
        .map(|x| cls_to_ir(x, tyenv, &mut alias))
        .collect();
    res
}
fn new_name(st: &String, ty: ty::Type, tyenv: &mut HashMap<usize, ty::Type>) -> Name {
    let tyvar = syntax::genvar();
    tyenv.insert(tyvar, ty.clone());
    (st.clone(), tyvar)
}
pub fn g(
    e: closure::CExpr,
    tyenv: &mut HashMap<usize, ty::Type>,
    label: &mut String,
    block: &mut VecDeque<Inst>,
    blocks: &mut Vec<Block>,
    phis: &mut Vec<Phi>,
    alias: &mut HashMap<String, knormal::Var>,
    dst: Option<(String, usize)>,
) {
    let e = match e {
        CExpr::COp(opcode, operand) => {
            macro_rules! binrr {
                ($op:expr) => {
                    Inst::BinaryRR {
                        dst: dst.unwrap(),
                        opcode: $op,
                        lhs: operand[0].clone(),
                        rhs: operand[1].clone(),
                    }
                };
            }
            macro_rules! binri {
                ($op:expr) => {
                    Inst::BinaryRR {
                        dst: dst.unwrap(),
                        opcode: $op,
                        lhs: operand[0].clone(),
                        rhs: operand[1].getimm().unwrap(),
                    }
                };
            }
            macro_rules! uni {
                ($op:expr) => {
                    Inst::Unary {
                        dst: dst.unwrap(),
                        opcode: $op,
                        src: operand[0].clone(),
                    }
                };
            }
            let inst = match opcode {
                syntax::Op::Add => binrr!(OpBinaryRR::Add),
                syntax::Op::Sub => binrr!(OpBinaryRR::Sub),
                syntax::Op::Mul => binrr!(OpBinaryRR::Mul),
                syntax::Op::Div => binrr!(OpBinaryRR::Div),
                syntax::Op::FAdd => binrr!(OpBinaryRR::FAdd),
                syntax::Op::FSub => binrr!(OpBinaryRR::FSub),
                syntax::Op::FMul => binrr!(OpBinaryRR::FMul),
                syntax::Op::FDiv => binrr!(OpBinaryRR::FDiv),
                syntax::Op::Cond(u) => binrr!(OpBinaryRR::Cond(u)),
                syntax::Op::Array => binrr!(OpBinaryRR::Array),
                syntax::Op::FNeg => uni!(OpUnary::FNeg),
                syntax::Op::Neg => uni!(OpUnary::Neg),
                syntax::Op::Not => uni!(OpUnary::Not),
                syntax::Op::Load => Inst::Load {
                    dst: dst.unwrap(),
                    ptr: operand[0].clone(),
                    idx: operand[1].clone(),
                },
                syntax::Op::Store => Inst::Store {
                    ptr: operand[0].clone(),
                    idx: operand[1].clone(),
                    src: operand[2].clone(),
                },
            };
            block.push_back(inst);
        }
        CExpr::CLet((x, y), u, v) => {
            g(*u, tyenv, label, block, blocks, phis, alias, Some((x, y)));
            g(*v, tyenv, label, block, blocks, phis, alias, dst);
        }
        CExpr::CIf(cond, x, y, t, f) => {
            let ty = syntax::genvar();
            tyenv.insert(ty, ty::Type::TyBool);
            let name = (syntax::genname(), ty);
            let b = Inst::BinaryRR {
                opcode: OpBinaryRR::Cond(cond),
                dst: name.clone(),
                lhs: x,
                rhs: y,
            };
            block.push_back(b);
            let mut t_label = label.clone();
            t_label.push_str(".true");
            let mut f_label = label.clone();
            f_label.push_str(".false");
            let mut cont_label = label.clone();
            cont_label.push_str(".cont");
            blocks.push(Block {
                label: label.clone(),
                inst: block.clone(),
                last: ControlFlow::Branch(Var::OpVar(name), t_label.clone(), f_label.clone()),
                phis: phis.clone(),
            });
            macro_rules! sub {
                ($label:expr,$e:expr, $dst:expr) => {{
                    block.clear();
                    phis.clear();
                    label.clone_from(&$label);
                    g(*$e, tyenv, label, block, blocks, phis, alias, $dst);
                    blocks.push(Block {
                        label: label.clone(),
                        inst: block.clone(),
                        last: ControlFlow::Jump(cont_label.clone()),
                        phis: phis.clone(),
                    });
                }};
            }
            if dst.is_none() {
                // phiは生えない
                sub!(t_label, t, None);
                sub!(f_label, f, None);
                block.clear();
                phis.clear();
                label.clone_from(&cont_label);
            } else {
                let (name, ty) = dst.unwrap();
                let tv = syntax::genname();
                let fv = syntax::genname();
                sub!(t_label, t, Some((tv.clone(), ty)));
                sub!(f_label, f, Some((fv.clone(), ty)));
                block.clear();
                phis.clear();
                label.clone_from(&cont_label);
                let phi = Phi {
                    dst: (name, ty),
                    args: vec![
                        (knormal::Var::OpVar(tv, ty), t_label),
                        (knormal::Var::OpVar(fv, ty), f_label),
                    ],
                };
                phis.push(phi.clone());
                block.push_back(Inst::Phi(phi));
            }
        }

        CExpr::CTuple(elements) => {
            let ptr = new_name(&syntax::genname(), ty::Type::TyPtr, tyenv);
            let f = new_name(
                &String::from("create_tuple"),
                ty::Type::TyFun(vec![ty::Type::TyInt], Box::new(ty::Type::TyPtr)),
                tyenv,
            );
            block.push_back(Inst::CallDir {
                dst: Some(ptr.clone()),
                label: f,
                args: vec![knormal::Var::Constant(syntax::Const::CInt(
                    elements.len() as i32
                ))],
            });
            for (i, x) in elements.into_iter().enumerate() {
                block.push_back(Inst::Store {
                    src: x,
                    idx: knormal::Var::Constant(syntax::Const::CInt(i as i32)),
                    ptr: knormal::Var::OpVar(ptr.0.clone(), ptr.1),
                });
            }
        }
        CExpr::CLetTuple(binds, u, v) => {
            for (i, (x, y)) in binds.into_iter().enumerate() {
                let inst = Inst::Load {
                    dst: (x, y),
                    ptr: u.clone(),
                    idx: knormal::Var::Constant(syntax::Const::CInt(i as i32)),
                };
                block.push_back(inst);
            }
            g(*v, tyenv, label, block, blocks, phis, alias, dst);
        }
        CExpr::CAppDir(label, args) => block.push_back(Inst::CallDir { dst, label, args }),
        CExpr::CAppCls(label, args) => block.push_back(Inst::CallCls { dst, label, args }),

        CExpr::CVar(y) => {
            if dst.is_some() {
                alias.insert(dst.unwrap().0, y);
                // dstの全ての出現をyにする
                // block.push_back(Inst::Mv {
                //     src: y,
                //     dst: dst.unwrap(),
                // })
            }
        }
        // CExpr::CAppCls(_, _) => unimplemented!(),
        CExpr::CMakeCls(_, _, _) => unimplemented!(),
    };
}
