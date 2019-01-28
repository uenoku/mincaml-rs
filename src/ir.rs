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
    args: Vec<(Var, Label)>,
    dst: Name,
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
type Name = (String, usize);

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
    Load {
        dst: Name,
        ptr: Var,
        idx: Var,
    },
    CallDir {
        dst: Option<Name>,
        label: String,
        args: Vec<Var>,
    },
    CallCls {
        dst: Option<Name>,
        label: String,
        args: Vec<Var>,
    },
    Phi(Phi),
}

impl Inst {
    fn dest(&self) -> Option<Name> {
        match self.clone() {
            Inst::Phi(Phi { dst, args }) => Some(dst),
            Inst::CallCls { dst, label, args } => dst,
            Inst::CallDir { dst, label, args } => dst,
            Inst::Load { dst, ptr, idx } => Some(dst),
            Inst::Unary { opcode, dst, src } => Some(dst),
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
pub struct Fundef {
    pub name: (String, usize),
    pub args: Vec<(String, usize)>,
    pub formal_fv: Vec<(String, usize)>,
    pub entry: Label,
    pub blocks: Vec<Block>,
}
impl Block {}

pub fn cls_to_ir(f:closure::Fundef, tyenv:&mut HashMap<usize,ty::Type>){
    let e = f.body;
    let mut label = String::from("entry");
    let (name,ty) = f.name;
    let ty = 
        match *tyenv.get(&ty).unwrap() {
            ty::Type::TyFun(x,y) => 
            match *y {
                ty::Type::TyUnit => None,
                _ => 
                Some(*y.clone()),
            },
            _ => unreachable!(),
        };
    let name = syntax::genname();
    let mut blocks = Vec::new();
    g(*e, tyenv, &mut label, &mut VecDeque::new(), &mut blocks, &mut Vec::new(), dst);
}
pub fn f( functions: Vec<closure::Fundef>){
}
pub fn g(
    e: closure::CExpr,
    tyenv: &mut HashMap<usize, ty::Type>,
    label: &mut String,
    block: &mut VecDeque<Inst>,
    blocks: &mut Vec<Block>,
    phis: &mut Vec<Phi>,
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
            g(*u, tyenv, label, block, blocks, phis, Some((x, y)));
            g(*v, tyenv, label, block, blocks, phis, dst);
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
                    g(*$e, tyenv, label, block, blocks, phis, $dst);
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
            let ty = syntax::genvar();
            tyenv.insert(ty, ty::Type::TyPtr);
            let mut i = 0;
            let n = syntax::genname();
            let ptr = (n.clone(), ty);
            block.push_back(Inst::CallDir {
                dst: Some(ptr),
                label: String::from("create_tuple"),
                args: vec![knormal::Var::Constant(syntax::Const::CInt(
                    elements.len() as i32
                ))],
            });
            for (i, x) in elements.into_iter().enumerate() {
                block.push_back(Inst::Store {
                    src: x,
                    idx: knormal::Var::Constant(syntax::Const::CInt(i as i32)),
                    ptr: knormal::Var::OpVar(n.clone(), ty),
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
            g(*v, tyenv, label, block, blocks, phis, dst);
        }
        CExpr::CAppDir(label, args) => block.push_back(Inst::CallDir { dst, label, args }),
        CExpr::CVar(_) => unimplemented!(),
        CExpr::CAppCls(_, _) => unimplemented!(),
        CExpr::CMakeCls(_, _, _) => unimplemented!(),
    };
}
