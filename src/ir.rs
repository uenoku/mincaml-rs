use crate::closure;
use crate::closure::CExpr;
use crate::knormal;
use crate::syntax;

use std::collections::HashSet;
use std::collections::VecDeque;

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
    dst: (String, usize),
}
type Var = knormal::Var;

#[derive(Debug, Clone)]
pub enum OpBinaryRR {
    // register,register
    Add,
    Sub,
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
    Phi {
        dst: Name,
        args: Vec<(Var, Label)>,
    },
}

impl Inst {
    fn dest(&self) -> Option<Name> {
        match self.clone() {
            Inst::Phi { dst, args } => Some(dst),
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
            Inst::Phi { dst, args } => args.iter().map(|(x, y)| x).cloned().collect(),
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

pub fn g(
    e: closure::CExpr,
    label: &mut String,
    block: &mut VecDeque<Inst>,
    blocks: &mut Vec<Block>,
    dst: Option<(String, usize)>,
) {
    let e = match e {
        CExpr::COp(opcode, operand) => {
            let inst = match opcode {
                syntax::Op::Add => Inst::BinaryRR {
                    dst: dst.unwrap(),
                    opcode: OpBinaryRR::Add,
                    lhs: operand[0].clone(),
                    rhs: operand[1].clone(),
                },
                _ => unreachable!(),
            };
            block.push_back(inst);
        }
        CExpr::CLet((x, y), u, v) => {
            g(*u, label, block, blocks, Some((x, y)));
            g(*v, label, block, blocks, dst);
        }
        CExpr::CAppDir(label, args) => block.push_back(Inst::CallDir { dst, label, args }),
        _ => unreachable!(),
    };
}
