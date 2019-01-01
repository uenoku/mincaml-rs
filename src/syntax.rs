use crate::ty::Type;
use crate::ty::Type::*;
#[derive(Debug, Clone)]
pub enum Cmp {
    EQ,
    NE,
    LT,
    GT,
    LE,
    GE,
}
#[derive(Debug, Clone)]
pub enum Op {
    Add,
    Mul,
    Sub,
    Div,
    Cond(Cmp),
    FAdd,
    FSub,
    FMul,
    FDiv,
    Neg,
    FNeg,
    Not,
    Load,
    Store,
    Array, // for polymorhpsim Array(Expr,Expr)
}
pub fn infer_op(op: &Op) -> Type {
    match op {
        Op::Add => TyInt,
        Op::Mul => TyInt,
        Op::Sub => TyInt,
        Op::Div => TyInt,
        Op::Cond(_) => TyBool,
        Op::FAdd => TyFloat,
        Op::FMul => TyFloat,
        Op::FSub => TyFloat,
        Op::FDiv => TyFloat,
        Op::Neg => TyInt,
        Op::FNeg => TyFloat,
        Op::Not => TyBool,
        _ => unreachable!(),
    }
}

#[derive(Debug, Clone)]
pub enum Const {
    CInt(i32),
    CFloat(f32),
    CBool(bool),
    CUnit,
    // CExtArray(String),
}
pub type Var = String;
pub fn getvar(name: String) -> Var {
    name
}

#[derive(Debug, Clone)]
pub struct Fundef {
    pub name: (Var, usize),
    pub args: Vec<(Var, usize)>,
    pub body: BE,
}

type BE = Box<Expr>;

#[derive(Debug, Clone)]
pub enum Expr {
    EConst(Const),
    EVar(Var),
    EOp(Op, Vec<BE>),
    EIf(BE, BE, BE),
    ELet((Var, usize), BE, BE),
    ELetTuple(Vec<(Var, usize)>, BE, BE),
    ELetRec(Fundef, BE),
    EApp(BE, Vec<BE>),
    ETuple(Vec<BE>),
}
static mut COUNTER: usize = 0;
pub fn genvar() -> usize {
    let tmp = unsafe { COUNTER };
    unsafe {
        COUNTER += 1;
    }
    tmp
}
pub fn genname() -> String {
    let v = genvar();
    v.to_string()
}
pub fn newvar() -> Var {
    getvar(genname())
}

pub fn infer_const(x: &Const) -> Type {
    match *x {
        Const::CInt(_) => Type::TyInt,
        Const::CFloat(_) => Type::TyFloat,
        Const::CBool(_) => Type::TyBool,
        Const::CUnit => Type::TyUnit,
    }
}
