use crate::ty::Type;
use crate::ty::Type::*;
#[derive(Debug, Clone)]
pub enum Op {
    Add,
    Mul,
    Sub,
    Div,
    EQ,
    NE,
    LE,
    LT,
    GE,
    GT,
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
pub fn infer_op(op: Op) -> Type {
    match op {
        Add => TyInt,
        Mul => TyInt,
        Sub => TyInt,
        Div => TyInt,
        EQ => TyBool,
        NE => TyBool,
        LE => TyBool,
        LT => TyBool,
        GE => TyBool,
        GT => TyBool,
        FAdd => TyFloat,
        FMul => TyFloat,
        FSub => TyFloat,
        FDiv => TyFloat,
        Neg => TyInt,
        FNeg => TyFloat,
        Not => TyBool,
        _ => unreachable!(),
    }
}

#[derive(Debug, Clone)]
pub enum Const {
    CInt(i32),
    CFloat(f32),
    CBool(bool),
    CUnit,
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

pub fn infer_const(x: Const) -> Type {
    match x {
        Const::CInt(_) => Type::TyInt,
        Const::CFloat(_) => Type::TyFloat,
        Const::CBool(_) => Type::TyBool,
        Const::CUnit => Type::TyUnit,
    }
}
