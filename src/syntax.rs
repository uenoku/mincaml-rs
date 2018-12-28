use crate::ty::Type;
#[derive(Debug)]
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
    Put(usize),
    Get(usize),
}

#[derive(Debug)]
pub enum Const {
    CInt(i32),
    CFloat(f32),
    CBool(bool),
    CUnit,
}

#[derive(Debug)]
pub struct Var {
    pub name: String,
    pub ty: usize,
}
pub fn getvar(name: String) -> Var {
    Var {
        name: name,
        ty: genvar(),
    }
}

#[derive(Debug)]
pub struct Fundef {
    pub name: Var,
    pub args: Vec<Var>,
    pub body: BE,
}

type BE = Box<Expr>;

#[derive(Debug)]
pub enum Expr {
    EConst(Const),
    EVar(Var),
    EOp(Op, Vec<BE>),
    EIf(BE, BE, BE),
    ELet(Var, BE, BE),
    ELetTuple(Vec<Var>, BE, BE),
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
