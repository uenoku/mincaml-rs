#[derive(Debug)]
pub enum Type {
    TyUnit,
    TyBool,
    TyInt,
    TyFloat,
    TyFun(Vec<Box<Type>>, Box<Type>),
    TyTuple(Vec<Box<Type>>),
    TyArray(Box<Type>),
    TyVar(usize),
}

#[derive(Debug)]
pub enum Op {
    Add,
    Mul,
    Sub,
    Div,
    EQ,
    LE,
    LT,
    GE,
    GT,
    FAdd,
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
    name: String,
    ty: Box<Type>,
}

#[derive(Debug)]
pub struct Fundef {
    name: Var,
    args: Vec<Var>,
    body: BE,
}

type BE = Box<Expr>;

#[derive(Debug)]
pub enum Expr {
    EConst(Const),
    EOp(Op, Vec<BE>),
    EIf(BE, BE, BE),
    ELet(Var, BE, BE),
    ELetTuple(Var, BE, BE),
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
