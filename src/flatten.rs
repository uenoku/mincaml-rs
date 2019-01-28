type BE = Box<CExpr>;

#[derive(Debug, Clone)]
pub struct Fundef {
    pub name: (String, usize),
    pub args: Vec<(String, usize)>,
    pub formal_fv: Vec<(String, usize)>,
    pub body: BE,
}
#[derive(Debug, Clone)]
pub struct Closure {
    entry: String,
    actual_fv: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum FU {
    FVar(Var), 
    FOp(Op, Vec<Var>),
    FIf(Cmp, Var, Var, BE, BE),
    FTuple(Vec<Var>),
    FMakeCls((String, usize), Closure, BE),
    FAppCls(String, Vec<Var>),
    FAppDir(String, Vec<Var>),
}

#[derive(Debug, Clone)]
pub enum FV {
    FLet((String, usize), Box<FU>, Box<FV>),
    FAns(Box<FV>),
}
