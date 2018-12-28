#[derive(Debug, Clone)]
pub enum Type {
    TyUnit,
    TyBool,
    TyInt,
    TyFloat,
    TyFun(Vec<Type>, Box<Type>),
    TyTuple(Vec<Type>),
    TyArray(Box<Type>),
    TyVar(usize),
}
