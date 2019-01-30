use crate::syntax;
#[derive(Debug, Clone)]
pub enum Type {
    TyUnit,
    TyBool,
    TyInt,
    TyFloat,
    TyPtr,
    TyFun(Vec<Type>, Box<Type>),
    TyTuple(Vec<Type>),
    TyArray(Box<Type>),
    TyVar(usize),
}
pub fn get_element(t: Type) -> Type {
    match t {
        Type::TyArray(y) => *y,
        _ => unreachable!(),
    }
}
impl Type {
    pub fn get_ret(self) -> Type {
        match self {
            Type::TyFun(x, y) => *y,
            _ => unreachable!(),
        }
    }
    pub fn get_args(self) -> Vec<Type> {
        match self {
            Type::TyFun(x, y) => x,
            _ => unreachable!(),
        }
    }
}
pub fn unitfun() -> Type {
    Type::TyFun(vec![Type::TyUnit], Box::new(Type::TyUnit))
}
pub fn alpha() -> Type {
    Type::TyVar(syntax::genvar())
}
