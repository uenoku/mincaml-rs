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
pub fn unitfun() -> Type {
    Type::TyFun(vec![Type::TyUnit], Box::new(Type::TyUnit))
}
pub fn alpha() -> Type {
    Type::TyVar(syntax::genvar())
}
pub fn get_element(t: Type) -> Type {
    match t {
        Type::TyArray(y) => *y,
        _ => unreachable!(),
    }
}
