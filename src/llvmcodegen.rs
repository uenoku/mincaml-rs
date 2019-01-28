use crate::closure;
use crate::ir;
use crate::knormal;
use crate::ty;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, FloatValue, FunctionValue, PointerValue};
use std::collections::HashMap;

pub struct LlvmEmitter<'a> {
    pub context: &'a Context,
    pub builder: &'a Builder,
    pub fpm: &'a PassManager,
    pub module: &'a Module,
    variables: HashMap<String, PointerValue>,
    tyenv: HashMap<usize, ty::Type>,
}
impl<'a> LlvmEmitter<'a> {
    fn get_function(&self, name: &String) -> Option<FunctionValue> {
        self.module.get_function(name.as_str())
    }
}
impl<'a> knormal::Var {
    fn build(&self, emitter: LlvmEmitter<'a>) {}
}
impl<'a> ir::Inst {
    fn build(&self, emitter: LlvmEmitter<'a>) {}
}
