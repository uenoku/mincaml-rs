use crate::closure;
use crate::ir;
use crate::knormal;
use crate::syntax;
use crate::syntax::Const;
use crate::ty;
use crate::ty::Type;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::BasicTypeEnum;
use inkwell::types::PointerType;
use inkwell::values::{
    BasicValueEnum, FloatValue, FunctionValue, GenericValue, IntValue, PointerValue,
};
use inkwell::AddressSpace;
use inkwell::{FloatPredicate, IntPredicate};
use std::collections::HashMap;
use std::error::Error;
pub enum LlvmError {
    UnboundedVariable(String),
    InvalidOperand(&'static str),
    NotImpl,
}
pub fn cmp_to_ipred(cmp: &syntax::Cmp) -> IntPredicate {
    match cmp {
        EQ => IntPredicate::EQ,
        NE => IntPredicate::NE,
        LT => IntPredicate::SLT,
        GT => IntPredicate::SGT,
        LE => IntPredicate::SLE,
        GE => IntPredicate::SGE,
    }
}
pub fn cmp_to_fpred(cmp: &syntax::Cmp) -> FloatPredicate {
    match cmp {
        EQ => FloatPredicate::OEQ,
        NE => FloatPredicate::ONE,
        LT => FloatPredicate::OLT,
        GT => FloatPredicate::OGT,
        LE => FloatPredicate::OLE,
        GE => FloatPredicate::OGE,
    }
}
pub struct LlvmEmitter<'a> {
    pub context: &'a Context,
    pub builder: &'a Builder,
    pub fpm: &'a PassManager,
    pub module: &'a Module,
    ivariables: HashMap<String, IntValue>,
    fvariables: HashMap<String, FloatValue>,
    bvariables: HashMap<String, IntValue>,

    tyenv: HashMap<usize, ty::Type>,
    extenv: HashMap<usize, usize>,
    fn_value_opt: Option<FunctionValue>,
}
impl<'a> LlvmEmitter<'a> {
    fn fn_value(&self) -> FunctionValue {
        self.fn_value_opt.unwrap()
    }
    fn get_function(&self, name: &String) -> Option<FunctionValue> {
        self.module.get_function(name.as_str())
    }
    fn get_ptr_type(&self) -> PointerType {
        self.context.i32_type().ptr_type(AddressSpace::Generic)
    }
    fn get_type_by_name(&self, (x, ty): &ir::Name) -> Type {
        self.tyenv.get(ty).unwrap().clone()
    }
    fn get_type(&self, var: &knormal::Var) -> Type {
        match var {
            knormal::Var::OpVar(x, ty) | knormal::Var::Ext(x, ty) => {
                self.tyenv.get(ty).unwrap().clone()
            }
            knormal::Var::Constant(c) => syntax::infer_const(&c),
        }
    }
}
impl<'a> knormal::Var {
    pub fn build_bool(&self, emitter: &mut LlvmEmitter<'a>) -> Result<IntValue, LlvmError> {
        match self {
            knormal::Var::OpVar(x, ty) | knormal::Var::Ext(x, ty) => {
                match emitter.bvariables.get(x) {
                    Some(x) => Ok(*x),
                    None => Err(LlvmError::UnboundedVariable(x.clone())),
                }
            }
            knormal::Var::Constant(Const::CBool(u)) => Ok(emitter
                .context
                .bool_type()
                .const_int(if *u { 1u64 } else { 0u64 }, true)),
            _ => Err(LlvmError::InvalidOperand("bool")),
        }
    }
    pub fn build_i32(&self, emitter: &mut LlvmEmitter<'a>) -> Result<IntValue, LlvmError> {
        match self {
            knormal::Var::OpVar(x, ty) | knormal::Var::Ext(x, ty) => {
                match emitter.ivariables.get(x) {
                    Some(x) => Ok(*x),
                    None => Err(LlvmError::UnboundedVariable(x.clone())),
                }
            }
            knormal::Var::Constant(Const::CInt(u)) => {
                Ok(emitter.context.i32_type().const_int(*u as u64, true))
            }
            _ => Err(LlvmError::InvalidOperand("i32")),
        }
    }
    pub fn build_f32(&self, emitter: &mut LlvmEmitter<'a>) -> Result<FloatValue, LlvmError> {
        match self {
            knormal::Var::OpVar(x, ty) | knormal::Var::Ext(x, ty) => {
                match emitter.fvariables.get(x) {
                    Some(x) => Ok(*x),
                    None => Err(LlvmError::UnboundedVariable(x.clone())),
                }
            }
            knormal::Var::Constant(Const::CFloat(u)) => {
                Ok(emitter.context.f32_type().const_float(*u as f64))
            }
            _ => Err(LlvmError::InvalidOperand("f32")),
        }
    }

    pub fn build_ptr(&self, emitter: &mut LlvmEmitter<'a>) -> Result<PointerValue, LlvmError> {
        let p = self.build_i32(emitter)?;
        Ok(p.const_to_pointer(emitter.get_ptr_type()))
    }
    pub fn build_bitcast_to_i32(
        &self,
        emitter: &mut LlvmEmitter<'a>,
    ) -> Result<IntValue, LlvmError> {
        let p = match emitter.get_type(self) {
            TyFloat => unreachable!(),
            TyBool => {
                let new = syntax::genname();
                emitter.builder.build_bitcast(
                    self.build_bool(emitter)?,
                    emitter.context.i32_type(),
                    new.as_str(),
                )
            }
            _ => self.build_i32(emitter)?,
        };
        Ok(p)
    }
}
impl<'a> ir::Inst {
    fn build(&self, emitter: &mut LlvmEmitter<'a>) -> Result<(), LlvmError> {
        match self {
            ir::Inst::BinaryRR {
                opcode,
                dst,
                lhs,
                rhs,
            } => {
                macro_rules! subii {
                    ($e1:ident) => {{
                        let tmp = emitter.builder.$e1(
                            lhs.build_i32(emitter)?,
                            rhs.build_i32(emitter)?,
                            dst.0.as_str(),
                        );
                        emitter.ivariables.insert(dst.0.clone(), tmp);
                        Ok(())
                    }};
                }
                macro_rules! subff {
                    ($e1:ident) => {{
                        let tmp = emitter.builder.$e1(
                            lhs.build_f32(emitter)?,
                            rhs.build_f32(emitter)?,
                            dst.0.as_str(),
                        );
                        emitter.fvariables.insert(dst.0.clone(), tmp);
                        Ok(())
                    }};
                }
                match opcode {
                    ir::OpBinaryRR::Add => subii!(build_int_add),
                    ir::OpBinaryRR::Sub => subii!(build_int_sub),
                    ir::OpBinaryRR::FAdd => subff!(build_float_add),
                    ir::OpBinaryRR::FSub => subff!(build_float_sub),
                    ir::OpBinaryRR::FMul => subff!(build_float_mul),
                    ir::OpBinaryRR::FDiv => subff!(build_float_div),
                    ir::OpBinaryRR::Cond(cmp) => match emitter.get_type(&lhs) {
                        TyFloat => {
                            let tmp = emitter.builder.build_float_compare(
                                cmp_to_fpred(&cmp),
                                lhs.build_f32(emitter)?,
                                rhs.build_f32(emitter)?,
                                dst.0.as_str(),
                            );
                            emitter.bvariables.insert(dst.0.clone(), tmp);
                            Ok(())
                        }
                        _ => {
                            let tmp = emitter.builder.build_int_compare(
                                cmp_to_ipred(&cmp),
                                lhs.build_i32(emitter)?,
                                rhs.build_i32(emitter)?,
                                dst.0.as_str(),
                            );
                            emitter.bvariables.insert(dst.0.clone(), tmp);
                            Ok(())
                        }
                    },
                    _ => Err(LlvmError::NotImpl),
                }
            }
            _ => Err(LlvmError::NotImpl),
        }
    }
}
impl<'a> ir::Block {
    pub fn build(&self, emitter: &mut LlvmEmitter<'a>) -> Result<(), LlvmError> {
        let parent = emitter.fn_value();
        emitter
            .context
            .append_basic_block(&parent, self.label.as_str());
        for i in &self.inst {
            i.build(emitter)?;
        }
        Ok(())
    }
}

impl<'a> ir::Fundef {
    pub fn compile_prototype(&self, emitter: &mut LlvmEmitter<'a>) {
        let ret_type = emitter.get_type_by_name(&self.name);
        let args: Vec<_> = self
            .args
            .iter()
            .map(|x| emitter.get_type_by_name(x))
            .collect();
        let args = args.as_slice();
    }
    pub fn compile(&self, emitter: &mut LlvmEmitter<'a>) -> Result<(), LlvmError> {
        Ok(())
    }
}
pub fn f(
    functions: Vec<ir::Fundef>,
    tyenv: HashMap<usize, ty::Type>,
    extenv: HashMap<usize, usize>,
    file: String,
) {
    let mut newfilename = file.clone();
    newfilename.push_str(".ll");
    let context = Context::create();
    let module = context.create_module("mincaml");
    let builder = context.create_builder();
    let fpm = PassManager::create_for_function(&module);
    let mut emitter = LlvmEmitter {
        context: &context,
        builder: &builder,
        fpm: &fpm,
        module: &module,
        fn_value_opt: None,
        ivariables: HashMap::new(),
        fvariables: HashMap::new(),
        bvariables: HashMap::new(),
        tyenv: tyenv,
        extenv: extenv,
    };
    functions
        .iter()
        .for_each(|x| x.compile_prototype(&mut emitter));
    functions.iter().try_for_each(|x| x.compile(&mut emitter));
    emitter.module.print_to_file(newfilename);
}
