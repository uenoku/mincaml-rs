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
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::types::PointerType;
use inkwell::values::{
    BasicValueEnum, FloatValue, FunctionValue, GenericValue, IntValue, PointerValue,
};
use inkwell::AddressSpace;
use inkwell::{FloatPredicate, IntPredicate};
use std::collections::HashMap;
use std::error::Error;
#[derive(Debug)]
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
    blockenv: HashMap<String, BasicBlock>,
}
impl<'a> LlvmEmitter<'a> {
    fn fn_value(&self) -> FunctionValue {
        self.fn_value_opt.unwrap()
    }
    fn get_function(&mut self, name: &ir::Name) -> FunctionValue {
        match self.module.get_function(name.0.as_str()) {
            Some(x) => x,
            None => {
                let args = self.get_type_by_name(name).get_args();
                let ftype = match self.get_type_by_name(name).get_ret() {
                    Type::TyUnit => {
                        let ret = self.context.void_type();
                        let argsty = types_to_llvmtypes(&args, self);
                        ret.fn_type(&argsty, false)
                    }
                    x => {
                        let ret = x.to_llvm_type(self);
                        let argsty = types_to_llvmtypes(&args, self);
                        ret.fn_type(&argsty, false)
                    }
                };
                self.module.add_function(name.0.as_str(), ftype, None)
            }
        }
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
        info!("{:?}", self);
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
    pub fn build_basic_value(
        &self,
        emitter: &mut LlvmEmitter<'a>,
    ) -> Result<BasicValueEnum, LlvmError> {
        match emitter.get_type(self) {
            Type::TyFloat => Ok(BasicValueEnum::FloatValue(self.build_f32(emitter)?)),
            Type::TyInt => Ok(BasicValueEnum::IntValue(self.build_i32(emitter)?)),
            Type::TyBool => Ok(BasicValueEnum::IntValue(self.build_bool(emitter)?)),
            _ => Ok(BasicValueEnum::PointerValue(self.build_ptr(emitter)?)),
        }
    }
    pub fn build_bitcast_to_i32(
        &self,
        emitter: &mut LlvmEmitter<'a>,
    ) -> Result<IntValue, LlvmError> {
        let p = match emitter.get_type(self) {
            Type::TyFloat => unreachable!(),
            Type::TyBool => {
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
                        Type::TyFloat => {
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
            ir::Inst::CallDir { dst, label, args } => {
                let call = emitter
                    .builder
                    .build_call(
                        emitter.get_function(&label),
                        &args_to_llvmvalues(args, emitter),
                        match dst {
                            Some(x) => x.0.as_str(),
                            None => "",
                        },
                    )
                    .try_as_basic_value();
                match dst {
                    Some(x) => {
                        let tmp: BasicValueEnum = call.left().unwrap();
                        match emitter.get_type_by_name(&x) {
                            Type::TyFloat => emitter
                                .fvariables
                                .insert(x.0.clone(), tmp.into_float_value()),
                            Type::TyBool => emitter
                                .bvariables
                                .insert(x.0.clone(), tmp.into_float_value()),
                            Type::TyInt => {
                                emitter.ivariables.insert(x.0.clone(), tmp.into_int_value())
                            }
                            _ => emitter.ivariables.insert(
                                x.0.clone(),
                                tmp.into_pointer_value()
                                    .const_to_int(emitter.context.i32_type()),
                            ),
                        };
                    }
                    None => (),
                };
                Ok(())
            }
            ir::Inst::Unary { opcode, dst, src } => {
                match opcode {
                    ir::OpUnary::Neg => {
                        let tmp = emitter
                            .builder
                            .build_int_neg(src.build_i32(emitter)?, dst.0.as_str());
                        emitter.ivariables.insert(dst.0.clone(), tmp);
                    }
                    ir::OpUnary::Not => {
                        let tmp = emitter
                            .builder
                            .build_not(src.build_bool(emitter)?, dst.0.as_str());
                        emitter.bvariables.insert(dst.0.clone(), tmp);
                    }
                    ir::OpUnary::FNeg => {
                        let tmp = emitter
                            .builder
                            .build_float_neg(src.build_f32(emitter)?, dst.0.as_str());
                        emitter.fvariables.insert(dst.0.clone(), tmp);
                    }
                };
                Ok(())
            }
            ir::Inst::Store { ptr, idx, src } => Ok(()),
            ir::Inst::Mv { src, dst } => Ok(()),
            ir::Inst::Load { dst, ptr, idx } => Ok(()),
            ir::Inst::Phi(ir::Phi { dst, args }) => Ok(()),
            _ => unimplemented!(),
        }
    }
}
impl<'a> ir::Block {
    pub fn add_bb(&self, emitter: &mut LlvmEmitter<'a>) -> Result<(), LlvmError> {
        let parent = emitter.fn_value();
        let bb = emitter
            .context
            .append_basic_block(&parent, self.label.as_str());
        emitter.blockenv.insert(self.label.clone(), bb);
        Ok(())
    }
    pub fn build(&self, emitter: &mut LlvmEmitter<'a>) -> Result<(), LlvmError> {
        let bb = emitter.blockenv.get(&self.label).unwrap();
        emitter.builder.position_at_end(&bb);
        self.inst.iter().try_for_each(|x| {
            info!("{:?}", x);
            x.build(emitter)
        });
        self.build_control_flow(emitter);
        Ok(())
    }
    pub fn build_control_flow(&self, emitter: &mut LlvmEmitter<'a>) -> Result<(), LlvmError> {
        match &self.last {
            ir::ControlFlow::Return(Some(x)) => match emitter.get_type(&x) {
                Type::TyFloat => emitter.builder.build_return(Some(&x.build_f32(emitter)?)),
                Type::TyBool => emitter.builder.build_return(Some(&x.build_bool(emitter)?)),
                _ => emitter.builder.build_return(Some(&x.build_i32(emitter)?)),
            },
            ir::ControlFlow::Return(None) => emitter.builder.build_return(None),
            ir::ControlFlow::Branch(cond, ifthen, ifelse) => {
                emitter.builder.build_conditional_branch(
                    cond.build_bool(emitter)?,
                    emitter.blockenv.get(ifthen).unwrap(),
                    emitter.blockenv.get(ifelse).unwrap(),
                )
            }
            ir::ControlFlow::Jump(label) => emitter
                .builder
                .build_unconditional_branch(emitter.blockenv.get(label).unwrap()),
        };
        Ok(())
    }
}

impl<'a> ty::Type {
    pub fn to_llvm_type(&self, emitter: &mut LlvmEmitter<'a>) -> BasicTypeEnum {
        match self {
            ty::Type::TyInt => emitter.context.i32_type().as_basic_type_enum(),
            ty::Type::TyFloat => emitter.context.f32_type().as_basic_type_enum(),
            ty::Type::TyBool => emitter.context.bool_type().as_basic_type_enum(),
            ty::Type::TyUnit => unreachable!(),
            _ => emitter.get_ptr_type().as_basic_type_enum(),
        }
    }
}
pub fn args_to_llvmvalues<'a>(
    args: &Vec<knormal::Var>,
    emitter: &mut LlvmEmitter<'a>,
) -> Vec<BasicValueEnum> {
    let args: Vec<_> = args
        .iter()
        .map(|x| (emitter.get_type(x), x))
        .filter(|x| match x.0 {
            Type::TyUnit => false,
            _ => true,
        })
        .map(|x| x.1)
        .collect();
    let args: Vec<_> = args
        .iter()
        .map(|x| x.build_basic_value(emitter).unwrap())
        .collect();
    args
}

pub fn types_to_llvmtypes<'a>(
    args: &Vec<ty::Type>,
    emitter: &mut LlvmEmitter<'a>,
) -> Vec<BasicTypeEnum> {
    let args: Vec<_> = args
        .iter()
        .filter(|x| match x {
            Type::TyUnit => false,
            _ => true,
        })
        .collect();
    let args: Vec<_> = args.iter().map(|x| x.to_llvm_type(emitter)).collect();
    args
}
pub fn args_to_llvmtypes<'a>(
    args: &Vec<ir::Name>,
    emitter: &mut LlvmEmitter<'a>,
) -> Vec<BasicTypeEnum> {
    let args: Vec<_> = args
        .iter()
        .map(|x| emitter.get_type_by_name(x))
        .filter(|x| match x {
            Type::TyUnit => false,
            _ => true,
        })
        .collect();
    let args: Vec<_> = args.iter().map(|x| x.to_llvm_type(emitter)).collect();
    args
}
impl<'a> ir::Fundef {
    pub fn compile_prototype(&self, emitter: &mut LlvmEmitter<'a>) -> Result<(), LlvmError> {
        match emitter.get_type_by_name(&self.name).get_ret() {
            Type::TyUnit => {
                info!("{:?}", emitter.get_type_by_name(&self.name).get_ret());
                info!("{}'s return type is void ", self.name.0);
                let ret_type = emitter.context.void_type();
                let args = args_to_llvmtypes(&self.args, emitter);
                let args = args.as_slice();
                let fn_type = ret_type.fn_type(args, false);
                let fn_val = emitter
                    .module
                    .add_function(self.name.0.as_str(), fn_type, None);

                for (i, arg) in fn_val.get_param_iter().enumerate() {
                    let a = &self.args[i];
                    let ty = emitter.get_type_by_name(a);
                    match ty {
                        Type::TyFloat => arg.into_float_value().set_name(self.args[i].0.as_str()),
                        Type::TyInt | Type::TyBool => {
                            arg.into_int_value().set_name(self.args[i].0.as_str())
                        }
                        _ => arg.into_pointer_value().set_name(self.args[i].0.as_str()),
                    }
                }
                Ok(())
            }
            _ => {
                let ret_type = emitter
                    .get_type_by_name(&self.name)
                    .get_ret()
                    .to_llvm_type(emitter);
                let args = args_to_llvmtypes(&self.args, emitter);
                let args = args.as_slice();

                let fn_type = ret_type.fn_type(args, false);
                let fn_val = emitter
                    .module
                    .add_function(self.name.0.as_str(), fn_type, None);

                for (i, arg) in fn_val.get_param_iter().enumerate() {
                    let a = &self.args[i];
                    let ty = emitter.get_type_by_name(a);
                    match ty {
                        Type::TyFloat => arg.into_float_value().set_name(self.args[i].0.as_str()),
                        Type::TyInt | Type::TyBool => {
                            arg.into_int_value().set_name(self.args[i].0.as_str())
                        }
                        _ => arg.into_pointer_value().set_name(self.args[i].0.as_str()),
                    }
                }
                Ok(())
            }
        }
    }

    pub fn compile(&self, emitter: &mut LlvmEmitter<'a>) -> Result<(), LlvmError> {
        let f = emitter.get_function(&self.name);
        emitter.fn_value_opt = Some(f);
        self.blocks.iter().try_for_each(|x| x.add_bb(emitter));
        self.blocks.iter().try_for_each(|x| x.build(emitter))
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
        blockenv: HashMap::new(),
    };
    functions
        .iter()
        .try_for_each(|x| x.compile_prototype(&mut emitter));
    functions.iter().try_for_each(|x| x.compile(&mut emitter));
    emitter.module.print_to_file(newfilename);
}
