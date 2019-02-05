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
use inkwell::values::BasicValue;
use inkwell::values::{
    BasicValueEnum, FloatValue, FunctionValue, GenericValue, IntValue, PhiValue, PointerValue,
};
use inkwell::AddressSpace;
use inkwell::{FloatPredicate, IntPredicate};
use rpds::HashTrieMap;
use std::collections::HashMap;
use std::error::Error;
#[derive(Debug)]
pub enum LlvmError {
    UnboundedVariable(String),
    InvalidOperand(knormal::Var, &'static str),
    NotImpl,
}
pub fn cmp_to_ipred(cmp: &syntax::Cmp) -> IntPredicate {
    match cmp {
        syntax::Cmp::EQ => IntPredicate::EQ,
        syntax::Cmp::NE => IntPredicate::NE,
        syntax::Cmp::LT => IntPredicate::SLT,
        syntax::Cmp::GT => IntPredicate::SGT,
        syntax::Cmp::LE => IntPredicate::SLE,
        syntax::Cmp::GE => IntPredicate::SGE,
    }
}
pub fn cmp_to_fpred(cmp: &syntax::Cmp) -> FloatPredicate {
    match cmp {
        syntax::Cmp::EQ => FloatPredicate::OEQ,
        syntax::Cmp::NE => FloatPredicate::ONE,
        syntax::Cmp::LT => FloatPredicate::OLT,
        syntax::Cmp::GT => FloatPredicate::OGT,
        syntax::Cmp::LE => FloatPredicate::OLE,
        syntax::Cmp::GE => FloatPredicate::OGE,
    }
}
pub struct LlvmEmitter<'a> {
    pub context: &'a Context,
    pub builder: &'a Builder,
    pub fpm: &'a PassManager,
    pub module: &'a Module,
    variables: HashMap<String, BasicValueEnum>,

    tyenv: HashMap<usize, ty::Type>,
    extenv: HashMap<String, usize>,
    builtin: HashMap<String, ty::Type>,
    fn_value_opt: Option<FunctionValue>,
    blockenv: HashMap<String, BasicBlock>,
    phis: Vec<(ir::Phi, PhiValue)>,
}
impl<'a> LlvmEmitter<'a> {
    fn fn_value(&self) -> FunctionValue {
        self.fn_value_opt.unwrap()
    }
    fn construct_phis(&mut self) {
        self.phis.clone().iter().for_each(|(i, phi)| {
            let dst = &i.dst;
            //let mut phi = PhiValue::new(self.variables.get(&dst.0).unwrap().get());
            match self.get_type_by_name(&dst) {
                ty::Type::TyInt | ty::Type::TyBool => {
                    for (v, label) in &i.args {
                        phi.add_incoming(&[(
                            &v.build_i32(self).unwrap(),
                            self.blockenv.get(label).unwrap(),
                        )]);
                    }
                }
                ty::Type::TyFloat => {
                    for (v, label) in &i.args {
                        phi.add_incoming(&[(
                            &v.build_f32(self).unwrap(),
                            self.blockenv.get(label).unwrap(),
                        )]);
                    }
                }
                _ => {
                    for (v, label) in &i.args {
                        phi.add_incoming(&[(
                            &v.build_ptr(self).unwrap(),
                            self.blockenv.get(label).unwrap(),
                        )]);
                    }
                }
                _ => {
                    ()
                    // let tmp : Vec<_>  = args.into_iter().map(|(x,y)| (&x.build_ptr(emitter).unwrap(),y)).collect();
                    // phi.add_incoming(tmp)
                }
            }
        });
    }
    fn get_builtin(&mut self, name: &str) -> FunctionValue {
        match self.module.get_function(name) {
            Some(x) => x,
            None => {
                let args = self
                    .builtin
                    .get(&String::from(name))
                    .unwrap()
                    .clone()
                    .get_args();
                let ftype = match self
                    .builtin
                    .get(&String::from(name))
                    .unwrap()
                    .clone()
                    .get_ret()
                {
                    ty::Type::TyUnit | ty::Type::TyVar(_) => {
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
                self.module.add_function(name, ftype, None)
            }
        }
    }

    fn get_function(&mut self, name: &ir::Name) -> FunctionValue {
        match self.module.get_function(name.0.as_str()) {
            Some(x) => x,
            None => {
                let args = self.get_type_by_name(name).get_args();
                let ftype = match self.get_type_by_name(name).get_ret() {
                    ty::Type::TyUnit | ty::Type::TyVar(_) => {
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
    // fn get_ptr_type_from_llvmtype(&self, ty:BasicTypeEnum) -> PointerType {
    //     self.context.i32_type().ptr_type(AddressSpace::Generic)
    // }

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
        println!("build bool {:?}", self);
        match self {
            knormal::Var::OpVar(x, ty) => match emitter.variables.get(x) {
                Some(x) => Ok(x.into_int_value()),
                None => Err(LlvmError::UnboundedVariable(x.clone())),
            },
            knormal::Var::Constant(Const::CBool(u)) => Ok(emitter
                .context
                .bool_type()
                .const_int(if *u { 1u64 } else { 0u64 }, true)),
            _ => Err(LlvmError::InvalidOperand(self.clone(), "bool")),
        }
    }
    pub fn build_i32(&self, emitter: &mut LlvmEmitter<'a>) -> Result<IntValue, LlvmError> {
        println!("build i32 {:?}", self);
        match self {
            knormal::Var::OpVar(x, ty) => match emitter.variables.get(x) {
                Some(x) => Ok(x.into_int_value()),
                None => Err(LlvmError::UnboundedVariable(x.clone())),
            },
            knormal::Var::Constant(Const::CInt(u)) => {
                Ok(emitter.context.i32_type().const_int(*u as u64, true))
            }
            _ => Err(LlvmError::InvalidOperand(self.clone(), "i32")),
        }
    }
    pub fn build_f32(&self, emitter: &mut LlvmEmitter<'a>) -> Result<FloatValue, LlvmError> {
        println!("build f32 {:?}", self);
        match self {
            knormal::Var::OpVar(x, ty) => match emitter.variables.get(x) {
                Some(x) => Ok(x.into_float_value()),
                None => Err(LlvmError::UnboundedVariable(x.clone())),
            },
            knormal::Var::Constant(Const::CFloat(u)) => {
                Ok(emitter.context.f32_type().const_float(*u as f64))
            }
            _ => Err(LlvmError::InvalidOperand(self.clone(), "f32")),
        }
    }

    pub fn build_ptr(&self, emitter: &mut LlvmEmitter<'a>) -> Result<PointerValue, LlvmError> {
        println!("build ptr {:?}", self);
        match self {
            knormal::Var::OpVar(x, ty) => match emitter.variables.get(x) {
                Some(x) => Ok(x.into_pointer_value()),
                None => Err(LlvmError::UnboundedVariable(x.clone())),
            },
            knormal::Var::Ext(x, ty) => match emitter.extenv.get(x) {
                Some(x) => Ok(emitter.builder.build_int_to_ptr(
                    emitter.context.i32_type().const_int(*x as u64, true),
                    emitter.get_ptr_type(),
                    "",
                )),
                None => Err(LlvmError::UnboundedVariable(x.clone())),
            },
            knormal::Var::Constant(Const::CPtr(u)) => Ok(emitter.builder.build_int_to_ptr(
                emitter.context.i32_type().const_int(*u as u64, true),
                emitter.get_ptr_type(),
                "",
            )),
            _ => Err(LlvmError::InvalidOperand(self.clone(), "pointer")),
        }
    }
    pub fn build_basic_value(
        &self,
        emitter: &mut LlvmEmitter<'a>,
    ) -> Result<BasicValueEnum, LlvmError> {
        match emitter.get_type(self) {
            ty::Type::TyInt => Ok(BasicValueEnum::IntValue(self.build_i32(emitter)?)),
            ty::Type::TyBool => Ok(BasicValueEnum::IntValue(self.build_bool(emitter)?)),
            ty::Type::TyFloat => Ok(BasicValueEnum::FloatValue(self.build_f32(emitter)?)),
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
                        emitter.variables.insert(dst.0.clone(), tmp.into());
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
                        emitter.variables.insert(dst.0.clone(), tmp.into());
                        Ok(())
                    }};
                }
                match opcode {
                    ir::OpBinaryRR::Add => subii!(build_int_add),
                    ir::OpBinaryRR::Sub => subii!(build_int_sub),
                    ir::OpBinaryRR::Div => subii!(build_int_unsigned_div),
                    ir::OpBinaryRR::Mul => subii!(build_int_mul),
                    ir::OpBinaryRR::FAdd => subff!(build_float_add),
                    ir::OpBinaryRR::FSub => subff!(build_float_sub),
                    ir::OpBinaryRR::FMul => subff!(build_float_mul),
                    ir::OpBinaryRR::FDiv => subff!(build_float_div),
                    ir::OpBinaryRR::Cond(cmp) => match emitter.get_type(&lhs) {
                        ty::Type::TyFloat => {
                            let tmp = emitter.builder.build_float_compare(
                                cmp_to_fpred(&cmp),
                                lhs.build_f32(emitter)?,
                                rhs.build_f32(emitter)?,
                                dst.0.as_str(),
                            );
                            emitter.variables.insert(dst.0.clone(), tmp.into());
                            Ok(())
                        }
                        ty::Type::TyBool => {
                            let tmp = emitter.builder.build_int_compare(
                                cmp_to_ipred(&cmp),
                                lhs.build_bool(emitter)?,
                                rhs.build_bool(emitter)?,
                                dst.0.as_str(),
                            );
                            emitter.variables.insert(dst.0.clone(), tmp.into());
                            Ok(())
                        }
                        _ => {
                            let tmp = emitter.builder.build_int_compare(
                                cmp_to_ipred(&cmp),
                                lhs.build_i32(emitter)?,
                                rhs.build_i32(emitter)?,
                                dst.0.as_str(),
                            );
                            emitter.variables.insert(dst.0.clone(), tmp.into());
                            Ok(())
                        }
                    },
                    ir::OpBinaryRR::Array => {
                        let call = match emitter.get_type(rhs) {
                            ty::Type::TyFloat => emitter
                                .builder
                                .build_call(
                                    emitter.get_builtin("create_array_float"),
                                    &[
                                        lhs.build_basic_value(emitter)?,
                                        rhs.build_basic_value(emitter)?,
                                    ],
                                    dst.0.as_str(),
                                )
                                .try_as_basic_value()
                                .left()
                                .unwrap(),

                            ty::Type::TyInt => emitter
                                .builder
                                .build_call(
                                    emitter.get_builtin("create_array"),
                                    &[
                                        lhs.build_basic_value(emitter)?,
                                        rhs.build_basic_value(emitter)?,
                                    ],
                                    dst.0.as_str(),
                                )
                                .try_as_basic_value()
                                .left()
                                .unwrap(),
                            ty::Type::TyBool => emitter
                                .builder
                                .build_call(
                                    emitter.get_builtin("create_array"),
                                    &[
                                        lhs.build_basic_value(emitter)?,
                                        emitter
                                            .builder
                                            .build_int_z_extend_or_bit_cast(
                                                rhs.build_bool(emitter)?,
                                                emitter.context.i32_type(),
                                                "",
                                            )
                                            .into(),
                                    ],
                                    dst.0.as_str(),
                                )
                                .try_as_basic_value()
                                .left()
                                .unwrap(),

                            _ => {
                                let rhs = rhs.build_ptr(emitter)?;
                                let rhs = emitter.builder.build_ptr_to_int(
                                    rhs,
                                    emitter.context.i32_type(),
                                    "",
                                );

                                emitter
                                    .builder
                                    .build_call(
                                        emitter.get_builtin("create_array"),
                                        &[lhs.build_basic_value(emitter)?, rhs.into()],
                                        dst.0.as_str(),
                                    )
                                    .try_as_basic_value()
                                    .left()
                                    .unwrap()
                            }
                        };

                        emitter.variables.insert(dst.0.clone(), call);
                        Ok(())
                    }
                }
            }
            ir::Inst::CallDir { dst, label, args } | ir::Inst::CallCls { dst, label, args } => {
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
                        let tmp = call.left().unwrap();
                        emitter.variables.insert(x.0.clone(), tmp);
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
                        emitter.variables.insert(dst.0.clone(), tmp.into());
                    }
                    ir::OpUnary::Not => {
                        let tmp = emitter
                            .builder
                            .build_not(src.build_bool(emitter)?, dst.0.as_str());
                        emitter.variables.insert(dst.0.clone(), tmp.into());
                    }
                    ir::OpUnary::FNeg => {
                        let tmp = emitter
                            .builder
                            .build_float_neg(src.build_f32(emitter)?, dst.0.as_str());
                        emitter.variables.insert(dst.0.clone(), tmp.into());
                    }
                };
                Ok(())
            }
            ir::Inst::Store { ptr, idx, src } => {
                debug!(
                    "{:?}",
                    (
                        ptr,
                        idx,
                        src,
                        emitter.get_type(ptr),
                        emitter.get_type(idx),
                        emitter.get_type(src)
                    )
                );
                let ptr = emitter.builder.build_ptr_to_int(
                    ptr.build_basic_value(emitter)?.into_pointer_value(),
                    emitter.context.i32_type(),
                    "",
                );
                let ptr = emitter
                    .builder
                    .build_int_add(ptr, idx.build_i32(emitter)?, "");
                let pointer_type = match emitter.get_type(src) {
                    Type::TyFloat => emitter.context.f32_type().ptr_type(AddressSpace::Generic),
                    Type::TyInt | Type::TyBool => {
                        emitter.context.i32_type().ptr_type(AddressSpace::Generic)
                    }
                    _ => emitter.get_ptr_type().ptr_type(AddressSpace::Generic),
                };
                let ptr = emitter.builder.build_int_to_ptr(ptr, pointer_type, "");
                let v = match emitter.get_type(src) {
                    Type::TyBool => emitter.builder.build_store(
                        ptr,
                        emitter.builder.build_int_z_extend(
                            src.build_bool(emitter)?,
                            emitter.context.i32_type(),
                            "",
                        ),
                    ),
                    _ => emitter
                        .builder
                        .build_store(ptr, src.build_basic_value(emitter)?),
                };
                Ok(())
            }
            ir::Inst::Load { dst, ptr, idx } => {
                debug!("{:?}", dst);
                let ptr = emitter.builder.build_ptr_to_int(
                    ptr.build_basic_value(emitter)?.into_pointer_value(),
                    emitter.context.i32_type(),
                    "",
                );
                let ptr = emitter
                    .builder
                    .build_int_add(ptr, idx.build_i32(emitter)?, "");
                let pointer_type = match emitter.get_type_by_name(dst) {
                    Type::TyFloat => emitter.context.f32_type().ptr_type(AddressSpace::Generic),
                    Type::TyInt | Type::TyBool => {
                        emitter.context.i32_type().ptr_type(AddressSpace::Generic)
                    }
                    _ => emitter.get_ptr_type().ptr_type(AddressSpace::Generic),
                };
                let ptr = emitter.builder.build_int_to_ptr(ptr, pointer_type, "");
                let v = emitter.builder.build_load(ptr, dst.0.as_str());
                let v = match emitter.get_type_by_name(dst) {
                    Type::TyBool => emitter
                        .builder
                        .build_int_truncate_or_bit_cast(
                            v.into_int_value(),
                            emitter.context.bool_type(),
                            "",
                        )
                        .into(),
                    _ => v,
                };

                emitter.variables.insert(dst.0.clone(), v);
                debug!("{:?} is added", dst);
                Ok(())
            }
            ir::Inst::Phi(ir::Phi { dst, args }) => {
                let phi = emitter.builder.build_phi(
                    emitter.get_type_by_name(dst).to_llvm_type(emitter),
                    dst.0.as_str(),
                );

                emitter
                    .variables
                    .insert(dst.0.clone(), phi.as_basic_value().into());

                emitter.phis.push(
                    ((
                        ir::Phi {
                            dst: dst.clone(),
                            args: args.clone(),
                        },
                        phi,
                    )),
                );
                Ok(())
            }
            ir::Inst::Mv { src, dst } => {
                let tmp = src.build_basic_value(emitter)?;
                emitter.variables.insert(dst.0.clone(), tmp);
                Ok(())
            }
            ir::Inst::ArrayAlloc { src, dst, len, ptr } => {
                let tmp = knormal::Var::Constant(syntax::Const::CPtr(*ptr as i32));
                let val = tmp.build_basic_value(emitter)?;
                emitter.variables.insert(dst.clone().0, val);
                for i in 0..*len {
                    let inst = ir::Inst::Store {
                        src: src.clone(),
                        idx: knormal::Var::Constant(syntax::Const::CInt(i)),
                        ptr: tmp.clone(),
                    };
                    inst.build(emitter);
                }
                Ok(())
            }
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
        self.inst.iter().try_for_each(|x| x.build(emitter))?;
        self.build_control_flow(emitter)?;
        Ok(())
    }
    pub fn build_control_flow(&self, emitter: &mut LlvmEmitter<'a>) -> Result<(), LlvmError> {
        match &self.last {
            ir::ControlFlow::Return(Some(x)) => match emitter.get_type(&x) {
                _ => emitter
                    .builder
                    .build_return(Some(&x.build_basic_value(emitter)?)),
                // Type::TyFloat => emitter.builder.build_return(Some(&x.build_basic_value(emitter)?)),
                // Type::TyBool => emitter.builder.build_return(Some(&x.build_bool(emitter)?)),
                // Type::TyInt => emitter.builder.build_return(Some(&x.build_i32(emitter)?)),
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
            Type::TyUnit | Type::TyVar(_) => {
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
        debug!("{:?}", self);
        let f = emitter.get_function(&self.name);

        for (i, arg) in f.get_param_iter().enumerate() {
            let arg_name = self.args[i].0.clone();
            emitter.variables.insert(arg_name, arg);
        }
        emitter.fn_value_opt = Some(f);
        self.blocks.iter().try_for_each(|x| x.add_bb(emitter));
        self.blocks.iter().try_for_each(|x| x.build(emitter))
    }
}
pub fn f(
    functions: Vec<ir::Fundef>,
    tyenv: HashMap<usize, ty::Type>,
    extenv: HashMap<String, usize>,
    builtin: HashMap<String, ty::Type>,
    file: String,
) -> Result<(), LlvmError> {
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
        variables: HashMap::new(),
        tyenv: tyenv,
        extenv: extenv,
        builtin: builtin,
        blockenv: HashMap::new(),
        phis: Vec::new(),
    };
    functions
        .iter()
        .try_for_each(|x| x.compile_prototype(&mut emitter));
    functions.iter().try_for_each(|x| x.compile(&mut emitter))?;
    emitter.construct_phis();
    emitter.module.print_to_file(newfilename);
    Ok(())
}
