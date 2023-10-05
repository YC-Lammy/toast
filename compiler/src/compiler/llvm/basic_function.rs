use std::collections::HashMap;
use std::sync::Arc;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::ContextRef;
use inkwell::module::Module;
use inkwell::types::IntType;
use inkwell::values::{FloatValue, FunctionValue, IntValue, PointerValue};
use swc_atoms::JsWord;

use crate::ir_builder::ir::{ArgListId, IterId, TempId};
use crate::ir_builder::{IRFunction, VariableId};

pub struct NormalBuilder<'ctx> {
    ir_function: Arc<IRFunction>,

    context: ContextRef<'ctx>,
    module: Arc<Module<'ctx>>,
    builder: Builder<'ctx>,
    function: FunctionValue<'ctx>,

    size_ty: IntType<'ctx>,

    acc: PointerValue<'ctx>,
    this: PointerValue<'ctx>,
    return_block: BasicBlock<'ctx>,
    /// pointer to GcPtr<Array<GcPtr<Any>>>
    capture_stack: PointerValue<'ctx>,

    /// body, label
    continues: Vec<(BasicBlock<'ctx>, Option<JsWord>)>,
    /// exit, label
    breaks: Vec<(BasicBlock<'ctx>, Option<JsWord>)>,

    iterators: HashMap<IterId, PointerValue<'ctx>>,
    temps: HashMap<TempId, PointerValue<'ctx>>,

    args: HashMap<ArgListId, Vec<FloatValue<'ctx>>>,

    /// stores the error
    error: PointerValue<'ctx>,
    error_handlers: Vec<BasicBlock<'ctx>>,

    /// variables and their corresponding location
    stack_variables: HashMap<VariableId, PointerValue<'ctx>>,
    /// pointer to the GcPtr<Any>
    heap_variables: HashMap<VariableId, PointerValue<'ctx>>,
}

impl<'ctx> super::builder::FunctionBuilder<'ctx> for NormalBuilder {
    fn context(&self) -> &ContextRef<'ctx> {
        return &self.context;
    }
    fn module(&self) -> &Module<'ctx> {
        return &self.module;
    }
    fn builder(&self) -> &Builder<'ctx> {
        return &self.builder;
    }
    fn function_value(&self) -> FunctionValue<'ctx> {
        return self.function;
    }
    fn ir_function(&self) -> &IRFunction {
        return &self.ir_function;
    }

    fn this_ptr(&self) -> PointerValue<'ctx> {
        return self.this;
    }
    fn acc_ptr(&self) -> PointerValue<'ctx> {
        return self.acc;
    }
    fn capture_stack(&self) -> PointerValue<'ctx> {
        return self.capture_stack;
    }
    fn error_ptr(&self) -> PointerValue<'ctx> {
        return self.error;
    }
}
