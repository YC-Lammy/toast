use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::ContextRef;
use inkwell::module::Module;
use inkwell::values::{BasicValueEnum, FloatValue, FunctionValue, IntValue, PointerValue};

use swc_atoms::JsWord;

use crate::ir_builder::ir::{ArgListId, IterId, TempId};
use crate::ir_builder::{IRFunction, VariableId};

pub trait FunctionBuilder<'ctx> {
    fn context(&self) -> &ContextRef<'ctx>;
    fn module(&self) -> &Module<'ctx>;
    fn builder(&self) -> &Builder<'ctx>;
    fn function_value(&self) -> FunctionValue<'ctx>;
    fn ir_function(&self) -> &IRFunction;

    /// stack location of the acc
    fn acc_ptr(&self) -> PointerValue<'ctx>;
    /// pointer to this value
    fn this_ptr(&self) -> PointerValue<'ctx>;
    /// pointer to GcPtr<Array<GcPtr<Any>>>
    fn capture_stack(&self) -> PointerValue<'ctx>;
    /// location where errors are stored
    fn error_ptr(&self) -> PointerValue<'ctx>;

    /// the return block
    fn return_block(&self) -> BasicBlock<'ctx>;

    fn push_continue_block(&mut self, block: BasicBlock<'ctx>, label: Option<JsWord>);
    fn pop_continue_block(&mut self);
    /// return the labeled block if label is some
    fn get_continue_block(&mut self, label: Option<&JsWord>) -> BasicBlock<'ctx>;

    fn push_break_block(&mut self, block: BasicBlock<'ctx>, label: Option<JsWord>);
    fn pop_break_block(&mut self);
    fn get_break_block(&mut self, label: Option<&JsWord>) -> BasicBlock<'ctx>;

    fn create_iterator(&mut self, id: IterId, ptr: PointerValue<'ctx>);
    fn destroy_iterator(&mut self, id: IterId);
    fn get_iterator(&self, id: IterId) -> PointerValue<'ctx>;

    fn create_temp(&mut self, id: TempId);
    fn release_temp(&mut self, id: TempId);
    fn get_temp(&self, id: TempId) -> PointerValue<'ctx>;

    fn push_error_handler(&mut self, block: BasicBlock<'ctx>);
    fn pop_error_handler(&mut self);
    fn get_error_handler(&self) -> Option<BasicBlock<'ctx>>;

    fn create_arg_list(&mut self, id: ArgListId);
    fn drop_arg_list(&mut self, id: ArgListId);
    fn push_arg_list(&mut self, id: ArgListId, value: FloatValue<'ctx>);

    fn declare_variable(&mut self, id: VariableId);
    fn drop_variable(&mut self, id: VariableId);
    fn get_variable_ptr(&self, id: VariableId) -> PointerValue<'ctx>;

    fn throw(&self, value: FloatValue<'ctx>);
    fn call(
        &self,
        func: FunctionValue<'ctx>,
        args: &[BasicValueEnum<'ctx>],
    ) -> Option<BasicValueEnum<'ctx>>;
    fn await_(&self, value: FloatValue<'ctx>) -> FloatValue<'ctx>;
    fn yield_(&self, value: FloatValue<'ctx>) -> FloatValue<'ctx>;

    fn finish(&mut self);
}
