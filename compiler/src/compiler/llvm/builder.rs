use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::ContextRef;
use inkwell::module::Module;
use inkwell::values::{BasicValueEnum, FloatValue, FunctionValue, PointerValue};

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

    /// creates a Box<Iterator>
    fn create_iterator(&mut self, id: IterId, iter: PointerValue<'ctx>);
    /// returns a Box<Iterator>
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

    fn create_capture_stack(&self, variables: &[VariableId]) -> PointerValue<'ctx> {
        let ty = self
            .context()
            .f64_type()
            .ptr_type(Default::default())
            .array_type(variables.len() as u32);

        let stack = self.builder().build_alloca(ty, "capture_stack");

        let i32_ty = self.context().i32_type();
        let zero = i32_ty.const_zero();

        let mut i = 0;
        for v in variables {
            let ptr = self.get_variable_ptr(*v);

            let index = i32_ty.const_int(i, false);

            let iptr = self.builder().build_gep(ty, stack, &[zero, index], "gep");

            self.builder().build_store(iptr, ptr);
            i += 1;
        }

        let stack_ty = self
            .context()
            .f64_type()
            .ptr_type(Default::default())
            .ptr_type(Default::default());

        let stack = self
            .builder()
            .build_pointer_cast(stack, stack_ty, "ptr_cast");

        return stack;
    }

    fn throw(&self, value: FloatValue<'ctx>);
    fn call(
        &self,
        func: FunctionValue<'ctx>,
        args: &[BasicValueEnum<'ctx>],
    ) -> Option<BasicValueEnum<'ctx>>;
    fn await_(&self, value: FloatValue<'ctx>) -> FloatValue<'ctx>;
    fn yield_(&self, value: FloatValue<'ctx>) -> FloatValue<'ctx>;

    fn finish(&mut self) -> FunctionValue<'ctx>;
}
