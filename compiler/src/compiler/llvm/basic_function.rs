use std::collections::HashMap;
use std::sync::Arc;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::ContextRef;
use inkwell::module::Module;
use inkwell::types::IntType;
use inkwell::values::{
    BasicMetadataValueEnum, BasicValueEnum, FloatValue, FunctionValue, PointerValue,
};
use itertools::Itertools;
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

    iterators: Vec<(IterId, PointerValue<'ctx>)>,
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

impl<'ctx> super::builder::FunctionBuilder<'ctx> for NormalBuilder<'ctx> {
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

    fn return_block(&self) -> BasicBlock<'ctx> {
        return self.return_block;
    }

    fn push_continue_block(&mut self, block: BasicBlock<'ctx>, label: Option<JsWord>) {
        self.continues.push((block, label));
    }
    fn pop_continue_block(&mut self) {
        self.continues.pop();
    }
    fn get_continue_block(&mut self, label: Option<&JsWord>) -> BasicBlock<'ctx> {
        if let Some(label) = label {
            for (b, l) in self.continues.iter().rev() {
                if l.as_ref() == Some(label) {
                    return *b;
                }
            }

            unreachable!()
        } else {
            return self.continues.last().unwrap().0;
        }
    }

    fn push_break_block(&mut self, block: BasicBlock<'ctx>, label: Option<JsWord>) {
        self.breaks.push((block, label));
    }
    fn pop_break_block(&mut self) {
        self.breaks.pop();
    }
    fn get_break_block(&mut self, label: Option<&JsWord>) -> BasicBlock<'ctx> {
        if let Some(label) = label {
            for (b, l) in self.breaks.iter().rev() {
                if l.as_ref() == Some(label) {
                    return *b;
                }
            }

            unreachable!()
        } else {
            return self.breaks.last().unwrap().0;
        }
    }

    fn create_iterator(&mut self, id: IterId, iter: PointerValue<'ctx>) {
        self.iterators.push((id, iter));
    }
    fn get_iterator(&self, id: IterId) -> PointerValue<'ctx> {
        self.iterators
            .iter()
            .rev()
            .find(|(d, _)| id.eq(d))
            .unwrap()
            .1
    }

    fn create_temp(&mut self, id: TempId) {
        let slot = self.builder.build_alloca(self.context.f64_type(), "alloca");
        self.temps.insert(id, slot);
    }
    fn get_temp(&self, id: TempId) -> PointerValue<'ctx> {
        *self.temps.get(&id).unwrap()
    }
    fn release_temp(&mut self, id: TempId) {
        self.temps.remove(&id);
    }

    fn push_error_handler(&mut self, block: BasicBlock<'ctx>) {
        self.error_handlers.push(block);
    }
    fn pop_error_handler(&mut self) {
        self.error_handlers.pop();
    }
    fn get_error_handler(&self) -> Option<BasicBlock<'ctx>> {
        self.error_handlers.last().copied()
    }

    fn create_arg_list(&mut self, id: ArgListId) {
        self.args.insert(id, Vec::new());
    }
    fn push_arg_list(&mut self, id: ArgListId, value: FloatValue<'ctx>) {
        let args = self.args.get_mut(&id).unwrap();
        args.push(value);
    }
    fn drop_arg_list(&mut self, id: ArgListId) {
        self.args.remove(&id);
    }

    fn declare_variable(&mut self, id: VariableId) {
        if let Some((pos, _)) = self.ir_function.captures.iter().find_position(|d| id.eq(d)) {
            return;
        }

        if self.ir_function.heap_variables.contains(&id) {
            // allocate heap variable
            let func = self
                .module
                .get_function("__nativejs_allocate_heap_variable")
                .unwrap();

            // call the allocate
            let re = self.builder.build_call(func, &[], "allocate_any");

            // get the pointer
            let ptr = re.try_as_basic_value().left().unwrap().into_pointer_value();

            // insert to map
            self.heap_variables.insert(id, ptr);
        } else {
            // allocate stack variable
            let ptr = self.builder.build_alloca(self.context.f64_type(), "alloca");

            // insert to map
            self.stack_variables.insert(id, ptr);
        };
    }
    
    fn drop_variable(&mut self, id: VariableId) {
        self.stack_variables.remove(&id);
        self.heap_variables.remove(&id);
    }

    fn get_variable_ptr(&self, id: VariableId) -> PointerValue<'ctx> {
        if let Some((pos, _)) = self.ir_function.captures.iter().find_position(|d| id.eq(d)) {

            // index
            let index = self.context.i32_type().const_int(pos as u64, false);

            // ptr: Rc<Any> = captures[index]
            let ptr = unsafe {
                self.builder.build_gep(
                    self.context.f64_type().ptr_type(Default::default()),
                    self.capture_stack,
                    &[index],
                    "gep",
                )
            };
            return ptr;
        }

        if let Some(ptr) = self.stack_variables.get(&id) {
            return *ptr;
        }

        return self.heap_variables.get(&id).copied().unwrap();
    }

    fn throw(&self, value: FloatValue<'ctx>) {
        // jump to handler if some
        if let Some(handler) = self.get_error_handler() {
            self.builder.build_store(self.error, value);

            self.builder.build_unconditional_branch(handler);

            let dummy = self.context.append_basic_block(self.function, "dummy");
            self.builder.position_at_end(dummy);
        } else {
            // call runtime throw if no handler registered
            let func = self.module.get_function("__nativejs_throw").unwrap();

            self.builder.build_call(func, &[value.into()], "throw");
        }
    }

    // await does nothing in non async context
    fn await_(&self, value: FloatValue<'ctx>) -> FloatValue<'ctx> {
        unimplemented!("await in non async function")
    }

    // yield does nothing in non generator context
    fn yield_(&self, value: FloatValue<'ctx>) -> FloatValue<'ctx> {
        unimplemented!("yield in non generator function")
    }

    fn call(
        &self,
        func: FunctionValue<'ctx>,
        args: &[inkwell::values::BasicValueEnum<'ctx>],
    ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
        // if a handler handler exist, catch exceptions with landingpad
        if let Some(handler) = self.get_error_handler() {
            // block when no error catched
            let then_block = self.context.append_basic_block(self.function, "then_block");
            // block when error is catched
            let catch_block = self
                .context
                .append_basic_block(self.function, "catch_block");

            // invoke the function
            let call_site =
                self.builder
                    .build_invoke(func, args, then_block, catch_block, "invoke");

            // switch to catch block
            self.builder.position_at_end(catch_block);

            {
                // get the personality function
                let personality = self
                    .module
                    .get_function("__nativejs_personality_routine")
                    .unwrap();

                // null is used as clause, catches any exception
                let null = self
                    .context
                    .i8_type()
                    .ptr_type(Default::default())
                    .const_null();

                // rust and c++ exception type
                let exception_ty = self.context.struct_type(
                    &[
                        self.context.i8_type().ptr_type(Default::default()).into(),
                        self.context.i32_type().into(),
                    ],
                    false,
                );

                // landing pad to catch exception
                let exception = self.builder.build_landing_pad(
                    exception_ty,
                    personality,
                    &[null.into()],
                    false,
                    "res",
                );

                // read exception from runtime specific exception
                let get_exception_func = self
                    .module
                    .get_function("__nativejs_read_exception")
                    .unwrap();

                let exception = self
                    .builder
                    .build_call(get_exception_func, &[exception.into()], "read_exception")
                    .try_as_basic_value()
                    .left()
                    .unwrap();

                // store exception to error slot
                self.builder.build_store(self.error, exception);

                // jump to the handler
                self.builder.build_unconditional_branch(handler);
            }

            // no error catched
            self.builder.position_at_end(then_block);

            {
                // get the call result
                let value = call_site.try_as_basic_value().left();

                return value;
            }

        } else {
            // translate BaseicValueEnum to BasicMatadataValueEnum
            let mut args_vec = Vec::new();

            for arg in args {
                let arg = match *arg {
                    BasicValueEnum::ArrayValue(a) => BasicMetadataValueEnum::ArrayValue(a),
                    BasicValueEnum::FloatValue(f) => BasicMetadataValueEnum::FloatValue(f),
                    BasicValueEnum::IntValue(i) => BasicMetadataValueEnum::IntValue(i),
                    BasicValueEnum::PointerValue(p) => BasicMetadataValueEnum::PointerValue(p),
                    BasicValueEnum::StructValue(s) => BasicMetadataValueEnum::StructValue(s),
                    BasicValueEnum::VectorValue(v) => BasicMetadataValueEnum::VectorValue(v),
                };
                args_vec.push(arg);
            }

            // call the function directly
            let call_site = self.builder.build_call(func, &args_vec, "call");

            // return result
            return call_site.try_as_basic_value().left();
        }
    }

    // return the function value
    fn finish(&mut self) -> FunctionValue<'ctx> {
        return self.function;
    }
}
