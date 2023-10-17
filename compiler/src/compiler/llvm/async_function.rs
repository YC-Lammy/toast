use std::collections::HashMap;
use std::sync::Arc;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::ContextRef;
use inkwell::module::Module;
use inkwell::types::IntType;
use inkwell::values::{FloatValue, FunctionValue, PointerValue};
use itertools::Itertools;
use swc_atoms::JsWord;

use crate::ir_builder::ir::{ArgListId, IterId, TempId};
use crate::ir_builder::{IRFunction, VariableId};

pub struct AsyncBuilder<'ctx> {
    ir_function: Arc<IRFunction>,

    context: ContextRef<'ctx>,
    module: Arc<Module<'ctx>>,
    builder: Builder<'ctx>,
    function: FunctionValue<'ctx>,

    size_ty: IntType<'ctx>,

    acc: PointerValue<'ctx>,
    this: PointerValue<'ctx>,
    return_block: BasicBlock<'ctx>,

    /// *const [HeapVariable]
    capture_stack: PointerValue<'ctx>,
    /// *const [Any]
    variable_stack: PointerValue<'ctx>,
    /// *const [HeapVariable]
    heap_variable_stack: PointerValue<'ctx>,

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
    stack_variables: Vec<VariableId>,
    /// pointer to the HeapVariable
    heap_variables: Vec<VariableId>,
}

impl<'ctx> super::builder::FunctionBuilder<'ctx> for AsyncBuilder<'ctx> {
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
        if self.ir_function.captures.contains(&id) {
            return;
        }

        if self.ir_function.heap_variables.contains(&id) {
            self.heap_variables.push(id);
        } else {
            self.stack_variables.push(id);
        };
    }

    fn drop_variable(&mut self, id: VariableId) {}

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

        if let Some((idx, _)) = self.stack_variables.iter().find_position(|v| **v == id) {
            unsafe {
                let ptr = self.builder.build_gep(
                    self.context.f64_type(),
                    self.variable_stack,
                    &[self.context.i32_type().const_int(idx as _, false).into()],
                    "gep",
                );

                return ptr;
            }
        }

        if let Some((idx, _)) = self.heap_variables.iter().find_position(|v| **v == id) {
            unsafe {
                let ptr = self.builder.build_gep(
                    self.context.f64_type(),
                    self.heap_variable_stack,
                    &[self.context.i32_type().const_int(idx as u64, false).into()],
                    "gep",
                );

                return ptr;
            }
        }

        unreachable!()
    }

    fn throw(&self, value: FloatValue<'ctx>) {
        if let Some(handler) = self.get_error_handler() {
            self.builder.build_store(self.error, value);

            self.builder.build_unconditional_branch(handler);

            let dummy = self.context.append_basic_block(self.function, "dummy");
            self.builder.position_at_end(dummy);
        } else {
            let func = self.module.get_function("__nativejs_throw").unwrap();

            self.builder.build_call(func, &[value.into()], "throw");
        }
    }

    fn await_(&self, value: FloatValue<'ctx>) -> FloatValue<'ctx> {
        unimplemented!("await in non async function")
    }

    fn yield_(&self, value: FloatValue<'ctx>) -> FloatValue<'ctx> {
        unimplemented!("yield in non generator function")
    }

    fn call(
        &self,
        func: FunctionValue<'ctx>,
        args: &[inkwell::values::BasicValueEnum<'ctx>],
    ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
        let then_block = self.context.append_basic_block(self.function, "then_block");
        let catch_block = self
            .context
            .append_basic_block(self.function, "catch_block");

        let call_site = self
            .builder
            .build_invoke(func, args, then_block, catch_block, "invoke");

        self.builder.position_at_end(catch_block);

        {
            let personality = self
                .module
                .get_function("__nativejs_personality_routine")
                .unwrap();

            let null = self
                .context
                .i8_type()
                .ptr_type(Default::default())
                .const_null();

            let exception_ty = self.context.struct_type(
                &[
                    self.context.i8_type().ptr_type(Default::default()).into(),
                    self.context.i32_type().into(),
                ],
                false,
            );

            let exception = self.builder.build_landing_pad(
                exception_ty,
                personality,
                &[null.into()],
                false,
                "res",
            );

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

            self.builder.build_store(self.error, exception);

            if let Some(handler) = self.error_handlers.last() {
                self.builder.build_unconditional_branch(*handler);
            } else {
                self.builder.build_unconditional_branch(self.return_block);
            }
        }

        self.builder.position_at_end(then_block);

        {
            let value = call_site.try_as_basic_value().left();

            return value;
        }
    }

    fn finish(&mut self) -> FunctionValue<'ctx> {
        let f64_ty = self.context.f64_type();
        let ty = f64_ty.fn_type(
            &[
                // any
                f64_ty.into(),
                // captures
                f64_ty
                    .ptr_type(Default::default())
                    .ptr_type(Default::default())
                    .into(),
                // argc
                self.context.i32_type().into(),
                // arg1
                f64_ty.into(),
                // arg2
                f64_ty.into(),
                // arg3
                f64_ty.into(),
            ],
            true,
        );
        let func = self.module.add_function(
            &self.ir_function.id.to_string(),
            ty,
            Some(inkwell::module::Linkage::Private),
        );

        let block = self.context.append_basic_block(func, "entry");

        self.builder.position_at_end(block);

        {
            let i32_ty = self.context.i32_type();

            let func_ptr = self.function.as_global_value().as_pointer_value();
            let this = func.get_nth_param(0).unwrap();
            let capture_stack = func.get_nth_param(1).unwrap();

            let capture_len = self.ir_function.captures.len();
            let heap_var_len = self.ir_function.heap_variables.len();
            let stack_var_len = self.ir_function.own_variables.len() - heap_var_len;

            let call_async = self.module.get_function("__nativejs_call_async").unwrap();

            let call_site = self.builder.build_call(
                call_async,
                &[
                    func_ptr.into(),
                    this.into(),
                    capture_stack.into(),
                    i32_ty.const_int(capture_len as u64, false).into(),
                    i32_ty.const_int(stack_var_len as u64, false).into(),
                    i32_ty.const_int(heap_var_len as u64, false).into(),
                ],
                "call_async",
            );

            let promise = call_site.try_as_basic_value().left().unwrap();
            self.builder.build_return(Some(&promise));
        }

        return func;
    }
}
