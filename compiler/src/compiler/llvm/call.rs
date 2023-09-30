use super::*;

impl<'ctx> FunctionBuilder<'ctx> {
    /// handles exceptions
    pub fn call_function(
        &self,
        function: FunctionValue<'ctx>,
        args: &[BasicMetadataValueEnum<'ctx>],
    ) -> BasicValueEnum<'ctx> {
        let context = self.module.get_context();

        if let Some(error_handler) = self.has_error_handler() {

            let re = self.builder.build_call(function, args, "call");

            let has_exception = self.module.get_function("RT_has_exception").unwrap();
            let has_exception = self
                .builder
                .build_call(has_exception, &[], "")
                .try_as_basic_value()
                .left()
                .unwrap()
                .into_int_value();

            let then_block = context.append_basic_block(self.function, "then_call_block");
            let catch_block = context.append_basic_block(self.function, "catch_call_block");

            self.builder
                .build_conditional_branch(has_exception, catch_block, then_block);

            {
                self.builder.position_at_end(catch_block);

                let get_exception = self.module.get_function("RT_get_exception").unwrap();
                let exception = self
                    .builder
                    .build_call(get_exception, &[], "get_exception")
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_float_value();
                self.write_acc(exception);

                self.builder.build_unconditional_branch(error_handler);
            }

            {
                self.builder.position_at_end(then_block);
            }

            return re.try_as_basic_value().left().unwrap();

        } else {
            let re = self.builder.build_direct_call(function, &args, "call");

            return re.try_as_basic_value().left().unwrap();
        }
    }

    pub fn compile_read_param(&self, index: usize) {
        // fn(this:Any, capture:*const *const Any, argc:u32, arg1:Any, arg2:Any, arg3:Any, args:...) -> Any;
        if index < 3 {
            // read directly from param
            let value = self
                .function
                .get_nth_param(3 + index as u32)
                .expect("failed to get param")
                .into_float_value();
            self.write_acc(value);
        } else {
            // read from va_list

            let has_param_block = self
                .module
                .get_context()
                .append_basic_block(self.function, "has_param_block");
            let default_param_block = self
                .module
                .get_context()
                .append_basic_block(self.function, "default_param_block");
            let exit_block = self
                .module
                .get_context()
                .append_basic_block(self.function, "exit_block");

            let argc = self
                .function
                .get_nth_param(2)
                .expect("failed to get argc")
                .into_int_value();
            let index = self
                .module
                .get_context()
                .i32_type()
                .const_int(index as u64, false);

            // argc > inex
            let has_param = self.builder.build_int_compare(
                inkwell::IntPredicate::UGT,
                argc,
                index,
                "argc_greater_then_index",
            );

            self.builder
                .build_conditional_branch(has_param, has_param_block, default_param_block);

            {
                self.builder.position_at_end(has_param_block);
                let va_list = self.function.get_last_param().unwrap().into_pointer_value();

                let value = self.builder.build_va_arg(
                    va_list,
                    self.module.get_context().f64_type(),
                    "va_arg",
                );
                self.write_acc(value.into_float_value());

                self.builder.build_unconditional_branch(exit_block);
            }

            {
                self.builder.position_at_end(default_param_block);

                self.write_acc(self.undefined());

                self.builder.build_unconditional_branch(exit_block);
            }

            self.builder.position_at_end(exit_block);
        }
    }

    pub fn compile_read_remaining_params(&self, mut starting_from: usize) {
        // fn(u32) -> Any
        let create_array = self.module.get_function("RT_create_array").unwrap();
        let re = self.builder.build_direct_call(
            create_array,
            &[self.module.get_context().i32_type().const_zero().into()],
            "",
        );
        let array = re.try_as_basic_value().left().unwrap().into_float_value();

        // fn(array:Any, value:Any)
        let push_array = self.module.get_function("RT_array_push").unwrap();

        while starting_from < 3 {
            let value = self
                .function
                .get_nth_param(3 + starting_from as u32)
                .unwrap()
                .into_float_value();

            self.builder
                .build_direct_call(push_array, &[array.into(), value.into()], "array_push");

            starting_from += 1;
        }

        let argc = self
            .function
            .get_nth_param(2)
            .expect("failed to get argc")
            .into_int_value();
        let index = self
            .builder
            .build_alloca(self.module.get_context().f64_type(), "index_count");

        let starting_from = self
            .module
            .get_context()
            .i32_type()
            .const_int(starting_from as u64, false);
        self.builder.build_store(index, starting_from);

        let body_block = self
            .module
            .get_context()
            .append_basic_block(self.function, "body_block");
        let exit_block = self
            .module
            .get_context()
            .append_basic_block(self.function, "exit_block");

        let next_index = self
            .builder
            .build_load(
                self.module.get_context().f64_type(),
                index,
                "get_next_index",
            )
            .into_int_value();
        let has_arg = self.builder.build_int_compare(
            inkwell::IntPredicate::UGT,
            argc,
            next_index,
            "has_remain_arg",
        );

        self.builder
            .build_conditional_branch(has_arg, body_block, exit_block);

        {
            self.builder.position_at_end(body_block);

            let va_list = self.function.get_last_param().unwrap().into_pointer_value();
            let value =
                self.builder
                    .build_va_arg(va_list, self.module.get_context().f64_type(), "va_arg");

            self.builder
                .build_direct_call(push_array, &[array.into(), value.into()], "array_push");

            // index += 1;
            let next_index = self
                .builder
                .build_load(
                    self.module.get_context().f64_type(),
                    index,
                    "get_next_index",
                )
                .into_int_value();
            let next_index = self.builder.build_int_add(
                next_index,
                self.module.get_context().i32_type().const_int(1, false),
                "add",
            );
            self.builder.build_store(index, next_index);

            // loop again if argc > next_index
            let has_arg = self.builder.build_int_compare(
                inkwell::IntPredicate::UGT,
                argc,
                next_index,
                "has_remain_arg",
            );
            self.builder
                .build_conditional_branch(has_arg, body_block, exit_block);
        }

        self.builder.position_at_end(exit_block);
    }

    pub fn compile_call(&self, arg_len: usize, args: ArgListId) {
        let arguments = self.get_args(args);
        let callee = self.read_acc();

        debug_assert!(arg_len == arguments.len());

        // fn(func: Any, this:Any, argc:i32, argv: *const Any) -> Any
        let rt_call = self.module.get_function("RT_call").unwrap();

        let argv = self.builder.build_alloca(self.context.f64_type().array_type(arg_len as u32), "argv");
        let argv = self.builder.build_pointer_cast(argv, self.context.f64_type().ptr_type(AddressSpace::default()), "ptr_cast");

        let mut i = 0;
        for arg in arguments{
            let ptr = unsafe{
                self.builder.build_gep(
                    self.context.f64_type(), 
                    argv, 
                    &[
                        self.context.i32_type().const_int(i, false)
                        ], 
                    "gep"
                )
            };
            self.builder.build_store(ptr, *arg);
            i += 1;
        }

        let args_v = vec![
            callee.into(),
            self.read_this().into(),
            self.module
                .get_context()
                .i32_type()
                .const_int(arg_len as u64, false)
                .into(),
            argv.into(),
        ];

        let re = self.call_function(rt_call, &args_v);
        self.write_acc(re.into_float_value());

        self.drop_arg_list(args);
    }

    pub fn create_capture_stack_for_func(&self, funcid: FunctionId) -> (PointerValue<'ctx>, usize) {
        let f = self.compiler.functions.get(&funcid).unwrap();

        let length = f.captures.len();

        let stack_ty = self
            .module
            .get_context()
            .f64_type()
            .ptr_type(AddressSpace::default())
            .array_type(length as u32);

        // return a null pointer if no capture
        if length == 0 {
            return (stack_ty.ptr_type(AddressSpace::default()).const_zero(), 0);
        }

        // allocate the capture stack on stack
        let capture_stack = self.builder.build_alloca(stack_ty, "alloca_capture_stack");

        // write the captures to the stack
        let mut i = 0;
        for v in &f.captures {
            // get the pointer of the capture
            let v_ptr = *self
                .variables
                .borrow()
                .get(v)
                .expect("failed to get variable for capture stack");

            // get the location on the capture stack
            let ptr_ptr_v = unsafe {
                self.builder.build_gep(
                    self.module
                        .get_context()
                        .f64_type()
                        .ptr_type(AddressSpace::default()),
                    capture_stack,
                    &[
                        self.context.i32_type().const_zero(),
                        self.module.get_context().i32_type().const_int(i, false)
                    ],
                    "gep",
                )
            };

            // store the pointer of capture to the capture stack
            self.builder.build_store(ptr_ptr_v, v_ptr);
            i += 1;
        }

        return (capture_stack, length);
    }

    pub fn compile_call_static(&self, funcid: FunctionId, arg_len: usize, args: ArgListId) {
        let arguments = self.get_args(args);
        debug_assert!(arg_len == arguments.len());

        let mut args = vec![
            self.read_this().into(),
            self.create_capture_stack_for_func(funcid).0.into(),
            self.context
                .i32_type()
                .const_int(arg_len as u64, false)
                .into(),
        ];

        args.extend(
            arguments
                .iter()
                .map(|v| BasicMetadataValueEnum::FloatValue(*v)),
        );

        while args.len() < 6 {
            args.push(self.undefined().into());
        }

        // fn(this:Any, capture:*const *const Any, argc:i32, arg1:Any, arg2:Any, arg3:Any, args:...) -> Any;
        let func = self.module.get_function(&funcid.to_string()).unwrap();

        let re = self.call_function(func, &args);

        self.write_acc(re.into_float_value())
    }

    pub fn compile_call_varargs(&self, va_args: VarArgId) {
        let callee = self.read_acc();
        let this = self.read_this();
        let (argc, argv) = self.get_varargs(va_args);

        // fn(callee:Any, this:Any, argc:u32, args:*const Any) -> Any;
        let func = self.module.get_function("RT_call_varargs").unwrap();

        let re = self.call_function(
            func,
            &[callee.into(), this.into(), argc.into(), argv.into()],
        );

        self.drop_varargs(va_args);

        self.write_acc(re.into_float_value());
    }

    pub fn compile_call_static_varargs(&self, funcid: FunctionId, va_args: VarArgId) {
        let callee = self.module.get_function(&funcid.to_string()).unwrap();

        // cast function as pointer
        let callee = self.builder.build_pointer_cast(
            callee.as_global_value().as_pointer_value(),
            self.module
                .get_context()
                .i8_type()
                .ptr_type(AddressSpace::default()),
            "pointer_cast",
        );
        let this = self.read_this();
        let (argc, argv) = self.get_varargs(va_args);

        // fn(callee:*const u8, capture_stack:*const *const Any, this:Any, argc:u32, args:*const Any) -> Any;
        let func = self.module.get_function("RT_call_static_varargs").unwrap();

        let re = self.call_function(
            func,
            &[
                callee.into(),
                self.create_capture_stack_for_func(funcid).0.into(),
                this.into(),
                argc.into(),
                argv.into(),
            ],
        );

        self.drop_varargs(va_args);

        self.write_acc(re.into_float_value());
    }

    pub fn compile_new_call(&self, args: ArgListId, arg_len: usize) {
        let arg_v = self.get_args(args);

        debug_assert!(arg_v.len() == arg_len);

        let args_v = self.builder.build_alloca(self.context.f64_type().array_type(arg_len as u32), "call_arg");
        let args_v = self.builder.build_pointer_cast(args_v, self.context.f64_type().ptr_type(AddressSpace::default()), "ptr_cast");
        
        let mut i = 0;
        for arg in arg_v{
            let ptr = unsafe{
                self.builder.build_gep(
                    self.context.f64_type(), 
                    args_v, 
                    &[self.context.i32_type().const_int(i, false)], 
                    "gep"
                )
            };
            self.builder.build_store(ptr, *arg);
            i += 1;
        }

        let ptr = self.builder.build_pointer_cast(args_v, self.context.f64_type().ptr_type(AddressSpace::default()), "ptr_cast");

        let new_call = self.module.get_function("RT_new_call").unwrap();

        let re = self.call_function(
            new_call, 
            &[
                self.read_acc().into(), 
                self.context.i32_type().const_int(arg_len as u64, false).into(),
                ptr.into()
                ]
            );

        self.write_acc(re.into_float_value());
        self.drop_arg_list(args);
    }
}
