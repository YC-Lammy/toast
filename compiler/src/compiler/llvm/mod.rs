use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::hash::Hasher;
use std::path::Path;
use std::sync::Arc;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::context::ContextRef;
use inkwell::intrinsics;
use inkwell::module::Linkage;
use inkwell::module::Module;
use inkwell::targets::TargetMachine;
use inkwell::values::*;
use inkwell::AddressSpace;

use swc_atoms::JsWord;

use crate::ir_builder::ir::*;
use crate::ir_builder::FunctionId;
use crate::ir_builder::IRFunction;
use crate::ir_builder::IRPackage;
use crate::ir_builder::VariableId;
use crate::Configuration;

mod async_function;
mod basic_function;
mod bin;
mod builder;
mod call;
mod compiler;
mod conditional;
mod loops;
mod object;

#[derive(Default)]
pub struct Compiler {
    functions: HashMap<FunctionId, Arc<IRFunction>>,
    external_functions: Vec<(JsWord, FunctionId)>,
}

pub struct VarArgList<'ctx> {
    /// *mut *mut u8
    ptr: PointerValue<'ctx>,
    /// *mut u32
    alloc_length: PointerValue<'ctx>,
    /// *mut u32
    length: PointerValue<'ctx>,
}

pub struct FunctionBuilder<'ctx> {
    compiler: Arc<Compiler>,
    ir_function: Arc<IRFunction>,

    context: ContextRef<'ctx>,
    module: Arc<Module<'ctx>>,
    builder: Builder<'ctx>,
    function: FunctionValue<'ctx>,

    acc: PointerValue<'ctx>,
    this: PointerValue<'ctx>,
    return_block: BasicBlock<'ctx>,
    /// *const *const Any
    capture_stack: PointerValue<'ctx>,

    /// body, label
    continues: RefCell<Vec<(BasicBlock<'ctx>, Option<JsWord>)>>,
    /// exit, label
    breaks: RefCell<Vec<(BasicBlock<'ctx>, Option<JsWord>)>>,

    iterators: RefCell<HashMap<IterId, PointerValue<'ctx>>>,
    temps: RefCell<HashMap<TempId, PointerValue<'ctx>>>,

    args: RefCell<HashMap<ArgListId, Vec<FloatValue<'ctx>>>>,
    varargs: RefCell<HashMap<VarArgId, VarArgList<'ctx>>>,

    /// stores the error
    error: PointerValue<'ctx>,
    error_handlers: RefCell<Vec<BasicBlock<'ctx>>>,

    /// variables and their corresponding location
    variables: RefCell<HashMap<VariableId, PointerValue<'ctx>>>,
}

/// 'compile' has to be a function to avoid lifetime dependency loop
pub fn compile(config: Configuration, pacakge: IRPackage) {
    let machine = initialise_llvm(&config);

    let context = Context::create();
    let module = context.create_module("");
    create_runtime(&context, &config, &module);

    let anyty = context.f64_type();
    let functy = anyty.fn_type(
        &[
            // this
            anyty.into(),
            // capture_stack
            context
                .f64_type()
                .ptr_type(AddressSpace::default())
                .ptr_type(AddressSpace::default())
                .into(),
            // argc
            context.i32_type().into(),
            // arg 1
            anyty.into(),
            // arg 2
            anyty.into(),
            // arg 3
            anyty.into(),
        ],
        true,
    );

    let mut functions = HashMap::new();

    for (id, f) in pacakge.functions {
        module.add_function(&id.to_string(), functy, None);
        functions.insert(id, Arc::new(f));
    }

    for (name, _id) in &pacakge.external_functions {
        module.add_function(name.as_ref(), functy, Some(Linkage::External));
    }

    let module = Arc::new(module);

    let compiler = Compiler {
        functions: functions,
        external_functions: pacakge.external_functions,
    };
    let compiler = Arc::new(compiler);

    for (funcid, func) in &compiler.functions {
        let funcid = *funcid;
        let func = func.clone();
        let module = module.clone();
        let compiler = compiler.clone();

        let work = move || {
            let mut builder = FunctionBuilder::new(funcid, module, compiler, func);
            builder.compile();
        };

        work();
    }

    let main_id = FunctionId::new();
    let main_func = module.add_function(&main_id.to_string(), functy, None);
    let mut main = FunctionBuilder::new(
        main_id,
        module.clone(),
        compiler.clone(),
        Arc::new(IRFunction {
            id: main_id,
            parent: None,

            is_arrow: false,
            is_async: false,
            is_generator: false,
            is_class_function: false,
            is_constructor: false,
            heap_variables: pacakge.heap_variables,
            own_variables: pacakge.global_variables,
            captures: Vec::new(),
            ir: pacakge.ir,
        }),
    );
    main.compile();

    {
        // create main function
        let main_entry = module.add_function("main", context.void_type().fn_type(&[], false), None);
        let builder = context.create_builder();
        let entry = context.append_basic_block(main_entry, "entry");
        builder.position_at_end(entry);

        // fn() -> Any
        let rt_entry = module.get_function("RT_entry").unwrap();
        let re = builder.build_call(rt_entry, &[], "rt_entry");

        // get the globalThis value
        let global_this = re.try_as_basic_value().left().unwrap().into_float_value();
        let undefined = context.f64_type().const_float(f64::from_bits(
            FunctionBuilder::MASK_UNDEFINED | FunctionBuilder::NAN_BITS,
        ));

        // call the main function
        builder.build_call(
            main_func,
            &[
                global_this.into(),
                context
                    .f64_type()
                    .ptr_type(AddressSpace::default())
                    .ptr_type(AddressSpace::default())
                    .const_zero()
                    .into(),
                context.i32_type().const_zero().into(),
                undefined.into(),
                undefined.into(),
                undefined.into(),
            ],
            "main",
        );

        let rt_exit = module.get_function("RT_exit").unwrap();
        builder.build_call(rt_exit, &[], "rt_exit");
        builder.build_return(None);
    }

    // verify module
    let re = module.verify();

    if let Err(e) = re {
        panic!("Internal error: {}", e.to_string_lossy());
    }

    module
        .print_to_file(&Path::new("foo.ll"))
        .expect("failed to write to file");
    module.write_bitcode_to_path(&Path::new("foo.bc"));

    let path = std::env::current_dir().unwrap();
    let path = path.join("output");
    let _ = std::fs::create_dir(&path);

    let path = path.join("jsbundle.o");
    if !path.exists() {
        std::fs::File::create(&path).expect("failed to open file");
    }

    machine
        .write_to_file(&module, inkwell::targets::FileType::Object, &path)
        .expect("failed to write to file");
}

fn create_runtime<'ctx>(context: &'ctx Context, _config: &Configuration, module: &Module<'ctx>) {
    let any_ty = context.f64_type();

    /*
    module.add_function(
        "__gxx_personality_v0",
        context.i64_type().fn_type(&[], false),
        Some(Linkage::External),
    );
    */

    let shadow_stack_elem = context.struct_type(
        &[
            context.f64_type().ptr_type(Default::default()).into(),
            context.i32_type().into(),
        ],
        true,
    );

    module.add_global(shadow_stack_elem.array_type(1024 * 8), None, "shadow_stack");

    let cursor = module.add_global(context.i32_type(), None, "shadow_stack_cursor");

    cursor.set_constant(false);
    cursor.set_initializer(&context.i32_type().const_zero());

    module.add_function(
        "RT_entry",
        any_ty.fn_type(&[], false),
        Some(Linkage::External),
    );

    module.add_function(
        "RT_exit",
        context.void_type().fn_type(&[], false),
        Some(Linkage::External),
    );

    module.add_function(
        "RT_has_exception",
        context.bool_type().fn_type(&[], false),
        Some(Linkage::External),
    );

    module.add_function(
        "RT_set_exception",
        context.void_type().fn_type(&[any_ty.into()], false),
        Some(Linkage::External),
    );

    module.add_function(
        "RT_get_exception",
        any_ty.fn_type(&[], false),
        Some(Linkage::External),
    );

    module.add_function(
        "RT_throw",
        context.void_type().fn_type(&[any_ty.into()], false),
        Some(Linkage::External),
    );

    module.add_function(
        "RT_enter_try",
        context.void_type().fn_type(&[], false),
        Some(Linkage::External),
    );

    module.add_function(
        "RT_exit_try",
        context.void_type().fn_type(&[], false),
        Some(Linkage::External),
    );

    module.add_function(
        "RT_gc_malloc",
        context
            .i8_type()
            .ptr_type(AddressSpace::default())
            .fn_type(&[context.i32_type().into()], false),
        Some(Linkage::External),
    );

    module.add_function(
        "RT_gc_malloc_uncollectable",
        context
            .i8_type()
            .ptr_type(AddressSpace::default())
            .fn_type(&[context.i32_type().into()], false),
        Some(Linkage::External),
    );

    module.add_function(
        "RT_rc_malloc",
        context
            .i8_type()
            .ptr_type(AddressSpace::default())
            .fn_type(&[context.i32_type().into()], false),
        Some(Linkage::External),
    );

    module.add_function(
        "RT_await",
        any_ty.fn_type(&[any_ty.into()], false),
        Some(Linkage::External),
    );

    module.add_function(
        "RT_yield",
        any_ty.fn_type(&[any_ty.into()], false),
        Some(Linkage::External),
    );

    module.add_function(
        "RT_create_for_in_iter",
        context
            .i8_type()
            .ptr_type(AddressSpace::default())
            .fn_type(&[any_ty.into()], false),
        Some(Linkage::External),
    );

    module.add_function(
        "RT_create_for_of_iter",
        context
            .i8_type()
            .ptr_type(AddressSpace::default())
            .fn_type(&[any_ty.into()], false),
        Some(Linkage::External),
    );

    module.add_function(
        "RT_create_async_iter",
        context
            .i8_type()
            .ptr_type(AddressSpace::default())
            .fn_type(&[any_ty.into()], false),
        Some(Linkage::External),
    );

    module.add_function(
        "RT_iter_next",
        any_ty.fn_type(
            &[context.i8_type().ptr_type(AddressSpace::default()).into()],
            false,
        ),
        Some(Linkage::External),
    );

    module.add_function(
        "RT_iter_done",
        context.bool_type().fn_type(
            &[context.i8_type().ptr_type(AddressSpace::default()).into()],
            false,
        ),
        Some(Linkage::External),
    );

    module.add_function(
        "RT_drop_iter",
        context.void_type().fn_type(
            &[context.i8_type().ptr_type(AddressSpace::default()).into()],
            false,
        ),
        Some(Linkage::External),
    );

    // fn(func: Any, this:Any, argc:i32, argv: *const Any) -> Any
    module.add_function(
        "RT_call",
        any_ty.fn_type(
            &[
                any_ty.into(),
                any_ty.into(),
                context.i32_type().into(),
                any_ty.ptr_type(AddressSpace::default()).into(),
            ],
            false,
        ),
        Some(Linkage::External),
    );

    // fn(callee:Any, this:Any, argc:u32, args:*const Any) -> Any;
    module.add_function(
        "RT_call_varargs",
        any_ty.fn_type(
            &[
                any_ty.into(),
                any_ty.into(),
                context.i32_type().into(),
                any_ty.ptr_type(AddressSpace::default()).into(),
            ],
            false,
        ),
        Some(Linkage::External),
    );

    // fn(callee:*const u8, capture_stack:*const *const Any, this:Any, argc:u32, args:*const Any) -> Any;
    module.add_function(
        "RT_call_static_vararg",
        any_ty.fn_type(
            &[
                context.i8_type().ptr_type(AddressSpace::default()).into(),
                any_ty
                    .ptr_type(AddressSpace::default())
                    .ptr_type(AddressSpace::default())
                    .into(),
                any_ty.into(),
                context.i32_type().into(),
                any_ty.ptr_type(AddressSpace::default()).into(),
            ],
            false,
        ),
        Some(Linkage::External),
    );

    module.add_function(
        "RT_new_call",
        any_ty.fn_type(
            &[
                any_ty.into(),
                context.i32_type().into(),
                any_ty.ptr_type(AddressSpace::default()).into(),
            ],
            false,
        ),
        Some(Linkage::External),
    );

    module.add_function(
        "RT_create_regexp",
        any_ty.fn_type(
            &[
                context.i8_type().ptr_type(AddressSpace::default()).into(),
                context.i32_type().into(),
                context.i8_type().ptr_type(AddressSpace::default()).into(),
                context.i32_type().into(),
            ],
            false,
        ),
        Some(Linkage::External),
    );

    // fn() -> Any
    module.add_function(
        "RT_create_array",
        any_ty.fn_type(&[context.i32_type().into()], false),
        Some(Linkage::External),
    );

    // fn(array:Any, value:Any)
    module.add_function(
        "RT_array_push",
        context
            .void_type()
            .fn_type(&[any_ty.into(), any_ty.into()], false),
        Some(Linkage::External),
    );

    // fn() -> Any
    module.add_function(
        "RT_create_object",
        any_ty.fn_type(&[], false),
        Some(Linkage::External),
    );

    // fn RT_object_get(object: Any, field:u64) -> Any
    module.add_function(
        "RT_object_get",
        any_ty.fn_type(&[any_ty.into(), context.i64_type().into()], false),
        Some(Linkage::External),
    );

    // fn RT_object_get_computed(object: Any, key:Any) -> Any
    module.add_function(
        "RT_object_get_computed",
        any_ty.fn_type(&[any_ty.into(), any_ty.into()], false),
        Some(Linkage::External),
    );

    // fn RT_object_get_index(object: Any, index:i32) -> Any
    module.add_function(
        "RT_object_get_index",
        any_ty.fn_type(&[any_ty.into(), context.i32_type().into()], false),
        Some(Linkage::External),
    );

    // fn RT_object_set(object: Any, field:u64, value: Any)
    module.add_function(
        "RT_object_set",
        context.void_type().fn_type(
            &[any_ty.into(), context.i64_type().into(), any_ty.into()],
            false,
        ),
        Some(Linkage::External),
    );

    // fn RT_object_set(object: Any, key:Any, value: Any)
    module.add_function(
        "RT_object_set_computed",
        context
            .void_type()
            .fn_type(&[any_ty.into(), any_ty.into(), any_ty.into()], false),
        Some(Linkage::External),
    );

    // fn RT_object_set_index(object: Any, index:i32, value: Any)
    module.add_function(
        "RT_object_set_index",
        context.void_type().fn_type(
            &[any_ty.into(), context.i32_type().into(), any_ty.into()],
            false,
        ),
        Some(Linkage::External),
    );
}

fn initialise_llvm(_config: &Configuration) -> TargetMachine {
    inkwell::targets::Target::initialize_native(&Default::default())
        .expect("failed to initialse LLVM");

    let default_triple = inkwell::targets::TargetMachine::get_default_triple();
    let default_cpu = inkwell::targets::TargetMachine::get_host_cpu_name();
    let default_features = inkwell::targets::TargetMachine::get_host_cpu_features();

    println!("target: {}", default_triple.as_str().to_string_lossy());
    println!("cpu: {}", default_cpu.to_string_lossy());
    println!("features: {}\n", default_features.to_string_lossy());

    let target = inkwell::targets::Target::from_triple(&default_triple);
    let target = match target {
        Ok(t) => t,
        Err(e) => {
            panic!("{}", e.to_string())
        }
    };

    let default_machine = target
        .create_target_machine(
            &default_triple,
            &default_cpu.to_string_lossy(),
            &default_features.to_string_lossy(),
            inkwell::OptimizationLevel::Aggressive,
            inkwell::targets::RelocMode::Default,
            inkwell::targets::CodeModel::Default,
        )
        .expect("failed to build target machine");

    return default_machine;

    /*
    match config.target_arch {
        "x86" | "x86_64" => inkwell::targets::Target::initialize_x86(&Default::default()),
        "mips" => inkwell::targets::Target::initialize_mips(&Default::default()),
        "powerpc" | "powerpc64" => {
            inkwell::targets::Target::initialize_power_pc(&Default::default())
        }
        "arm" => inkwell::targets::Target::initialize_arm(&Default::default()),
        "aarch64" => inkwell::targets::Target::initialize_aarch64(&Default::default()),
        "wasm32" | "wasm64" => {
            inkwell::targets::Target::initialize_webassembly(&Default::default())
        }
        "riscv" => inkwell::targets::Target::initialize_riscv(&Default::default()),
        "bpf" => inkwell::targets::Target::initialize_bpf(&Default::default()),
        _ => todo!("unsupported arch {}", config.target_arch),
    };

    let triple = inkwell::targets::TargetTriple::create(&format!(
        "{}-{}-{}",
        config.target_arch, config.target_vendor, config.target_os
    ));
    let target = inkwell::targets::Target::from_triple(&triple);
    let target = match target {
        Ok(t) => t,
        Err(e) => {
            panic!("{}", e.to_string())
        }
    };

    inkwell::targets::TargetMachine::get_default_triple();
    inkwell::targets::TargetMachine::get_host_cpu_name();
    inkwell::targets::TargetMachine::get_host_cpu_features();

    target
        .create_target_machine(
            &triple,
            "",
            "",
            inkwell::OptimizationLevel::Aggressive,
            inkwell::targets::RelocMode::Default,
            inkwell::targets::CodeModel::Default,
        )
        .expect("failed to build target machine")
    */
}

impl<'ctx> FunctionBuilder<'ctx> {
    pub fn new(
        funcid: FunctionId,
        module: Arc<Module<'ctx>>,
        compiler: Arc<Compiler>,
        irfunc: Arc<IRFunction>,
    ) -> Self {
        let context = module.get_context();
        let anyty = context.f64_type();

        let function = module.get_function(&funcid.to_string()).unwrap();
        let builder = context.create_builder();

        let entry = context.append_basic_block(function, "entry");
        builder.position_at_end(entry);

        let acc = builder.build_alloca(anyty, "ACC");
        let error = builder.build_alloca(anyty, "ERROR");
        let this = builder.build_alloca(anyty, "THIS");

        let this_value = function.get_first_param().unwrap();
        builder.build_store(this, this_value);

        let return_block = context.append_basic_block(function, "return_blcok");

        let capture_stack = function.get_nth_param(1).unwrap().into_pointer_value();

        return Self {
            compiler,
            ir_function: irfunc,

            context,
            module,
            builder,
            function,

            continues: Default::default(),
            breaks: Default::default(),

            iterators: Default::default(),
            temps: Default::default(),

            acc: acc,
            this: this,
            return_block: return_block,
            capture_stack: capture_stack,

            args: Default::default(),
            varargs: Default::default(),
            error: error,
            error_handlers: Default::default(),

            variables: Default::default(),
        };
    }

    fn compile(&mut self) {
        // read variables from the capture stack
        let mut i = 0;
        for v in &self.ir_function.captures {
            let elem_ptr = unsafe {
                self.builder.build_gep(
                    self.context.f64_type().ptr_type(AddressSpace::default()),
                    self.capture_stack,
                    &[self.context.i32_type().const_int(i, false)],
                    "capture_stack_gep",
                )
            };
            let var_ptr = self
                .builder
                .build_load(
                    self.context.f64_type().ptr_type(AddressSpace::default()),
                    elem_ptr,
                    "load_capture_var",
                )
                .into_pointer_value();

            self.variables.borrow_mut().insert(*v, var_ptr);

            i += 1;
        }

        // async and generator ir functions must not panic
        let mut async_error_handler = None;

        if self.ir_function.is_async || self.ir_function.is_generator {
            let error_handler = self
                .context
                .append_basic_block(self.function, "error_handler");
            async_error_handler = Some(error_handler);
            self.push_error_handler(error_handler);
        }

        /*
        // allocate variables
        if !self.ir_function.own_variables.is_empty(){
            let len = self.ir_function.own_variables.len();

            let ptr = self.builder.build_alloca(self.context.f64_type().array_type(len as u32), "alloca_var");

            let mut va = self.variables.borrow_mut();

            for i in 0..len{
                unsafe{
                    let p = self.builder.build_gep(self.context.f64_type(), ptr, &[self.context.i32_type().const_int(i as u64, false)], "gep");
                    let id = self.ir_function.own_variables[i];

                    // register the variable
                    va.insert(id, p);
                }
            }

            // store the pointer to the shadow stack
            self.push_shadow_stack(ptr, self.context.i32_type().const_int(len as u64, false));
        }
        */

        // compile ir code
        let mut cursor = 0;

        while cursor < self.ir_function.ir.len() {
            self.compile_ir(&self.ir_function.ir, &mut cursor);
        }

        // jump to return
        self.write_acc(self.undefined());
        self.builder.build_unconditional_branch(self.return_block);

        // if the function is async or generator
        if let Some(error_handler) = async_error_handler {
            self.builder.position_at_end(error_handler);

            // set the exception
            let error = self.read_acc();
            let set_exception = self.module.get_function("RT_set_exception").unwrap();
            self.builder
                .build_call(set_exception, &[error.into()], "set_exception");

            // return
            self.builder.build_unconditional_branch(self.return_block);
        }

        // return block
        self.builder.position_at_end(self.return_block);

        /*
        if !self.ir_function.own_variables.is_empty(){
            // pop the shadow stack
            self.pop_shadow_stack();
        }
        */

        let value = self.read_acc();
        self.builder.build_return(Some(&value));
    }

    fn push_shadow_stack(
        &self,
        value: PointerValue<'ctx>,
        length: IntValue<'ctx>,
    ) -> IntValue<'ctx> {
        let stack = self.module.get_global("shadow_stack").unwrap();
        let cursor = self.module.get_global("shadow_stack_cursor").unwrap();

        let re = self.builder.build_atomicrmw(
            inkwell::AtomicRMWBinOp::Add,
            cursor.as_pointer_value(),
            self.context.i32_type().const_int(1, false),
            inkwell::AtomicOrdering::SequentiallyConsistent,
        );

        let index = match re {
            Ok(i) => i,
            Err(e) => {
                panic!("{}", e);
            }
        };

        let stack_ptr = stack.as_pointer_value();

        unsafe {
            let shadow_stack_ptr = self.builder.build_gep(
                self.context.f64_type().array_type(1024 * 8),
                stack_ptr,
                &[self.context.i32_type().const_zero(), index],
                "shadow_stack_gep",
            );

            self.builder.build_store(shadow_stack_ptr, value);

            return index;
        }
    }

    fn pop_shadow_stack(&self) {
        let cursor = self.module.get_global("shadow_stack_cursor").unwrap();

        let re = self.builder.build_atomicrmw(
            inkwell::AtomicRMWBinOp::Sub,
            cursor.as_pointer_value(),
            self.context.i32_type().const_int(1, false),
            inkwell::AtomicOrdering::SequentiallyConsistent,
        );

        if let Err(e) = re {
            panic!("{}", e)
        }
    }

    fn increment_rc(&self, value: FloatValue<'ctx>) {}

    fn decrement_rc(&self, value: FloatValue<'ctx>) {}

    fn batch_decrement_rc(&self, ptr: PointerValue<'ctx>, length: IntValue<'ctx>) {}

    fn read_acc(&self) -> FloatValue<'ctx> {
        self.builder
            .build_load(self.context.f64_type(), self.acc, "load_acc")
            .into_float_value()
    }

    fn write_acc(&self, value: FloatValue<'ctx>) {
        let old = self
            .builder
            .build_load(self.context.f64_type(), self.acc, "desc_acc");
        self.decrement_rc(old.into_float_value());

        self.builder.build_store(self.acc, value);
        self.increment_rc(value);
    }

    fn read_this(&self) -> FloatValue<'ctx> {
        self.builder
            .build_load(self.module.get_context().f64_type(), self.this, "load_this")
            .into_float_value()
    }

    fn read_temp(&self, temp: TempId) -> FloatValue<'ctx> {
        let p = *self.temps.borrow().get(&temp).unwrap();
        let v = self
            .builder
            .build_load(self.module.get_context().f64_type(), p, "load_temp");
        return v.into_float_value();
    }

    fn write_temp(&self, temp: TempId, value: FloatValue<'ctx>) {
        let mut b = self.temps.borrow_mut();

        let p = if let Some(p) = b.get(&temp) {
            let p = *p;

            // read the old value and decrement count
            let old = self
                .builder
                .build_load(self.context.f64_type(), p, "desc_temp");
            self.decrement_rc(old.into_float_value());

            p
        } else {
            let p = self.builder.build_alloca(self.context.f64_type(), "alloca");
            b.insert(temp, p);
            p
        };

        self.builder.build_store(p, value);
        self.increment_rc(value);
    }

    fn drop_temp(&self, temp: TempId) {
        let mut temps = self.temps.borrow_mut();
        let p = temps.remove(&temp).unwrap();

        let value = self
            .builder
            .build_load(self.context.f64_type(), p, "destroy_temp");
        self.decrement_rc(value.into_float_value());
    }

    fn push_error_handler(&self, block: BasicBlock<'ctx>) {
        self.error_handlers.borrow_mut().push(block);
    }

    fn pop_error_handler(&self) {
        self.error_handlers
            .borrow_mut()
            .pop()
            .expect("pop error handler");
    }

    fn has_error_handler(&self) -> Option<BasicBlock<'ctx>> {
        self.error_handlers.borrow().last().map(|m| m.clone())
    }

    fn create_arglist(&self, id: ArgListId) {
        self.args.borrow_mut().insert(id, Vec::new());
    }

    fn push_arg(&self, id: ArgListId, value: FloatValue<'ctx>) {
        // increment rc
        //self.increment_rc(value);

        let mut list = self.args.borrow_mut();
        let list = list.get_mut(&id).expect("failed to get arglist");
        list.push(value);
    }

    fn get_args(&self, id: ArgListId) -> &[FloatValue<'ctx>] {
        let args = self.args.borrow();
        let args = args.get(&id).unwrap();
        return unsafe { std::slice::from_raw_parts(args.as_ptr(), args.len()) };
    }

    fn drop_arg_list(&self, id: ArgListId) {
        let list = self
            .args
            .borrow_mut()
            .remove(&id)
            .expect("failed to remove arg list");

        // decrement rc
        for _v in list {
            //self.decrement_rc(v);
        }
    }

    fn create_varargs(&self, id: VarArgId) {
        let counter = self
            .builder
            .build_alloca(self.context.i32_type(), "counter");
        self.builder
            .build_store(counter, self.context.i32_type().const_zero());

        // alloc_length = 32
        let alloc_length = self
            .builder
            .build_alloca(self.context.i32_type(), "alloc_length");
        self.builder.build_store(
            alloc_length,
            self.context.i32_type().const_int(16 * 8, false),
        );

        let alloc_func = self
            .module
            .get_function("RT_gc_malloc_uncollectable")
            .unwrap();

        let init_ptr = self.builder.build_call(
            alloc_func,
            &[self.context.i32_type().const_int(16 * 8, false).into()],
            "RT_gc_malloc_uncollectable",
        );

        let ptr = self.builder.build_pointer_cast(
            init_ptr
                .try_as_basic_value()
                .left()
                .unwrap()
                .into_pointer_value(),
            self.context.f64_type().ptr_type(AddressSpace::default()),
            "cast",
        );

        let ptr_buf = self.builder.build_alloca(
            self.context.f64_type().ptr_type(AddressSpace::default()),
            "ptr_buf",
        );

        self.builder.build_store(ptr_buf, ptr);

        self.varargs.borrow_mut().insert(
            id,
            VarArgList {
                length: counter,
                alloc_length: alloc_length,
                ptr: ptr_buf,
            },
        );
    }

    fn push_varargs(&self, id: VarArgId, value: FloatValue<'ctx>) {
        // increment rc
        self.increment_rc(value);

        let v_args = self.varargs.borrow();
        let v_arg = v_args.get(&id).unwrap();

        let length = self
            .builder
            .build_load(self.context.i32_type(), v_arg.length, "length")
            .into_int_value();

        let alloc_length = self
            .builder
            .build_load(self.context.i32_type(), v_arg.alloc_length, "allco_length")
            .into_int_value();

        let length_eq_alloc = self.builder.build_int_compare(
            inkwell::IntPredicate::EQ,
            length,
            alloc_length,
            "length_eq_alloc",
        );

        let realloc_block = self
            .context
            .append_basic_block(self.function, "realloc_block");
        let push_block = self
            .context
            .append_basic_block(self.function, "push_va_block");

        // realloc if length == alloc_length
        self.builder
            .build_conditional_branch(length_eq_alloc, realloc_block, push_block);

        // realloc the buffer
        {
            self.builder.position_at_end(realloc_block);

            let new_alloc_len = self.builder.build_int_mul(
                alloc_length,
                self.context.i32_type().const_int(2, false),
                "mul",
            );

            let alloc_func = self
                .module
                .get_function("RT_gc_malloc_uncollectable")
                .unwrap();

            let init_ptr = self.builder.build_call(
                alloc_func,
                &[new_alloc_len.into()],
                "RT_gc_malloc_uncollectable",
            );

            let new_buf = self.builder.build_pointer_cast(
                init_ptr
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_pointer_value(),
                self.context.f64_type().ptr_type(AddressSpace::default()),
                "cast",
            );

            let old_buf = self
                .builder
                .build_load(
                    self.context.f64_type().ptr_type(AddressSpace::default()),
                    v_arg.ptr,
                    "ptr",
                )
                .into_pointer_value();

            // free old_buf
            let free_func = self.module.get_function("RT_gc_dealloc").unwrap();
            self.builder.build_call(
                free_func,
                &[old_buf.into(), alloc_length.into()],
                "RT_gc_dealloc",
            );

            // store new alloc_length
            self.builder.build_store(v_arg.alloc_length, new_alloc_len);
            self.builder.build_store(v_arg.ptr, new_buf);

            // push the value
            self.builder.build_unconditional_branch(push_block);
        }

        // push the value to buffer
        {
            self.builder.position_at_end(push_block);

            // let buf = *mut u8
            let buf = self
                .builder
                .build_load(
                    self.context.f64_type().ptr_type(AddressSpace::default()),
                    v_arg.ptr,
                    "ptr",
                )
                .into_pointer_value();
            // let elem_ptr = buf[length];
            let elem_ptr = unsafe {
                self.builder
                    .build_gep(self.context.f64_type(), buf, &[length], "gep")
            };

            // *elem_ptr = value
            self.builder.build_store(elem_ptr, value);

            // length += 1;
            let new_len = self.builder.build_int_add(
                length,
                self.context.i32_type().const_int(1, false),
                "add",
            );
            self.builder.build_store(v_arg.length, new_len);
        }
    }

    fn get_varargs(&self, id: VarArgId) -> (IntValue<'ctx>, PointerValue<'ctx>) {
        let v_args = self.varargs.borrow();
        let v_arg = v_args.get(&id).unwrap();

        let length = self
            .builder
            .build_load(self.context.i32_type(), v_arg.length, "length");
        let ptr = self.builder.build_load(
            self.context.f64_type().ptr_type(AddressSpace::default()),
            v_arg.ptr,
            "ptr",
        );

        return (length.into_int_value(), ptr.into_pointer_value());
    }

    fn drop_varargs(&self, id: VarArgId) {
        let v_args = self.varargs.borrow();
        let v_arg = v_args.get(&id).unwrap();

        let alloc_length =
            self.builder
                .build_load(self.context.i32_type(), v_arg.alloc_length, "alloc_length");

        let length = self
            .builder
            .build_load(self.context.i32_type(), v_arg.length, "length");

        let ptr = self.builder.build_load(
            self.context.f64_type().ptr_type(AddressSpace::default()),
            v_arg.ptr,
            "ptr",
        );

        // batch decrement
        self.batch_decrement_rc(ptr.into_pointer_value(), length.into_int_value());

        let func = self.module.get_function("RT_gc_dealloc").unwrap();

        self.builder
            .build_call(func, &[ptr.into(), alloc_length.into()], "RT_gc_dealloc");
    }

    pub const NAN_BITS: u64 = 0b0111111111111000000000000000000000000000000000000000000000000000;

    pub const MASK_BOOL: u64 = 0b0000000000001001000000000000000000000000000000000000000000000000;
    pub const MASK_INT: u64 = 0b0000000000001010000000000000000000000000000000000000000000000000;
    pub const MASK_UNDEFINED: u64 =
        0b0000000000001011000000000000000000000000000000000000000000000000;
    pub const MASK_SYMBOL: u64 = 0b0000000000001100000000000000000000000000000000000000000000000000;
    pub const MASK_BIGINT: u64 = 0b0000000000001101000000000000000000000000000000000000000000000000;
    pub const MASK_OBJECT: u64 = 0b0000000000001110000000000000000000000000000000000000000000000000;
    pub const MASK_STRING: u64 = 0b0000000000001111000000000000000000000000000000000000000000000000;

    fn data_mask(&self) -> IntValue<'ctx> {
        self.module.get_context().i64_type().const_int(
            0b0000000000000000111111111111111111111111111111111111111111111111,
            false,
        )
    }

    fn tag_mask(&self) -> IntValue<'ctx> {
        self.module.get_context().i64_type().const_int(
            0b1111111111111111000000000000000000000000000000000000000000000000,
            false,
        )
    }

    fn undefined(&self) -> FloatValue<'ctx> {
        self.context
            .f64_type()
            .const_float(f64::from_bits(Self::MASK_UNDEFINED | Self::NAN_BITS))
    }

    fn is_true(&self, value: FloatValue<'ctx>) -> IntValue<'ctx> {
        let val = self
            .builder
            .build_bitcast(value, self.module.get_context().i64_type(), "cast")
            .into_int_value();
        let re = self.builder.build_and(val, self.data_mask(), "and");
        let re = self.builder.build_int_compare(
            inkwell::IntPredicate::NE,
            re,
            self.module.get_context().i64_type().const_zero(),
            "is_zero",
        );
        return re;
    }

    // compile until an ir occours
    fn compile_until(&self, ir: &[IR], cursor: &mut usize, until: IR) {
        loop {
            if (&ir[*cursor]).eq(&until) {
                *cursor += 1;
                break;
            }
            self.compile_ir(ir, cursor);
        }
    }

    fn compile_ir(&self, ir: &[IR], cursor: &mut usize) {
        if ir.len() <= *cursor {
            return;
        }

        let next = &ir[*cursor];
        *cursor += 1;

        match next {
            IR::Noop => {}
            IR::Debugger => {
                // does nothing
            }
            IR::ReadThis => {
                self.write_acc(self.read_this());
            }
            IR::ReadSuper => {
                assert!(self.ir_function.is_class_function);
                todo!()
            }
            IR::StoreTemp(t) => {
                let value = self.read_acc();
                self.write_temp(*t, value);
            }
            IR::LoadTemp(t) => {
                let value = self.read_temp(*t);
                self.write_acc(value);
            }
            IR::DropTemp(t) => {
                self.drop_temp(*t);
            }
            IR::DeclareVar(v) => {
                let mut va = self.variables.borrow_mut();

                if self.ir_function.heap_variables.contains(v) {
                    // fn(u32) -> *mut Any
                    let gc_malloc = self.module.get_function("RT_rc_malloc").unwrap();

                    let p = self.builder.build_call(
                        gc_malloc,
                        &[self.context.i32_type().const_int(8, false).into()],
                        "rc_malloc",
                    );

                    let ptr = p.try_as_basic_value().left().unwrap().into_pointer_value();

                    va.insert(*v, ptr);
                } else {
                    let ptr = self
                        .builder
                        .build_alloca(self.context.f64_type(), "alloca_variable");
                    va.insert(*v, ptr);
                };
            }
            IR::WriteVar(v) => {
                let value = self.read_acc();
                let ptr = *self
                    .variables
                    .borrow()
                    .get(v)
                    .expect(&format!("failed to get variable {}", v));

                // read the old value and decrement count
                let old = self
                    .builder
                    .build_load(self.context.f64_type(), ptr, "desc_var");
                self.decrement_rc(old.into_float_value());

                // increment rc and store the value
                self.increment_rc(value);
                self.builder.build_store(ptr, value);
            }
            IR::ReadVar(v) => {
                let ptr = *self
                    .variables
                    .borrow()
                    .get(v)
                    .expect(&format!("failed to get variable {}", v));
                let value = self.builder.build_load(
                    self.module.get_context().f64_type(),
                    ptr,
                    "load_variable",
                );
                self.write_acc(value.into_float_value());
            }
            IR::If => self.compile_if(ir, cursor),
            IR::IfElse => self.compile_if_else(ir, cursor),
            IR::EndIf | IR::EndElse => unreachable!(),

            IR::CreateForInIter(id) => self.create_for_in_iter(*id),
            IR::CreateForOfIter(id) => self.create_for_of_iter(*id),
            IR::CreateAsyncIter(id) => self.create_async_iter(*id),
            IR::IterNext(id) => self.iter_next(*id),
            IR::DropIterator(id) => self.drop_iter(*id),

            IR::Block { label } => {
                let exit = self.context.append_basic_block(self.function, "block_exit");

                self.breaks.borrow_mut().push((exit, Some(label.clone())));

                self.compile_until(ir, cursor, IR::EndBlock);

                self.breaks.borrow_mut().pop();

                self.builder.build_unconditional_branch(exit);
                self.builder.position_at_end(exit);
            }
            IR::EndBlock => unreachable!(),

            IR::Loop { label } => self.compile_loop(ir, cursor, label),
            IR::EndLoop => unreachable!(),

            IR::Break { label } => self.compile_break(label),
            IR::BreakIfFalse => self.compile_break_if_false(),
            IR::BreakIfIterDone(iter) => self.compile_break_if_iter_done(*iter),
            IR::Continue { label } => self.compile_continue(label),

            IR::TryCatch => {
                let try_block = self.context.append_basic_block(self.function, "try_blcok");
                let catch_block = self
                    .module
                    .get_context()
                    .append_basic_block(self.function, "catch_block");
                let exit_block = self
                    .module
                    .get_context()
                    .append_basic_block(self.function, "exit_block");

                self.builder.build_unconditional_branch(try_block);

                self.push_error_handler(catch_block);

                //////////////// try block /////////////////////////
                self.builder.position_at_end(try_block);

                let enter_try = self.module.get_function("RT_enter_try").unwrap();
                self.builder.build_call(enter_try, &[], "enter_try");

                // try body
                self.compile_until(ir, cursor, IR::EndTry);

                // exit try
                let exit_try = self.module.get_function("RT_exit_try").unwrap();
                self.builder.build_call(exit_try, &[], "exit_try");

                // jump to exit
                self.builder.build_unconditional_branch(exit_block);

                self.pop_error_handler();

                //////// catch block ////////////
                self.builder.position_at_end(catch_block);

                // exit try
                let exit_try = self.module.get_function("RT_exit_try").unwrap();
                self.builder.build_call(exit_try, &[], "exit_try");

                // load the error into ACC
                let value = self
                    .builder
                    .build_load(
                        self.module.get_context().f64_type(),
                        self.error,
                        "load_exception",
                    )
                    .into_float_value();
                self.write_acc(value);

                self.compile_until(ir, cursor, IR::EndCatch);

                // jump to exit
                self.builder.build_unconditional_branch(exit_block);

                /////////////// exit block ////////////////////////////
                self.builder.position_at_end(exit_block);

                self.compile_until(ir, cursor, IR::EndTryCatchFinalizer);
            }

            IR::EndTry | IR::EndCatch | IR::EndTryCatchFinalizer => unreachable!(),

            IR::Throw => {
                let error = self.read_acc();

                if let Some(handler) = self.has_error_handler() {
                    let dummy_block = self
                        .context
                        .append_basic_block(self.function, "dummy_block");

                    self.builder.build_store(self.error, error);
                    self.builder.build_conditional_branch(
                        self.context.bool_type().const_int(1, false),
                        handler,
                        dummy_block,
                    );

                    self.builder.position_at_end(dummy_block);
                } else {
                    let func = self.module.get_function("RT_throw").unwrap();
                    self.builder
                        .build_direct_call(func, &[error.into()], "throw");

                    self.builder.build_unconditional_branch(self.return_block);
                }
            }
            IR::CreateArgList(id) => {
                self.create_arglist(*id);
            }
            IR::PushArg(id) => {
                let value = self.read_acc();
                self.push_arg(*id, value);
            }
            IR::CreateVarArgList(id) => {
                self.create_varargs(*id);
            }
            IR::PushVarArg(id) => {
                let value = self.read_acc();
                self.push_varargs(*id, value);
            }
            IR::Return => {
                self.builder.build_unconditional_branch(self.return_block);

                // dummy blocks were never executed
                let dummy = self.context.append_basic_block(self.function, "dummy");
                self.builder.position_at_end(dummy);
            }
            IR::ReadParam(index) => self.compile_read_param(*index),
            IR::ReadRemainParams { starting_from } => {
                self.compile_read_remaining_params(*starting_from)
            }
            IR::Call {
                arg_len,
                args,
                maybe_static,
            } => {
                // maybe static calls should already be resolved
                debug_assert!(maybe_static.is_none());
                self.compile_call(*arg_len, *args);
            }
            IR::CallStatic {
                func_id,
                arg_len,
                args,
            } => self.compile_call_static(*func_id, *arg_len, *args),
            IR::CallVarArgs { var_arg } => self.compile_call_varargs(*var_arg),
            IR::CallStaticVarArgs { func_id, var_arg } => {
                self.compile_call_static_varargs(*func_id, *var_arg)
            }
            IR::New { arg_len, args } => self.compile_new_call(*args, *arg_len),
            IR::Await => {
                let promise = self.read_acc();

                let await_func = self.module.get_function("RT_await").unwrap();
                let re = self
                    .builder
                    .build_call(await_func, &[promise.into()], "rt_await");

                // write result to acc
                self.write_acc(re.try_as_basic_value().left().unwrap().into_float_value());

                // check if there is any error
                let then_block = self.context.append_basic_block(self.function, "then_block");
                let error_block = self
                    .context
                    .append_basic_block(self.function, "error_block");

                let has_exception = self.module.get_function("RT_has_exception").unwrap();
                let has_exception = self
                    .builder
                    .build_call(has_exception, &[], "has_exception")
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_int_value();

                // branch to error handling
                self.builder
                    .build_conditional_branch(has_exception, error_block, then_block);

                // return
                self.builder.position_at_end(error_block);
                self.builder.build_unconditional_branch(self.return_block);

                // continue
                self.builder.position_at_end(then_block);
            }
            IR::Yield => {
                let value = self.read_acc();

                let rt_yield = self.module.get_function("RT_yield").unwrap();
                let re = self
                    .builder
                    .build_call(rt_yield, &[value.into()], "rt_yield");

                self.write_acc(re.try_as_basic_value().left().unwrap().into_float_value());
            }

            IR::LoadBigInt(b) => {}
            IR::LoadBool(b) => self.write_acc(
                self.context
                    .f64_type()
                    .const_float(f64::from_bits(Self::NAN_BITS | Self::MASK_BOOL | *b as u64)),
            ),
            IR::LoadInt(i) => self.write_acc(self.context.f64_type().const_float(*i as f64)),
            IR::LoadNull => self.write_acc(
                self.context
                    .f64_type()
                    .const_float(f64::from_bits(Self::NAN_BITS | Self::MASK_OBJECT)),
            ),
            IR::LoadNumber(n) => self.write_acc(self.context.f64_type().const_float(*n)),
            IR::LoadString(s) => {
                let length: [u8; 8] = unsafe { std::mem::transmute(s.len() as u64) };

                let mut encoded = length.to_vec();
                encoded.extend_from_slice(s.as_bytes());

                let s = self.context.const_string(&encoded, true);
                let g = self.module.add_global(s.get_type(), None, "global_string");

                g.set_initializer(&s);

                let ptr = g.as_pointer_value();
                let ptr_i = ptr.const_to_int(self.context.i64_type());
                let boxed_value = ptr_i.const_or(
                    self.context
                        .i64_type()
                        .const_int(Self::MASK_STRING | Self::NAN_BITS, false),
                );

                let boxed_value =
                    self.builder
                        .build_bitcast(boxed_value, self.context.f64_type(), "bitcast");

                self.write_acc(boxed_value.into_float_value());
            }
            IR::LoadUndefined => self.write_acc(self.undefined()),
            IR::LoadTpl {
                quasis,
                args_len,
                args,
            } => {}
            IR::CreateRegex { pattern, flag } => {
                let p_len = pattern.len();
                let f_len = flag.len();

                let pattern = self.context.const_string(pattern.as_bytes(), true);
                let flag = self.context.const_string(flag.as_bytes(), true);

                let pattern_gl = self
                    .module
                    .add_global(pattern.get_type(), None, "regexpattern");
                let flag_gl = self.module.add_global(flag.get_type(), None, "regexflags");

                pattern_gl.set_initializer(&pattern);
                flag_gl.set_initializer(&flag);

                let create_regex = self.module.get_function("RT_create_regexp").unwrap();

                let re = self.call_function(
                    create_regex,
                    &[
                        pattern_gl.as_pointer_value().into(),
                        self.context
                            .i32_type()
                            .const_int(p_len as u64, false)
                            .into(),
                        flag_gl.as_pointer_value().into(),
                        self.context
                            .i32_type()
                            .const_int(f_len as u64, false)
                            .into(),
                    ],
                );

                self.write_acc(re.into_float_value());
            }
            IR::CreateObject => self.compile_create_object(),
            IR::CreateArray { size } => self.compile_create_array(*size),
            IR::ArrayPush { array } => self.compile_array_push(*array),
            IR::ReadComputed { obj } => {
                let key = self.read_acc();
                let obj = self.read_temp(*obj);

                // fn(Any, Any) -> Any
                let func = self.module.get_function("RT_object_get_computed").unwrap();

                let re = self.builder.build_call(
                    func,
                    &[key.into(), obj.into()],
                    "RT_object_get_computed",
                );

                self.write_acc(re.try_as_basic_value().left().unwrap().into_float_value());
            }
            IR::ReadField { key } => {
                let obj = self.read_acc();

                let hash: u64 = cityhasher::hash(key.as_bytes());

                // fn(Any, u64) -> Any
                let func = self.module.get_function("RT_object_get").unwrap();

                let re = self.builder.build_call(
                    func,
                    &[
                        obj.into(),
                        self.context.i64_type().const_int(hash, false).into(),
                    ],
                    "RT_object_get",
                );

                self.write_acc(re.try_as_basic_value().left().unwrap().into_float_value());
            }
            IR::ReadIndex { index } => {
                let obj = self.read_acc();
            }
            IR::WriteComputed { obj, propname } => {
                let obj = self.read_temp(*obj);
                let key = self.read_temp(*propname);
                let value = self.read_acc();

                // fn(Any, Any, Any)
                let func = self.module.get_function("RT_object_set_computed").unwrap();
            }
            IR::WriteField { object, key } => {
                let hashkey = cityhasher::hash(key.as_bytes());

                let obj = self.read_temp(*object);
                let value = self.read_acc();

                let func = self.module.get_function("RT_object_set").unwrap();

                self.builder.build_call(
                    func,
                    &[
                        obj.into(),
                        self.context.i64_type().const_int(hashkey, false).into(),
                        value.into(),
                    ],
                    "RT_object_set",
                );
            }
            IR::WriteIndex { object, index } => {}
            IR::ObjAssign => {}
            IR::ObjCall { args, arg_len } => {}
            IR::ObjCallVarArg { var_arg } => {}

            IR::InitFunction(id) => {
                let func = self.module.get_function(&id.to_string()).unwrap();

                let func_ptr = func.as_global_value().as_pointer_value();
            }

            IR::EqEq(lhs) => {
                let rhs = self.read_acc();
                let lhs = self.read_temp(*lhs);

                let float_block = self
                    .context
                    .append_basic_block(self.function, "float_eqeq_block");
                let slow_block = self
                    .context
                    .append_basic_block(self.function, "slow_eqeq_block");
                let end_block = self
                    .context
                    .append_basic_block(self.function, "exit_eqeq_block");

                // determind if both values are numbers
                let is_number = self.builder.build_float_compare(
                    inkwell::FloatPredicate::ORD,
                    lhs,
                    rhs,
                    "float_oeq",
                );

                self.builder
                    .build_conditional_branch(is_number, float_block, slow_block);

                {
                    // fast path that compares two number directly
                    self.builder.position_at_end(float_block);

                    let is_eq = self.builder.build_float_compare(
                        inkwell::FloatPredicate::OEQ,
                        lhs,
                        rhs,
                        "eqeq",
                    );

                    let t = self
                        .context
                        .f64_type()
                        .const_float(f64::from_bits(Self::MASK_BOOL | Self::NAN_BITS | 0x01));
                    let f = self
                        .context
                        .f64_type()
                        .const_float(f64::from_bits(Self::MASK_BOOL | Self::NAN_BITS));

                    let re = self.builder.build_select(is_eq, t, f, "select");
                    self.write_acc(re.into_float_value());

                    self.builder.build_unconditional_branch(end_block);
                }

                {
                    // the slow path which calls the runtime function
                    self.builder.position_at_end(slow_block);

                    let eqeq = self.module.get_function("RT_eqeq").unwrap();
                    let re = self
                        .builder
                        .build_call(eqeq, &[rhs.into(), lhs.into()], "rt_eqeq");
                    self.write_acc(re.try_as_basic_value().left().unwrap().into_float_value());

                    self.builder.build_unconditional_branch(end_block);
                }

                self.builder.position_at_end(end_block);
            }
            IR::EqEqEq(lhs) => {
                let rhs = self.read_acc();
                let lhs = self.read_temp(*lhs);
                let is_eq = self.builder.build_float_compare(
                    inkwell::FloatPredicate::OEQ,
                    lhs,
                    rhs,
                    "eqeqeq",
                );

                let t = self
                    .context
                    .f64_type()
                    .const_float(f64::from_bits(Self::MASK_BOOL | Self::NAN_BITS | 0x1));
                let f = self
                    .context
                    .f64_type()
                    .const_float(f64::from_bits(Self::MASK_BOOL | Self::NAN_BITS));

                let re = self.builder.build_select(is_eq, t, f, "select");
                self.write_acc(re.into_float_value());
            }
            IR::NotEq(lhs) => {
                let rhs = self.read_acc();
                let lhs = self.read_temp(*lhs);

                let float_block = self
                    .context
                    .append_basic_block(self.function, "float_eqeq_block");
                let slow_block = self
                    .context
                    .append_basic_block(self.function, "slow_eqeq_block");
                let end_block = self
                    .context
                    .append_basic_block(self.function, "exit_eqeq_block");

                // determind if both values are numbers
                let is_number = self.builder.build_float_compare(
                    inkwell::FloatPredicate::ORD,
                    lhs,
                    rhs,
                    "float_oeq",
                );

                self.builder
                    .build_conditional_branch(is_number, float_block, slow_block);

                {
                    // fast path that compares two number directly
                    self.builder.position_at_end(float_block);

                    let is_eq = self.builder.build_float_compare(
                        inkwell::FloatPredicate::OEQ,
                        lhs,
                        rhs,
                        "eqeq",
                    );

                    let t = self
                        .context
                        .f64_type()
                        .const_float(f64::from_bits(Self::MASK_BOOL | Self::NAN_BITS | 0x01));
                    let f = self
                        .context
                        .f64_type()
                        .const_float(f64::from_bits(Self::MASK_BOOL | Self::NAN_BITS));

                    let re = self.builder.build_select(is_eq, f, t, "select");
                    self.write_acc(re.into_float_value());

                    self.builder.build_unconditional_branch(end_block);
                }

                {
                    // the slow path which calls the runtime function
                    self.builder.position_at_end(slow_block);

                    let eqeq = self.module.get_function("RT_not_eqeq").unwrap();
                    let re = self
                        .builder
                        .build_call(eqeq, &[rhs.into(), lhs.into()], "rt_eqeq");
                    self.write_acc(re.try_as_basic_value().left().unwrap().into_float_value());

                    self.builder.build_unconditional_branch(end_block);
                }

                self.builder.position_at_end(end_block);
            }
            IR::NotEqEq(lhs) => {
                let rhs = self.read_acc();
                let lhs = self.read_temp(*lhs);
                let is_eq = self.builder.build_float_compare(
                    inkwell::FloatPredicate::OEQ,
                    lhs,
                    rhs,
                    "eqeqeq",
                );

                let t = self
                    .context
                    .f64_type()
                    .const_float(f64::from_bits(Self::MASK_BOOL | Self::NAN_BITS | 0x1));
                let f = self
                    .context
                    .f64_type()
                    .const_float(f64::from_bits(Self::MASK_BOOL | Self::NAN_BITS));

                let re = self.builder.build_select(is_eq, f, t, "select");
                self.write_acc(re.into_float_value());
            }
            IR::Add(lhs) => {
                let rhs = self.read_acc();
                let lhs = self.read_temp(*lhs);

                let float_block = self
                    .context
                    .append_basic_block(self.function, "float_add_block");
                let slow_block = self
                    .context
                    .append_basic_block(self.function, "slow_add_block");
                let end_block = self
                    .context
                    .append_basic_block(self.function, "exit_add_block");

                // determind if both values are numbers
                let is_number = self.builder.build_float_compare(
                    inkwell::FloatPredicate::ORD,
                    lhs,
                    rhs,
                    "float_oeq",
                );

                self.builder
                    .build_conditional_branch(is_number, float_block, slow_block);

                {
                    // fast path that compares two number directly
                    self.builder.position_at_end(float_block);

                    let re = self.builder.build_float_add(lhs, rhs, "add");

                    self.write_acc(re);

                    self.builder.build_unconditional_branch(end_block);
                }

                {
                    // the slow path which calls the runtime function
                    self.builder.position_at_end(slow_block);

                    let add = self.module.get_function("RT_add").unwrap();
                    let re = self
                        .builder
                        .build_call(add, &[rhs.into(), lhs.into()], "rt_add");
                    self.write_acc(re.try_as_basic_value().left().unwrap().into_float_value());

                    self.builder.build_unconditional_branch(end_block);
                }

                self.builder.position_at_end(end_block);
            }
            IR::And(lhs) => {
                let rhs = self.read_acc();
                let lhs = self.read_temp(*lhs);

                let lhs_is_ture = self.is_true(lhs);
                let rhs_is_true = self.is_true(rhs);

                let is_ture = self.builder.build_and(lhs_is_ture, rhs_is_true, "and");

                let t = self
                    .context
                    .f64_type()
                    .const_float(f64::from_bits(Self::MASK_BOOL | Self::NAN_BITS | 0x1));
                let f = self
                    .context
                    .f64_type()
                    .const_float(f64::from_bits(Self::MASK_BOOL | Self::NAN_BITS));

                let re = self.builder.build_select(is_ture, t, f, "and");
                self.write_acc(re.into_float_value());
            }
            IR::BitAnd(lhs) => {
                let lhs = self.read_temp(*lhs);
                let rhs = self.read_acc();

                let lhs =
                    self.builder
                        .build_float_to_signed_int(lhs, self.context.i32_type(), "fti");
                let rhs =
                    self.builder
                        .build_float_to_signed_int(rhs, self.context.i32_type(), "fti");

                let re = self.builder.build_and(lhs, rhs, "bitand");
                let re = self
                    .builder
                    .build_signed_int_to_float(re, self.context.f64_type(), "itf");
                self.write_acc(re);
            }
            IR::BitOr(lhs) => {
                let lhs = self.read_temp(*lhs);
                let rhs = self.read_acc();

                let lhs =
                    self.builder
                        .build_float_to_signed_int(lhs, self.context.i32_type(), "fti");
                let rhs =
                    self.builder
                        .build_float_to_signed_int(rhs, self.context.i32_type(), "fti");

                let re = self.builder.build_or(lhs, rhs, "bitor");
                let re = self
                    .builder
                    .build_signed_int_to_float(re, self.context.f64_type(), "itf");
                self.write_acc(re);
            }
            IR::BitXor(lhs) => {
                let lhs = self.read_temp(*lhs);
                let rhs = self.read_acc();

                let lhs =
                    self.builder
                        .build_float_to_signed_int(lhs, self.context.i32_type(), "fti");
                let rhs =
                    self.builder
                        .build_float_to_signed_int(rhs, self.context.i32_type(), "fti");

                let re = self.builder.build_xor(lhs, rhs, "bitxor");
                let re = self
                    .builder
                    .build_signed_int_to_float(re, self.context.f64_type(), "itf");
                self.write_acc(re);
            }
            IR::Div(lhs) => {
                let lhs = self.read_temp(*lhs);
                let rhs = self.read_acc();

                let re = self.builder.build_float_div(lhs, rhs, "div");
                self.write_acc(re);
            }
            IR::Exp(lhs) => {
                let lhs = self.read_temp(*lhs);
                let rhs = self.read_acc();

                let f64_ty = self.context.f64_type();
                let func = intrinsics::Intrinsic::find("llvm.pow").unwrap();
                let pow = func
                    .get_declaration(&self.module, &[f64_ty.into(), f64_ty.into()])
                    .unwrap();

                let re = self
                    .builder
                    .build_call(pow, &[lhs.into(), rhs.into()], "pow");
                self.write_acc(re.try_as_basic_value().left().unwrap().into_float_value())
            }
            IR::Gt(lhs) => {
                let rhs = self.read_acc();
                let lhs = self.read_temp(*lhs);

                let float_block = self
                    .context
                    .append_basic_block(self.function, "float_gt_block");
                let slow_block = self
                    .context
                    .append_basic_block(self.function, "slow_gt_block");
                let end_block = self
                    .context
                    .append_basic_block(self.function, "exit_gt_block");

                // determind if both values are numbers
                let is_number = self.builder.build_float_compare(
                    inkwell::FloatPredicate::ORD,
                    lhs,
                    rhs,
                    "float_oeq",
                );

                self.builder
                    .build_conditional_branch(is_number, float_block, slow_block);

                {
                    // fast path that compares two number directly
                    self.builder.position_at_end(float_block);

                    let is_eq = self.builder.build_float_compare(
                        inkwell::FloatPredicate::OGT,
                        lhs,
                        rhs,
                        "gt",
                    );

                    let t = self
                        .context
                        .f64_type()
                        .const_float(f64::from_bits(Self::MASK_BOOL | Self::NAN_BITS | 0x01));
                    let f = self
                        .context
                        .f64_type()
                        .const_float(f64::from_bits(Self::MASK_BOOL | Self::NAN_BITS));

                    let re = self.builder.build_select(is_eq, t, f, "select");
                    self.write_acc(re.into_float_value());

                    self.builder.build_unconditional_branch(end_block);
                }

                {
                    // the slow path which calls the runtime function
                    self.builder.position_at_end(slow_block);

                    let eqeq = self.module.get_function("RT_gt").unwrap();
                    let re = self
                        .builder
                        .build_call(eqeq, &[rhs.into(), lhs.into()], "rt_gt");
                    self.write_acc(re.try_as_basic_value().left().unwrap().into_float_value());

                    self.builder.build_unconditional_branch(end_block);
                }

                self.builder.position_at_end(end_block);
            }
            IR::GtEq(lhs) => {
                let rhs = self.read_acc();
                let lhs = self.read_temp(*lhs);

                let float_block = self
                    .context
                    .append_basic_block(self.function, "float_gteq_block");
                let slow_block = self
                    .context
                    .append_basic_block(self.function, "slow_gteq_block");
                let end_block = self
                    .context
                    .append_basic_block(self.function, "exit_gteq_block");

                // determind if both values are numbers
                let is_number = self.builder.build_float_compare(
                    inkwell::FloatPredicate::ORD,
                    lhs,
                    rhs,
                    "float_oeq",
                );

                self.builder
                    .build_conditional_branch(is_number, float_block, slow_block);

                {
                    // fast path that compares two number directly
                    self.builder.position_at_end(float_block);

                    let is_eq = self.builder.build_float_compare(
                        inkwell::FloatPredicate::OGE,
                        lhs,
                        rhs,
                        "gteq",
                    );

                    let t = self
                        .context
                        .f64_type()
                        .const_float(f64::from_bits(Self::MASK_BOOL | Self::NAN_BITS | 0x01));
                    let f = self
                        .context
                        .f64_type()
                        .const_float(f64::from_bits(Self::MASK_BOOL | Self::NAN_BITS));

                    let re = self.builder.build_select(is_eq, t, f, "select");
                    self.write_acc(re.into_float_value());

                    self.builder.build_unconditional_branch(end_block);
                }

                {
                    // the slow path which calls the runtime function
                    self.builder.position_at_end(slow_block);

                    let eqeq = self.module.get_function("RT_gteq").unwrap();
                    let re = self
                        .builder
                        .build_call(eqeq, &[rhs.into(), lhs.into()], "rt_gteq");
                    self.write_acc(re.try_as_basic_value().left().unwrap().into_float_value());

                    self.builder.build_unconditional_branch(end_block);
                }

                self.builder.position_at_end(end_block);
            }
            IR::Lt(lhs) => {
                let rhs = self.read_acc();
                let lhs = self.read_temp(*lhs);

                let float_block = self
                    .context
                    .append_basic_block(self.function, "float_lt_block");
                let slow_block = self
                    .context
                    .append_basic_block(self.function, "slow_lt_block");
                let end_block = self
                    .context
                    .append_basic_block(self.function, "exit_lt_block");

                // determind if both values are numbers
                let is_number = self.builder.build_float_compare(
                    inkwell::FloatPredicate::ORD,
                    lhs,
                    rhs,
                    "float_oeq",
                );

                self.builder
                    .build_conditional_branch(is_number, float_block, slow_block);

                {
                    // fast path that compares two number directly
                    self.builder.position_at_end(float_block);

                    let is_eq = self.builder.build_float_compare(
                        inkwell::FloatPredicate::OLT,
                        lhs,
                        rhs,
                        "lt",
                    );

                    let t = self
                        .context
                        .f64_type()
                        .const_float(f64::from_bits(Self::MASK_BOOL | Self::NAN_BITS | 0x01));
                    let f = self
                        .context
                        .f64_type()
                        .const_float(f64::from_bits(Self::MASK_BOOL | Self::NAN_BITS));

                    let re = self.builder.build_select(is_eq, t, f, "select");
                    self.write_acc(re.into_float_value());

                    self.builder.build_unconditional_branch(end_block);
                }

                {
                    // the slow path which calls the runtime function
                    self.builder.position_at_end(slow_block);

                    let eqeq = self.module.get_function("RT_lt").unwrap();
                    let re = self
                        .builder
                        .build_call(eqeq, &[rhs.into(), lhs.into()], "rt_lt");
                    self.write_acc(re.try_as_basic_value().left().unwrap().into_float_value());

                    self.builder.build_unconditional_branch(end_block);
                }

                self.builder.position_at_end(end_block);
            }
            IR::LtEq(lhs) => {
                let rhs = self.read_acc();
                let lhs = self.read_temp(*lhs);

                let float_block = self
                    .context
                    .append_basic_block(self.function, "float_lteq_block");
                let slow_block = self
                    .context
                    .append_basic_block(self.function, "slow_lteq_block");
                let end_block = self
                    .context
                    .append_basic_block(self.function, "exit_lteq_block");

                // determind if both values are numbers
                let is_number = self.builder.build_float_compare(
                    inkwell::FloatPredicate::ORD,
                    lhs,
                    rhs,
                    "float_oeq",
                );

                self.builder
                    .build_conditional_branch(is_number, float_block, slow_block);

                {
                    // fast path that compares two number directly
                    self.builder.position_at_end(float_block);

                    let is_eq = self.builder.build_float_compare(
                        inkwell::FloatPredicate::OLE,
                        lhs,
                        rhs,
                        "lteq",
                    );

                    let t = self
                        .context
                        .f64_type()
                        .const_float(f64::from_bits(Self::MASK_BOOL | Self::NAN_BITS | 0x01));
                    let f = self
                        .context
                        .f64_type()
                        .const_float(f64::from_bits(Self::MASK_BOOL | Self::NAN_BITS));

                    let re = self.builder.build_select(is_eq, t, f, "select");
                    self.write_acc(re.into_float_value());

                    self.builder.build_unconditional_branch(end_block);
                }

                {
                    // the slow path which calls the runtime function
                    self.builder.position_at_end(slow_block);

                    let eqeq = self.module.get_function("RT_lteq").unwrap();
                    let re = self
                        .builder
                        .build_call(eqeq, &[rhs.into(), lhs.into()], "rt_lteq");
                    self.write_acc(re.try_as_basic_value().left().unwrap().into_float_value());

                    self.builder.build_unconditional_branch(end_block);
                }

                self.builder.position_at_end(end_block);
            }
            IR::Mul(lhs) => {
                let rhs = self.read_acc();
                let lhs = self.read_temp(*lhs);

                let re = self.builder.build_float_mul(lhs, rhs, "mul");
                self.write_acc(re);
            }
            IR::Sub(lhs) => {
                let rhs = self.read_acc();
                let lhs = self.read_temp(*lhs);

                let re = self.builder.build_float_sub(lhs, rhs, "sub");
                self.write_acc(re);
            }
            IR::Mod(lhs) => {
                let rhs = self.read_acc();
                let lhs = self.read_temp(*lhs);

                let re = self.builder.build_float_rem(lhs, rhs, "rem");
                self.write_acc(re);
            }
            IR::LShift(lhs) => {
                let lhs = self.read_temp(*lhs);
                let rhs = self.read_acc();

                let lhs =
                    self.builder
                        .build_float_to_signed_int(lhs, self.context.i32_type(), "fti");
                let rhs =
                    self.builder
                        .build_float_to_signed_int(rhs, self.context.i32_type(), "fti");

                let re = self.builder.build_left_shift(lhs, rhs, "lshift");
                let re = self
                    .builder
                    .build_signed_int_to_float(re, self.context.f64_type(), "itf");
                self.write_acc(re);
            }
            IR::RShift(lhs) => {
                let lhs = self.read_temp(*lhs);
                let rhs = self.read_acc();

                let lhs =
                    self.builder
                        .build_float_to_signed_int(lhs, self.context.i32_type(), "fti");
                let rhs =
                    self.builder
                        .build_float_to_signed_int(rhs, self.context.i32_type(), "fti");

                let re = self.builder.build_right_shift(lhs, rhs, true, "rshift");
                let re = self
                    .builder
                    .build_signed_int_to_float(re, self.context.f64_type(), "itf");
                self.write_acc(re);
            }
            IR::ZeroFillRShift(lhs) => {
                let lhs = self.read_temp(*lhs);
                let rhs = self.read_acc();

                let lhs =
                    self.builder
                        .build_float_to_signed_int(lhs, self.context.i32_type(), "fti");
                let rhs =
                    self.builder
                        .build_float_to_signed_int(rhs, self.context.i32_type(), "fti");

                let re = self
                    .builder
                    .build_right_shift(lhs, rhs, false, "unsigned_lshift");
                let re = self
                    .builder
                    .build_signed_int_to_float(re, self.context.f64_type(), "itf");
                self.write_acc(re);
            }
            IR::Or(lhs) => {
                let rhs = self.read_acc();
                let lhs = self.read_temp(*lhs);

                let l_is_true = self.is_true(lhs);
                let re = self.builder.build_select(l_is_true, lhs, rhs, "or");
                self.write_acc(re.into_float_value())
            }
            IR::Nullish(lhs) => {
                let a = self.read_temp(*lhs);
                let b = self.read_acc();

                let lhs = self
                    .builder
                    .build_bitcast(a, self.context.i64_type(), "cast")
                    .into_int_value();

                let undefined = self
                    .context
                    .i64_type()
                    .const_int(Self::MASK_UNDEFINED | Self::NAN_BITS, false);
                let is_undefined =
                    self.builder
                        .build_int_compare(inkwell::IntPredicate::EQ, lhs, undefined, "eq");

                let null = self
                    .context
                    .i64_type()
                    .const_int(Self::MASK_OBJECT | Self::NAN_BITS, false);
                let is_null =
                    self.builder
                        .build_int_compare(inkwell::IntPredicate::EQ, lhs, null, "eq");

                let is_nullish = self.builder.build_or(is_null, is_undefined, "and");

                let re = self.builder.build_select(is_nullish, b, a, "nullish");
                self.write_acc(re.into_float_value());
            }
            IR::NonUndefine(lhs) => {
                let a = self.read_temp(*lhs);
                let b = self.read_acc();

                let lhs = self
                    .builder
                    .build_bitcast(a, self.context.i64_type(), "cast")
                    .into_int_value();
                let undefined = self
                    .context
                    .i64_type()
                    .const_int(Self::MASK_UNDEFINED | Self::NAN_BITS, false);
                let is_undefined =
                    self.builder
                        .build_int_compare(inkwell::IntPredicate::EQ, lhs, undefined, "eq");

                let re = self
                    .builder
                    .build_select(is_undefined, b, a, "non_undefined");
                self.write_acc(re.into_float_value());
            }
            IR::In(lhs) => {}
            IR::InstanceOf(lhs) => {}

            IR::Bang => {
                let value = self.read_acc();
                let is_true = self.is_true(value);

                let t = self
                    .context
                    .f64_type()
                    .const_float(f64::from_bits(Self::MASK_BOOL | Self::NAN_BITS | 0x1));
                let f = self
                    .context
                    .f64_type()
                    .const_float(f64::from_bits(Self::MASK_BOOL | Self::NAN_BITS));

                let re = self.builder.build_select(is_true, f, t, "bang");
                self.write_acc(re.into_float_value());
            }
            IR::Delete => {
                // not supported, always returns false
                let _value = self.read_acc();

                let false_value = self
                    .context
                    .f64_type()
                    .const_float(f64::from_bits(Self::MASK_BOOL | Self::NAN_BITS));

                self.write_acc(false_value);
            }
            IR::Minus => {
                let value = self.read_acc();

                let fast_block = self.context.append_basic_block(self.function, "fast_block");
                let slow_block = self.context.append_basic_block(self.function, "slow_block");
                let exit_block = self.context.append_basic_block(self.function, "exit_block");

                // check if value is number
                let is_number = self.builder.build_float_compare(
                    inkwell::FloatPredicate::ORD,
                    value,
                    value,
                    "ord",
                );

                // if the value is number, do nothing
                self.builder
                    .build_conditional_branch(is_number, fast_block, slow_block);

                {
                    self.builder.position_at_end(fast_block);

                    let re = self.builder.build_float_neg(value, "neg");
                    self.write_acc(re);

                    self.builder.build_unconditional_branch(exit_block);
                }

                {
                    // convert the value into a number
                    self.builder.position_at_end(slow_block);

                    let to_num = self.module.get_function("RT_to_number").unwrap();
                    let re = self.builder.build_call(to_num, &[value.into()], "to_num");

                    let re = re.try_as_basic_value().left().unwrap().into_float_value();
                    let re = self.builder.build_float_neg(re, "neg");

                    self.write_acc(re);

                    self.builder.build_unconditional_branch(exit_block);
                }

                self.builder.position_at_end(exit_block);
            }
            IR::Plus => {
                let value = self.read_acc();

                let slow_block = self.context.append_basic_block(self.function, "slow_block");
                let exit_block = self.context.append_basic_block(self.function, "exit_block");

                // check if value is number
                let is_number = self.builder.build_float_compare(
                    inkwell::FloatPredicate::ORD,
                    value,
                    value,
                    "ord",
                );

                // if the value is number, do nothing
                self.builder
                    .build_conditional_branch(is_number, exit_block, slow_block);

                {
                    // convert the value into a number
                    self.builder.position_at_end(slow_block);

                    let to_num = self.module.get_function("RT_to_number").unwrap();
                    let re = self.builder.build_call(to_num, &[value.into()], "to_num");
                    self.write_acc(re.try_as_basic_value().left().unwrap().into_float_value());

                    self.builder.build_unconditional_branch(exit_block);
                }

                self.builder.position_at_end(exit_block);
            }
            IR::Tilde => {
                let value = self.read_acc();

                let v =
                    self.builder
                        .build_float_to_signed_int(value, self.context.i32_type(), "fti");

                let re = self.builder.build_not(v, "bitnot");
                let re = self
                    .builder
                    .build_signed_int_to_float(re, self.context.f64_type(), "itf");

                self.write_acc(re);
            }
            IR::TypeOf => {
                let value = self.read_acc();

                // cast value to u64
                let v = self
                    .builder
                    .build_float_to_signed_int(value, self.context.i64_type(), "");
                let tag = self.builder.build_and(v, self.tag_mask(), "");

                let number_block = self
                    .context
                    .append_basic_block(self.function, "number_block");

                let types = [
                    (Self::MASK_BIGINT | Self::NAN_BITS, "bigint"),
                    (Self::MASK_BOOL | Self::NAN_BITS, "boolean"),
                    (Self::MASK_INT | Self::NAN_BITS, "number"),
                    (Self::MASK_OBJECT | Self::NAN_BITS, "object"),
                    (Self::MASK_STRING | Self::NAN_BITS, "string"),
                    (Self::MASK_SYMBOL | Self::NAN_BITS, "symbol"),
                    (Self::MASK_UNDEFINED | Self::NAN_BITS, "undefined"),
                ];

                self.builder.build_switch(tag, number_block, &[]);
            }
        }
    }
}
