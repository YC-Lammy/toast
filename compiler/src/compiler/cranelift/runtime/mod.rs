use std::ops::DerefMut;

use cranelift::codegen::ir::Function;
use cranelift::codegen::Context;
use cranelift::prelude::types;
use cranelift::prelude::AbiParam;
use cranelift::prelude::FunctionBuilder;
use cranelift::prelude::FunctionBuilderContext;
use cranelift::prelude::InstBuilder;
use cranelift::prelude::MemFlags;
use cranelift::prelude::Signature;
use cranelift_module::DataContext;
use cranelift_module::Linkage;
use cranelift_module::ModuleError;

use super::Compiler;

pub fn build_runtime(compiler: &mut Compiler) -> Result<(), ModuleError> {
    let mut func_ctx = FunctionBuilderContext::new();
    let mut data_ctx = DataContext::new();

    build_gc_mark(compiler, &mut func_ctx, &mut data_ctx)?;
    build_gc_malloc(compiler, &mut func_ctx, &mut data_ctx)?;

    Ok(())
}

fn build_gc_malloc(
    compiler: &mut Compiler,
    func_ctx: &mut FunctionBuilderContext,
    data_ctx: &mut DataContext,
) -> Result<(), ModuleError> {
    let pointer_type = types::Type::int(compiler.config.target_pointer_width).unwrap();
    let id = compiler.module.write().declare_function(
        "gc_malloc",
        cranelift_module::Linkage::Hidden,
        &Signature {
            params: vec![AbiParam::new(pointer_type)],
            returns: vec![AbiParam::new(pointer_type)],
            call_conv: compiler.module.read().target_config().default_call_conv,
        },
    )?;

    compiler.funcs.insert("gc_malloc".to_owned(), id);

    let mut ctx = compiler.module.read().make_context();
    ctx.func.signature.params.push(AbiParam::new(pointer_type));
    ctx.func.signature.returns.push(AbiParam::new(pointer_type));

    // create a function builder
    let mut builder = FunctionBuilder::new(&mut ctx.func, func_ctx);

    // initialize the builder
    let entry = builder.create_block();
    builder.append_block_params_for_function_params(entry);
    builder.switch_to_block(entry);

    // defined a static u64 value to store the number of times gc_malloc is called
    data_ctx.define_zeroinit(8);
    let counter_id =
        compiler
            .module
            .write()
            .declare_data("gc_malloc_count", Linkage::Hidden, true, false)?;

    data_ctx.clear();

    // store the pointer to the u64 counter
    data_ctx.define_zeroinit(8);
    let counter_ptr_id = compiler.module.write().declare_data(
        "gc_malloc_counter_ptr",
        Linkage::Hidden,
        true,
        false,
    )?;
    let counter = compiler
        .module
        .write()
        .declare_data_in_data(counter_id, data_ctx);
    data_ctx.write_data_addr(0, counter, 0);
    compiler
        .module
        .write()
        .define_data(counter_ptr_id, data_ctx)?;

    data_ctx.clear();

    // declare the counter pointer in function
    let counter_ptr = compiler
        .module
        .write()
        .declare_data_in_func(counter_ptr_id, &mut builder.func);

    // load the counter pointer
    let counter_ptr = builder.ins().global_value(pointer_type, counter_ptr);

    // load the current counter value
    let counter_value = builder
        .ins()
        .load(types::I64, MemFlags::new(), counter_ptr, 0);
    let added_counter_value = builder.ins().iadd_imm(counter_value, 1);

    // store the counter
    builder
        .ins()
        .store(MemFlags::new(), added_counter_value, counter_ptr, 0);

    if compiler.config.garbage_collect_multithread {
        // indicate the gc thread that a mark is required
    } else {
        // directly mark the roots if not multithread
        let gc_mark = compiler.funcs["gc_mark"];
        let gc_mark = compiler
            .module
            .write()
            .declare_func_in_func(gc_mark, &mut builder.func);
        builder.ins().call(gc_mark, &[]);
    };

    // finalize the function
    builder.finalize();

    // define the function
    let _re = compiler.module.write().define_function(id, &mut ctx)?;

    compiler.module.read().clear_context(&mut ctx);

    Ok(())
}

fn build_gc_mark(
    compiler: &mut Compiler,
    func_ctx: &mut FunctionBuilderContext,
    data_ctx: &mut DataContext,
) -> Result<(), ModuleError> {
    let pointer_type = types::Type::int(compiler.config.target_pointer_width).unwrap();
    let func_id = compiler.module.write().declare_function(
        "gc_mark",
        cranelift_module::Linkage::Hidden,
        &Signature {
            params: vec![],
            returns: vec![],
            call_conv: compiler.module.read().target_config().default_call_conv,
        },
    )?;

    compiler.funcs.insert("gc_mark".to_owned(), func_id);

    let mut ctx = compiler.module.read().make_context();

    let mut builder = FunctionBuilder::new(&mut ctx.func, func_ctx);

    // finalize the function
    builder.finalize();

    // define the function
    let _re = compiler.module.write().define_function(func_id, &mut ctx)?;

    compiler.module.read().clear_context(&mut ctx);

    Ok(())
}
