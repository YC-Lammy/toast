
use alloc::boxed::Box;

use crate::types::Any;
use crate::types::HeapVariable;
use crate::types::Object;

#[no_mangle]
extern "C" fn __nativejs_initialise(){
    iron_gc::initialise();
}

#[no_mangle]
extern "C" fn __nativejs_exit(){
    iron_gc::destroy();
}

#[no_mangle]
extern "C" fn __nativejs_get_global_object(){

}


#[no_mangle]
extern "C" fn __nativejs_print(value: Any){
    unsafe{
        libc::printf(value.to_string().to_c_str())
    };
}

#[no_mangle]
extern "C" fn __nativejs_throw(exception: Any) -> !{
    crate::unwinding::throw(exception);
}

#[no_mangle]
extern "C" fn __nativejs_enter_try(){

}

#[no_mangle]
extern "C" fn __nativejs_exit_try(){

}

#[no_mangle]
unsafe extern "C" fn __nativejs_read_exception(exception: *mut u8) -> Any{
    return crate::unwinding::read_exception(exception)
}

#[no_mangle]
unsafe extern "C" fn __nativejs_personality_routine(
    version: libc::c_int,
    actions: unwinding::abi::UnwindAction,
    exception_class: u64,
    exception: *mut unwinding::abi::UnwindException,
    unwind_ctx: &mut unwinding::abi::UnwindContext<'_>,
) -> unwinding::abi::UnwindReasonCode{
    
    return crate::unwinding::rust_eh_personality(version, actions, exception_class, exception, unwind_ctx)
}

#[no_mangle]
extern "C" fn __nativejs_gc_alloc_heap_variable() -> HeapVariable{
    return HeapVariable::new(Any::UNDEFINED)
}

#[no_mangle]
unsafe extern "C" fn __nativejs_call_async_func(func: *const (), this: Any, captures: *mut HeapVariable, capture_len: u32, var_len: u32, heap_var_len: u32) -> Any{
    let captures = Box::from_raw(core::slice::from_raw_parts_mut(captures, capture_len as usize));

    let promise = crate::asynchronous::spawn(func, this, var_len, captures);

    return Object::new_promise(promise).into()
}

#[no_mangle]
extern "C" fn __nativejs_create_object() -> Any{
    return Object::new().into()
}

#[no_mangle]
extern "C" fn __nativejs_object_get(obj: Any, key: Any) -> Any{
    return obj.get_property(key.to_string()).unwrap_or(Any::UNDEFINED)
}

#[no_mangle]
extern "C" fn __nativejs_object_set(obj: Any, key: Any, value: Any){
    if let Some(obj) = obj.as_object(){
        obj.set_property(key.to_string(), value);
    }
}

#[no_mangle]
unsafe extern "C" fn __nativejs_object_call(func: Any, this: Any, argv: *const Any, argc: u32) -> Any{
    let args = core::slice::from_raw_parts(argv, argc as usize);
    return func.call(this, args)
}

#[no_mangle]
unsafe extern "C" fn __nativejs_create_regex(src: *const libc::c_char, flags: *const libc::c_char) -> Any{
    let slen = libc::strlen(src);
    let flen = libc::strlen(flags);

    let src = core::str::from_utf8_unchecked(core::slice::from_raw_parts(src as *const u8, slen));
    let flags = core::str::from_utf8_unchecked(core::slice::from_raw_parts(flags as *const u8, flen));

    let reg = crate::types::regex::Regexp::new(src, flags);

    match reg{
        Ok(reg) => {
            return Object::new_regex(reg).into()
        }
        Err(e) => {
            __nativejs_throw(Any::error(e.as_str()))
        }
    }
}