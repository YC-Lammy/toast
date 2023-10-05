use iron_gc::Array;
use iron_gc::GcPtr;

use crate::types::Any;
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
extern "C" fn __nativejs_throw(exception: Any){
    
}

#[no_mangle]
extern "C" fn __nativejs_enter_try(){

}

#[no_mangle]
extern "C" fn __nativejs_exit_try(){

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
extern "C" fn __nativejs_gc_alloc_any() -> iron_gc::GcPtr<Any>{
    return iron_gc::GcPtr::new(Any::UNDEFINED)
}

#[no_mangle]
extern "C" fn __nativejs_gc_write_any(mut ptr: iron_gc::GcPtr<Any>, value: Any){
    *ptr.as_mut() = value;
}

#[no_mangle]
extern "C" fn __nativejs_gc_read_any(ptr: iron_gc::GcPtr<Any>) -> Any{
    return ptr.copied()
}

#[no_mangle]
extern "C" fn __nativejs_gc_to_ptr_any(ptr: iron_gc::GcPtr<Any>) -> *mut Any{
    return ptr.as_mut_ptr()
}

#[no_mangle]
extern "C" fn __nativejs_gc_alloc_array_any(len: usize) -> GcPtr<Array<Any>>{
    return GcPtr::new_array_copied(Any::UNDEFINED, len)
}

#[no_mangle]
extern "C" fn __nativejs_gc_read_array_any(array: GcPtr<Array<Any>>, idx: usize) -> Any{
    return array.as_ref().get(idx).map(|a|*a).unwrap_or(Any::UNDEFINED)
}

#[no_mangle]
extern "C" fn __nativejs_call_async_func(func: *const (), data: GcPtr<Array<Any>>) -> Any{
    let promise = crate::asynchronous::spawn(
        crate::asynchronous::AsyncRoutine::new(func, data)
    );

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