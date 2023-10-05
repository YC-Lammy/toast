
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
extern "C" fn __nativejs_throw(exception: Any) -> !{
    unreachable!()
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
extern "C" fn __nativejs_gc_write_array_any(mut array: GcPtr<Array<Any>>, idx: usize, value: Any){
    if let Some(ptr) = array.get_mut(idx){
        *ptr = value;
    }
}

#[no_mangle]
extern "C" fn __nativejs_gc_alloc_capture_stack(len: usize) -> GcPtr<Array<GcPtr<Any>>>{
    let ptr = GcPtr::new(Any::UNDEFINED);
    let ptr = GcPtr::new_array_copied(ptr, len);

    return ptr
}

#[no_mangle]
extern "C" fn __nativejs_gc_read_capture_stack_elem_ptr(ptr: GcPtr<Array<GcPtr<Any>>>, idx: usize) -> *mut Any{
    return ptr.as_ref().get(idx).unwrap().as_mut_ptr()
}

#[no_mangle]
extern "C" fn __nativejs_call_async_func(func: *const (), data: GcPtr<Array<Any>>, this: Any, captures: GcPtr<Array<GcPtr<Any>>>) -> Any{
    let promise = crate::asynchronous::spawn(func, data);

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
            __nativejs_throw(Any::error(&e.text))
        }
    }
}