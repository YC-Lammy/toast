/*
 * Copyright 2023 YC. Lam. All rights reserved.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

// Export functions provided by this library

use core::ops::{AddAssign, SubAssign};

use parking_lot::RwLock;
use rusty_ts_macro::hash;

use minicoroutine::{Coroutine, CoroutineRef};
use crate::gc::{GcPtr, self};
use crate::types::JSValue;
use crate::types::async_function::{AwaitOrYield, ASYNC_FUTURES};
use crate::types::async_generator::ASYNC_GENERATORS;
use crate::types::{Any, Object, ObjectValue, JSString};

pub type JSFunc =
    unsafe extern "C" fn(this: Any, captures:*const *const Any, argc: u32, arg1: Any, arg2: Any, arg3: Any, args: ...) -> Any;

lazy_static::lazy_static!{
    pub static ref GLOBAL_THIS:Object = {
        let mut global = Object::new();

        global.set_property(JSString::Constant("NaN"), Any::NAN);

        global
    };
}

/// initialise runtime and return the globalThis value
#[no_mangle]
pub unsafe extern "C" fn RT_entry() -> Any {
    gc::init();
    return GLOBAL_THIS.to_any();
}

/// execute async functions and cleanup runtime
#[no_mangle]
pub unsafe extern "C" fn RT_exit(){
    libc::exit(0);
}

#[no_mangle]
pub unsafe extern "C" fn RT_print(value:Any){
    //println!("{}", value.to_string());
    let s = value.to_string();
    let a = GcPtr::<u8>::malloc_array(s.len() + 1);
    // copy content
    libc::memcpy(a.as_ptr() as _, s.as_slice().as_ptr() as _, s.len());
    // append null byte
    a.as_mut().add(s.len()).write(0);

    libc::printf(a.as_mut() as _);
    libc::printf(b"\n\0".as_ptr() as _);
}


#[no_mangle]
pub unsafe extern "C" fn RT_gc_malloc(size:u32) -> *mut Any{
    GcPtr::new(Any::UNDEFINED).as_mut()
}

static IN_TRY: RwLock<usize> = RwLock::new(0);
static ERROR: RwLock<Option<Any>> = RwLock::new(None);

#[no_mangle]
pub unsafe extern "C" fn RT_enter_try() {
    IN_TRY.write().add_assign(1);
}

#[no_mangle]
pub unsafe extern "C" fn RT_exit_try() {
    IN_TRY.write().sub_assign(1);
}

#[no_mangle]
pub unsafe extern "C" fn RT_throw(value: Any) {
    let coro = Coroutine::<Any, Result<Any, Any>, Any, ()>::running();

    // in a coroutine
    if let Some(coro) = coro{
        // yield an error
        coro.yield_(Err(value));
    }

    let in_try = IN_TRY.read();
    let is_in_try = *in_try;
    drop(in_try);

    if is_in_try != 0 {
        let mut error = ERROR.write();
        if let Some(e) = *error {
            libc::printf(b"Uncaught exception while handling exception: \0".as_ptr() as _);
            libc::printf(e.to_string().to_c_str().as_ptr());
            libc::printf(b"\n\0".as_ptr() as _);
            libc::exit(1);
        } else {
            *error = Some(value);
        }
    } else {
        libc::printf(b"Uncaught error: \n\0".as_ptr() as _);
        libc::printf(value.to_string().to_c_str().as_ptr());
        libc::printf(b"\n\0".as_ptr() as _);
        libc::exit(1);
    }
}

#[no_mangle]
pub unsafe extern "C" fn RT_has_exception() -> bool {
    ERROR.read().is_some()
}

#[no_mangle]
pub unsafe extern "C" fn RT_get_exception() -> Any {
    ERROR.read().unwrap()
}

#[no_mangle]
pub unsafe extern "C" fn RT_set_exception(value: Any) {
    let mut error = ERROR.write();

    *error = Some(value);
}

#[no_mangle]
pub unsafe extern "C" fn RT_init_function(func: JSFunc, captures:*const *const Any, capture_size:u32, this: Any, is_arrow:bool, is_async:bool, is_generator:bool) -> Any {
    let mut obj = Object::new();

    if capture_size == 0 && !is_arrow{
        obj.set_internal(ObjectValue::Function(crate::types::TSFuncPtr(func)));
    } else{

        let slice = core::slice::from_raw_parts(captures, capture_size as usize);
        
        // copy the captures
        let dst = GcPtr::<GcPtr<Any>>::malloc_array(capture_size as usize);

        let mut i = 0;
        for cap in slice{

            let ptr = GcPtr::from_ptr(*cap as *mut Any).expect("error creating GcPtr from *const ()");
            dst.as_mut().add(i).write(ptr);

            i += 1;
        }

        let c = crate::types::Closure{
            ptr: crate::types::TSFuncPtr(func),
            this: if is_arrow{Some(this)}else{None},
            captures: dst
        };

        if is_async && is_generator{
            obj.set_internal(ObjectValue::AsyncGeneratorFunction(c));
        } else if is_async{
            obj.set_internal(ObjectValue::AsyncFunction(c));
        } else if is_generator{
            obj.set_internal(ObjectValue::GeneratorFunction(c))
        } else{
            obj.set_internal(ObjectValue::Closure(c))
        }
        
    };
    return obj.to_any();
}

#[no_mangle]
pub unsafe extern "C" fn RT_call(
    callee: Any,
    this: Any,
    argc: u32,
    argv: *const Any,
) -> Any {
    let args = core::slice::from_raw_parts(argv, argc as usize);
    return callee.call(this, args);
}

#[no_mangle]
pub unsafe extern "C" fn RT_call_varargs(
    callee: Any,
    this: Any,
    argc: u32,
    argv: *const Any,
) -> Any {
    let args = core::slice::from_raw_parts(argv, argc as usize);
    return callee.call(this, args);
}

#[no_mangle]
pub unsafe extern "C" fn RT_new_call(callee: Any, argc:u32, argv:*mut Any) -> Any{
    let args = core::slice::from_raw_parts(argv, argc as usize);

    let obj = Object::new();

    // set prototype
    let prototype = callee.get_property_key(hash!("prototype")).unwrap_or(Any::NULL);
    
    todo!();

    return callee.call(obj.to_any(), args)
}

#[no_mangle]
pub unsafe extern "C" fn RT_await(value: Any) -> Any{
    

    if let Some(obj) = value.as_object(){
        if let Some(p) = obj.as_promise(){
            let coro = Coroutine::<Any, Result<AwaitOrYield, Any>, Any, (), gc::Allocater>::running();

            if let Some(coro) = coro{
                // not in global context
                // loop until result is fetched
                loop{
                    if let Some(re) = p.poll(){
                        match re{
                            Ok(r) => return r,
                            Err(e) => {
                                coro.yield_(Err(e));
                                return e
                            }
                        }
                    }

                    coro.yield_(Ok(AwaitOrYield::Await));
                };

            } else {
                // global context
                loop{
                    if let Some(re) = p.poll(){
                        match re{
                            Ok(r) => return r,
                            Err(e) => {
                                RT_throw(e);
                                return e
                            }
                        }
                    }

                    for i in &mut ASYNC_FUTURES{
                        i.resume();
                    }

                    for i in &mut ASYNC_GENERATORS{
                        i.resume();
                    }
                }
            }
            
        }
    }
    
    return value
}

#[no_mangle]
pub unsafe extern "C" fn RT_yield(value: Any) -> Any{
    let mut o = Object::new();
    o.set_property(JSString::Constant("value"), value);
    o.set_property(JSString::Constant("done"), Any::FALSE);

    let coro:CoroutineRef<Any, Result<AwaitOrYield, Any>, Any, (), gc::Allocater> = Coroutine::running().expect("calling yield on global context");

    let y:Any = coro.yield_(Ok(AwaitOrYield::Yield(o.to_any())));

    return y
}

#[no_mangle]
pub unsafe extern "C" fn RT_create_regexp(pattern:*mut u8, pattern_len:u32, flags:*mut u8, flags_len:u32) -> Any{
    todo!()
}


#[no_mangle]
pub unsafe extern "C" fn RT_create_array(size:u32) -> Any{
    let obj = Object::new();
    return obj.to_any();
}

#[no_mangle]
pub unsafe extern "C" fn RT_array_push(array:Any, value: Any){
    array.as_object().unwrap().push(value);
}


#[test]
fn test_rt_print(){
    
    unsafe{
        RT_print(Object::new().to_any())
    }
}