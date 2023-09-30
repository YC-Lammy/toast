
use minicoroutine::{Coroutine, CoroutineResult};

use crate::gc::GcPtr;
use crate::gc;

use super::JSString;
use super::{Any, TSFuncPtr, Object};

pub struct Generator{
    co: Coroutine<Any, Result<Any, Any>, Any, GeneratorState, gc::Allocater>
}

#[derive(Clone)]
pub struct GeneratorState{
    func: TSFuncPtr,
    this: Any,
    captures: GcPtr<GcPtr<Any>>,
    args: GcPtr<Any>,
    args_len: usize,
}

impl Generator{
    pub fn new(func:TSFuncPtr, captures:GcPtr<GcPtr<Any>>, this:Any, args:GcPtr<Any>, args_len:usize) -> Self{
        let co = Coroutine::new_in(
            |coro|{
                let state = coro.user_data();
                let re = state.func.dynamic_call(state.this, state.captures.as_double_ptr(), state.args_len as u32, state.args.as_ptr());

                return re
            }, 
            GeneratorState { 
                func,
                this,
                captures, 
                args,
                args_len
            },
            gc::Allocater
        ).unwrap();

        return Self { 
            co: co
        }
    }

    /// return None if generator returned
    pub fn resume(&mut self, value: Any) -> Option<Result<Any, Any>>{
        let re = self.co.resume(value);

        match re{
            None => None,
            Some(re) => {
                match re{
                    CoroutineResult::Error(e) => {
                        unsafe{
                            let p = alloc::format!("{}\n\0", e);
                            libc::printf(p.as_bytes().as_ptr() as _);
                        }
                        None
                    },
                    CoroutineResult::Return(r) => Some(Ok(r)),
                    CoroutineResult::Yield(y) => Some(y),
                }
            }
        }
    }
}

lazy_static::lazy_static!{
    pub static ref PROTOTYPE:Object = {
        let mut obj = Object::new();
        obj.inner.set_root();

        obj.set_property(JSString::Constant("next"), Any::UNDEFINED);

        obj
    };
}