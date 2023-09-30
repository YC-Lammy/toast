
use alloc::vec::Vec;
use minicoroutine::Coroutine;

use crate::gc::{GcPtr, self};

use super::{Any, TSFuncPtr};

pub static mut ASYNC_FUTURES:Vec<AsyncFuture> = Vec::new();

pub enum AwaitOrYield{
    Await,
    Yield(Any)
}

pub struct AsyncFuture{
    finished: bool,
    coro: Coroutine<Any, Result<AwaitOrYield, Any>, Any, AsyncState, gc::Allocater>,
    feeder: GcPtr<Option<Result<Any, Any>>>
}

struct AsyncState{
    func: TSFuncPtr,
    captures: Option<GcPtr<GcPtr<Any>>>,
    this: Any,
    args: GcPtr<Any>,
    args_len: usize,
}

impl AsyncFuture{
    pub fn register(func:TSFuncPtr, captures:Option<GcPtr<GcPtr<Any>>>, this:Any, args:&[Any]) -> GcPtr<Option<Result<Any, Any>>>{
        let args_len = args.len();
        let args = GcPtr::new_array(args);
        // set args to root
        args.set_root();

        // set the captures to root
        captures.map(|m|m.set_root());

        let state = AsyncState{
            func,
            captures,
            this,
            args,
            args_len
        };

        let ptr = GcPtr::new(None);
        // set feeder to root
        ptr.set_root();

        let coro = Coroutine::new_in(
            move |coro|{
                let state = coro.user_data();

                let captures = match state.captures{
                    Some(p) => p.as_double_ptr(),
                    None => 0 as _
                };

                let re = state.func.dynamic_call(state.this, captures, args_len as u32, args.as_ptr());

                return re;
            }, 
            state,
            gc::Allocater
        ).unwrap();

        let mut f = AsyncFuture{
            finished: false,
            coro: coro,
            feeder: ptr
        };

        f.resume();

        unsafe{
            ASYNC_FUTURES.push(f);
        }

        return ptr
    }

    pub fn resume(&mut self) -> bool{
        if self.finished{
            return true
        }

        let re = self.coro.resume(Any::UNDEFINED);

        match re{
            None => return true,
            Some(re) => {
                match re{
                    minicoroutine::CoroutineResult::Error(e) => panic!("{}", e),
                    minicoroutine::CoroutineResult::Return(r) => unsafe{
                        self.feeder.as_mut().write(Some(Ok(r)));
                        return true
                    },
                    minicoroutine::CoroutineResult::Yield(re) => unsafe{
                        match re{
                            Ok(_) => {},
                            Err(e) => {
                                self.feeder.as_mut().write(Some(Err(e)));
                                self.finished = true;
                                return true
                            }
                        };
                    }
                };
            }
        };
        return false
    }
}