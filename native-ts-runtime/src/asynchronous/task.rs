use core::future::Future;
use core::pin::Pin;
use core::ptr::NonNull;
use core::task::Poll;

use crate::gc;

#[repr(C)]
pub struct AsyncContext<'a> {
    raw_context: core::task::Context<'a>,
}

#[repr(u8)]
pub enum AsyncTaskPoll {
    Pending = 0,
    Ready = 1,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct AsyncTask<T = u8> {
    /// the poll function
    pub(crate) poll:
        extern "C" fn(ctx: &mut AsyncContext, state: *mut u8, re: *mut u8) -> AsyncTaskPoll,
    /// state of function
    pub(crate) state: NonNull<u8>,
    /// if is ready
    pub(crate) ready: bool,
    /// the result store
    pub(crate) result: NonNull<T>,
}

impl AsyncTask {
    pub fn from_future<F, T>(future: F) -> AsyncTask
    where
        F: Future<Output = T> + Unpin + 'static,
    {
        extern "C" fn poll_future<T, F>(
            ctx: &mut AsyncContext,
            state: *mut u8,
            re: *mut u8,
        ) -> AsyncTaskPoll
        where
            F: Future<Output = T>,
        {
            unsafe {
                let f = &mut *(state as *mut F);
                let future = Pin::new_unchecked(f);

                match future.poll(&mut ctx.raw_context) {
                    Poll::Pending => return AsyncTaskPoll::Pending,
                    Poll::Ready(v) => {
                        (re as *mut T).write(v);
                        return AsyncTaskPoll::Ready;
                    }
                };
            }
        }

        
        unsafe {
            let re = gc::allocate::<T>(core::mem::zeroed());
            let state = gc::allocate::<F>(future);
            
            return AsyncTask {
                poll: poll_future::<T, F>,
                state: state.cast(),
                ready: false,
                result: re.cast(),
            };
        }
    }
}
