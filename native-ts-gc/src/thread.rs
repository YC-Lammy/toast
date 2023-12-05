use std::marker::PhantomData;

use alloc::boxed::Box;

#[cfg(unix)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ThreadID(libc::pthread_t);

pub struct JoinHandle<T>{
    _mark: PhantomData<T>
}

#[cfg(unix)]
fn spawn<F: Fn() -> T + Send + 'static, T>(f: F) -> JoinHandle<T>{
    extern "C" fn pthread_wrapper<T, F: Fn() -> T + Send + 'static>(value: *mut libc::c_void) -> *mut libc::c_void{
        unsafe{
            let value = (value as *mut F).as_mut().unwrap_unchecked();
            let re = (value)();
            drop(Box::from_raw(value));
            return Box::leak(Box::new(re)) as *mut T as *mut libc::c_void
        }
    }

    unsafe{
        let mut id: libc::pthread_t = 0;
        let mut attr: libc::pthread_attr_t = core::mem::zeroed();
        let wrapped = Box::leak(Box::new(f));
        let r = libc::pthread_create(&mut id, &mut attr, pthread_wrapper::<T, F>, wrapped as *mut F as *mut libc::c_void);

        return JoinHandle { _mark: PhantomData }
    }   
}