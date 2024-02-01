use core::{any::Any, mem::MaybeUninit};

use alloc::boxed::Box;

use unwinding::abi::{UnwindException, UnwindReasonCode};

use super::panicking::Exception;


#[repr(transparent)]
struct RustPanic(Box<dyn Any + Send>, DropGuard);

struct DropGuard;

impl Drop for DropGuard {
    fn drop(&mut self) {
        unsafe{
            libc::exit(1)
        };
    }
}

#[repr(C)]
struct ExceptionWithPayload {
    exception: MaybeUninit<UnwindException>,
    payload: RustPanic,
}

unsafe impl Exception for RustPanic {
    const CLASS: [u8; 8] = *b"MOZ\0RUST";

    fn wrap(this: Self) -> *mut UnwindException {
        Box::into_raw(Box::new(ExceptionWithPayload {
            exception: MaybeUninit::uninit(),
            payload: this,
        })) as *mut UnwindException
    }

    unsafe fn unwrap(ex: *mut UnwindException) -> Self {
        let ex = unsafe { Box::from_raw(ex as *mut ExceptionWithPayload) };
        ex.payload
    }
}

pub fn begin_panic(payload: Box<dyn Any + Send>) -> UnwindReasonCode {
    super::panicking::begin_panic(RustPanic(payload, DropGuard))
}