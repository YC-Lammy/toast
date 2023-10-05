
use core::sync::atomic::AtomicU8;

use alloc::boxed::Box;
use iron_gc::{
    GcPtr,
    Array
};

use crate::types::Any;

type AsyncFunction = extern "C" fn(*const (), usize) -> Any;

#[repr(C)]
pub struct AsyncRoutine{
    state: &'static mut PromisState,
    pub(crate) count: usize,
    pub(crate) data: GcPtr<Array<Any>>,
    pub(crate) func: *const ()
}

impl AsyncRoutine{

}

pub fn spawn(func: *const (), data: GcPtr<Array<Any>>) -> Promise{
    // make sure data is root
    data.set_root();

    let state = Box::leak(Box::new(
        PromisState{
            flags: PromiseFlags::default(),
        }
    ));

    let routine = AsyncRoutine{
        state: state,
        count: 0,
        data,
        func
    };

    return Promise {
        state: &state,
    }
}

#[repr(C)]
pub struct Promise{
    state: &'static PromisState,
}

struct PromisState{
    flags: PromiseFlags
}

#[derive(Debug, Default)]
struct PromiseFlags(AtomicU8);


