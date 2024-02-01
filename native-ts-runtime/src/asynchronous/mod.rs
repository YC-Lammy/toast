
use core::{sync::atomic::AtomicU8, ptr::NonNull};

use alloc::boxed::Box;
use iron_gc::GcPtr;

use crate::types::{Any, HeapVariable};

struct AsyncVariables{
    len: usize,
}

impl iron_gc::Trace for AsyncVariables{
    fn additional_bytes(&self) -> usize {
        return self.len * 8
    }
    fn trace(&mut self, visitor: &mut iron_gc::Visitor) {
        unsafe{
            let ptr = (self as *const Self).add(1) as *mut Any;

            for i in 0..self.len{
                ptr.add(i).as_mut().map(
                    |ptr|ptr.trace(visitor)
                );
            }
        }
    }
}

impl AsyncVariables{
    pub fn len(&self) -> usize{
        return self.len
    }
    pub unsafe fn to_ptr(&self) -> *const Any{
        (self as *const Self).add(1) as *const Any
    }
}

type AsyncFunction = extern "C" fn(usize, *mut bool, Any, *const HeapVariable, *const Any, u32) -> Any;

#[repr(C)]
pub struct AsyncRoutine{
    state: NonNull<PromisState>,
    count: usize,
    is_done: bool,
    result: Any,
    this: Any,
    variables: GcPtr<AsyncVariables>,
    captures: Box<[HeapVariable]>,
    func: *const ()
}

impl AsyncRoutine{
    pub fn poll(&mut self) -> Option<Any>{
        if self.is_done{
            return Some(self.result);
        }

        unsafe{
            let func:AsyncFunction = core::mem::transmute(self.func);
            self.result = func(self.count, &mut self.is_done, self.this, self.captures.as_ptr(), self.variables.to_ptr(), self.variables.len as u32);
        };

        if self.is_done{
            return Some(self.result);
        }

        return None
    }
}

pub fn spawn(func: *const (), this: Any, variables: u32, captures: Box<[HeapVariable]>) -> Promise{

    let v = GcPtr::new(
        AsyncVariables{
            len: variables as usize
        }
    );

    let state = Box::leak(Box::new(
        PromisState{
            flags: PromiseFlags::default(),
            result: Any::UNDEFINED
        }
    ));

    let state = NonNull::from(state);

    let routine = AsyncRoutine{
        state: state,
        count: 0,
        is_done: false,
        result: Any::UNDEFINED,
        func,
        this,
        variables: v,
        captures
    };

    return Promise {
        state: state,
    }
}

#[repr(C)]
pub struct Promise{
    state: NonNull<PromisState>,
}

unsafe impl Sync for Promise{}
unsafe impl Send for Promise{}

struct PromisState{
    flags: PromiseFlags,
    result: Any,
}

#[derive(Debug, Default)]
struct PromiseFlags(AtomicU8);

impl iron_gc::Trace for Promise{
    fn trace(&mut self, visitor: &mut iron_gc::Visitor) {
        unsafe{
            self.state.as_mut().result.trace(visitor);
        }
    }
}

