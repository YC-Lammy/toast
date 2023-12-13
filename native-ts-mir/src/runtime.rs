use crate::util::{AggregateID, FunctionID};

#[derive(Debug, Default)]
pub struct Runtime<'ctx> {
    pub memory_management: MemoryManagement<'ctx>,
    pub async_runtime: Option<AsyncRuntime<'ctx>>,
}

#[derive(Debug)]
pub enum MemoryManagement<'ctx> {
    /// manual memory management
    Manual,
    /// automatic reference counting
    Arc {
        /// should have type fn(usize) -> *mut u8
        malloc: FunctionID<'ctx>,
        /// pointer may contain header
        ptr_offset: isize,
        /// should have type fn(*mut u8)
        increment_count: FunctionID<'ctx>,
        /// should have type fn(*mut u8)
        decrement_count: FunctionID<'ctx>,
    },
    /// garbage collector
    GarbageCollect {
        /// should have type fn(usize) -> *mut u8
        malloc: FunctionID<'ctx>,
        /// should have type fn(obj: *mut u8, offset: usize, child: *mut u8)
        write_barrier: FunctionID<'ctx>,
        /// pointer may contain header
        ptr_offset: isize,
        /// should have type fn()
        safepoint: FunctionID<'ctx>,
    },
}

impl<'ctx> Default for MemoryManagement<'ctx> {
    fn default() -> Self {
        return Self::Manual;
    }
}

#[derive(Debug)]
pub struct AsyncRuntime<'ctx> {
    /// the type of a future
    pub future_type: AggregateID<'ctx>,
    /// create a future. The first argument is the pointer that stores thr result.
    /// should have type fn(*mut u8) -> future_type
    pub create_future: FunctionID<'ctx>,
    /// should have type fn(furute_type)
    pub resolve_future: FunctionID<'ctx>,
    /// awaits a future, returns the result pointer, null if future is not resolved.
    /// should have type fn(future_type) -> *mut u8
    pub await_future: FunctionID<'ctx>,
}
