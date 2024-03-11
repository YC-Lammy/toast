use core::ptr::NonNull;

pub mod executor;
pub mod file;
pub mod task;

pub static GLOBAL_EXECUTOR: executor::Executor = executor::Executor::new();

#[no_mangle]
pub extern "C" fn __native_ts_async_task_create(
    poll_func: extern "C" fn(&mut task::AsyncContext, *mut u8, *mut u8) -> task::AsyncTaskPoll,
    task_size: usize,
    result_size: usize,
) -> task::AsyncTask {
    let state = unsafe{crate::gc::allocate_raw(task_size)};
    let re = unsafe{crate::gc::allocate_raw(result_size)};

    return task::AsyncTask {
        poll: poll_func,
        state: state,
        ready: false,
        result: re,
    };
}

#[no_mangle]
pub extern "C" fn __native_ts_async_spawn(task: task::AsyncTask) -> executor::TaskHandle<'static>{
    GLOBAL_EXECUTOR.spawn(task)
}

#[no_mangle]
pub extern "C" fn __native_ts_async_await(task: executor::TaskHandle) -> Option<NonNull<u8>>{
    match GLOBAL_EXECUTOR.poll_task(task){
        Some(p) => Some(NonNull::new(p as *const _ as *mut _)?),
        None => None,
    }
}