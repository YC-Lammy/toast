pub mod task;
pub mod file;
pub mod executor;

pub static GLOBAL_EXECUTOR: executor::Executor = executor::Executor::new();