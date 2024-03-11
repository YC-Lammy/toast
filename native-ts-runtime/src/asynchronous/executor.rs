use core::marker::PhantomData;

use alloc::vec::Vec;
use lazy_thread_local::ThreadLocal;

use super::task::AsyncTask;

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TaskHandle<'a, T = u8>(usize, PhantomData<&'a T>);

pub struct Executor {
    tasks: ThreadLocal<Vec<AsyncTask>>,
}

unsafe impl Sync for Executor {}

impl Executor {
    pub const fn new() -> Self {
        Self {
            tasks: ThreadLocal::const_new(Vec::new()),
        }
    }

    pub fn spawn<T>(&self, task: AsyncTask<T>) -> TaskHandle<T>{
        let mut task: AsyncTask = unsafe{core::mem::transmute(task)};

        self.run_once(&mut task);
        let tasks = self.tasks.get_mut();
        let id = tasks.len();
        tasks.push(task);

        TaskHandle(id, PhantomData)
    }

    pub fn wait_all(&self) {
        loop {
            let tasks = self.tasks.get_mut();
            let mut all_done = true;

            for task in tasks {
                all_done &= self.run_once(task);
            }

            if all_done {
                break;
            }
        }
    }

    /// poll the task once and return reference to result
    pub fn poll_task<'a, T>(&'a self, handle: TaskHandle<'a, T>) -> Option<&'a T>{
        let tasks = self.tasks.get_mut();
        if let Some(task) = tasks.get_mut(handle.0){
            if self.run_once(task){
                return Some(unsafe{task.result.cast().as_ref()})
            }
        }
        return None
    }

    fn run_once(&self, task: &mut AsyncTask) -> bool {
        if task.ready {
            return true;
        }

        return false;
    }
}
