use alloc::vec::Vec;
use lazy_thread_local::ThreadLocal;

use super::task::AsyncTask;

pub struct Executor{
    tasks: ThreadLocal<Vec<AsyncTask>>,
}

unsafe impl Sync for Executor{}

impl Executor{
    pub const fn new() -> Self{
        Self{
            tasks: ThreadLocal::const_new(Vec::new())
        }
    }

    pub fn spawn(&self, mut task: AsyncTask){
        self.run_once(&mut task);
        self.tasks.write().push(task);
    }

    pub fn wait_all(&self){

    }

    fn run_once(&self, task: &mut AsyncTask){

    }
}