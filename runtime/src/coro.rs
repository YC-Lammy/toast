use core::marker::PhantomData;
use core::sync::atomic::AtomicU8;
use core::sync::atomic::Ordering;


static mut CURRENT_CORO: usize = 0;

#[repr(u8)]
pub enum CoroutineState{
    Idle,
    Running,
    Paniced,
    Returned
}


pub struct Coroutine<Input, Yield, Return>{
    last_coro: usize,
    state: CoroutineState,

    _p: PhantomData<(Input, Yield, Return)>
}


pub enum ResumeResult<Yield, Return>{
    Yield(Yield),
    Return(Return),
    Err(&'static str),
}

impl<Input, Yield, Return> Coroutine<Input, Yield, Return>{
    pub fn new() -> Self{
        Coroutine {
            last_coro: 0,
            state: CoroutineState::Idle,
            _p: PhantomData
        }
    }

    pub fn resume(&mut self, value: Input) -> ResumeResult<Yield, Return>{
        match self.state {
            CoroutineState::Idle => {},
            CoroutineState::Running => return ResumeResult::Err("coroutine running"),
            _ => return ResumeResult::Err("Coroutine ended")
        };

        self.last_coro = unsafe {CURRENT_CORO};
        unsafe{
            CURRENT_CORO = self as *mut Self as usize;
        };


        unsafe{
            CURRENT_CORO = self.last_coro;
        };

        todo!()
    }
}