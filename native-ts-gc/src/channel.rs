use core::mem::MaybeUninit;
use std::sync::{Arc, atomic::{AtomicUsize, AtomicPtr, Ordering}};

struct Buffer<T, const N: usize = 0>{
    small: [Option<T>;N],
    cursor: AtomicUsize,
}

pub struct Sender<T, const N: usize = 0>{
    buffer: Arc<Buffer<T, N>>
}

pub struct Reciever<T, const N:usize = 0>{
    buffer: Arc<Buffer<T, N>>
}

impl<T, const N:usize> Buffer<T, N>{
    pub fn pop_front(&mut self) -> Option<T>{
        let mut loc = self.cursor.fetch_add(1, Ordering::SeqCst);
        if loc >= N{
            let _ = self.cursor.compare_exchange_weak(loc, loc - N, Ordering::SeqCst, Ordering::Relaxed);
            loc = loc - N;
        }
        let value = core::mem::replace(&mut self.small[loc], None);

        if value.is_none(){
            self.cursor.fetch_sub(1, Ordering::SeqCst);
        }
        return value
    }
    pub fn push_end(&mut self, value: T){
        loop{
            let mut loc = self.cursor.fetch_add(1, Ordering::SeqCst);
            if loc >= N{
                let _ = self.cursor.compare_exchange_weak(loc, loc - N, Ordering::SeqCst, Ordering::Relaxed);
                loc = loc - N;
            }

            self.cursor.fetch_sub(1, Ordering::SeqCst);
        }
    }
}

pub fn bounded<T, const N:usize>() -> (Sender<T>, Reciever<T>){
    todo!()
}