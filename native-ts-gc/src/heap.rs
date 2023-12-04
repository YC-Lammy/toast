
use std::sync::atomic::{
    Ordering,
    AtomicUsize, 
    AtomicBool
};

use crate::cell::{Cell, Flags};

pub struct Block<const N:usize>{
    cursor: AtomicUsize,
    data: *mut [u8;4096 * 4]
}

pub struct Heap<const N:usize>{
    cursor: AtomicUsize,
    growing: AtomicBool,
    blocks: Vec<Block<N>>,
}

impl<const N:usize> Heap<N>{
    pub const fn new() -> Self{
        Self{
            cursor: AtomicUsize::new(0),
            growing: AtomicBool::new(false),
            blocks: Vec::new(),
        }
    }
    pub fn allocate(&mut self) -> &'static mut Cell{
        if self.blocks.len() == 0{
            self.grow();
        };

        let mut cursor = self.cursor.load(Ordering::Relaxed);
        loop{
            if let Some(cell) = self.blocks[cursor].allocate(){
                return cell
            }
            
            if cursor +1 == self.blocks.len(){
                self.grow();
            }

            if let Err(updated) = self.cursor.compare_exchange_weak(cursor, cursor + 1, Ordering::SeqCst, Ordering::Relaxed){
                cursor = updated;
            } else{
                cursor += 1;
            }
        }
    }
    pub fn grow(&mut self){
        if self.growing.swap(true, Ordering::SeqCst){
            while self.growing.load(Ordering::Relaxed){};
            return;
        }

        self.blocks.push(
            Block { 
                cursor: AtomicUsize::new(0), 
                data: unsafe{
                    alloc::alloc::alloc_zeroed(
                        alloc::alloc::Layout::new::<[u8;4096 * 4]>()
                    ) as *mut [u8;4096 * 4]
                }
            }
        );

        self.growing.store(false, Ordering::SeqCst);
    }
}

impl<const N:usize> Block<N>{
    pub fn allocate(&self) -> Option<&'static mut Cell>{
        let mut cursor = self.cursor.fetch_add(N, Ordering::SeqCst);

        loop{
            if cursor >= 4096 * 4{
                return None
            }

            unsafe{
                let ptr = (self.data as *mut u8).add(cursor);
                let cell = (ptr as *mut Cell).as_mut().unwrap_unchecked();
                if !cell.header.flags.is_allocated(){
                    cell.header.flags = cell.header.flags & Flags::ALLOCATED;
                    return Some(cell)
                }
            }
            
            if let Err(updated) = self.cursor.compare_exchange_weak(cursor + N, cursor + N + N, Ordering::SeqCst, Ordering::Relaxed){
                cursor = updated;
            } else{
                cursor += N;
            }
        }
    }
}