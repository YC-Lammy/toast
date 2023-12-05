
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

unsafe impl<const N:usize> Sync for Heap<N>{}
unsafe impl<const N:usize> Send for Heap<N>{}

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
        // load cursor
        let mut cursor = self.cursor.load(Ordering::Relaxed);
        loop{
            // allocate from block
            if let Some(cell) = self.blocks[cursor].allocate(){
                return cell
            }
            
            // cursor reaches end
            if cursor +1 >= self.blocks.len(){
                self.grow();
            }

            // cursor is updated
            if let Err(updated) = self.cursor.compare_exchange_weak(cursor, cursor + 1, Ordering::SeqCst, Ordering::Relaxed){
                cursor = updated;
            } else{
                cursor += 1;
            }
        }
    }
    pub fn grow(&mut self){
        // heap is already growing
        if self.growing.swap(true, Ordering::SeqCst){
            while self.growing.load(Ordering::Relaxed){};
            return;
        }

        // allocate new block
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

        // finish growing
        self.growing.store(false, Ordering::SeqCst);
    }

    pub fn grey_scan(&self){
        for b in &self.blocks{
            b.grey_scan();
        }
    }

    pub fn sweep(&self){
        for b in &self.blocks{
            b.sweep();
        }
        // reset cursor
        self.cursor.store(0, Ordering::SeqCst);
    }
}

impl<const N:usize> Block<N>{
    pub fn allocate(&self) -> Option<&'static mut Cell>{
        let mut cursor = self.cursor.load(Ordering::Relaxed);

        loop{
            if cursor >= 4096 * 4{
                return None
            }

            unsafe{
                let ptr = (self.data as *mut u8).add(cursor);
                let cell = (ptr as *mut Cell).as_mut().unwrap_unchecked();

                // not allocated
                if !cell.header.flags.swap_allocated(true){
                    // set grey
                    cell.header.flags.set_grey();
                    // return cell
                    return Some(cell)
                }
            }
            
            if let Err(updated) = self.cursor.compare_exchange_weak(cursor, cursor + N, Ordering::SeqCst, Ordering::Relaxed){
                cursor = updated;
            } else{
                cursor += N;
            }
        }
    }

    pub fn grey_scan(&self){
        let ptr = self.data as *mut u8;

        for i in 0..(4096 * 4)/N{
            unsafe{
                let cell = (ptr.add(N * i) as *mut Cell).as_mut().unwrap_unchecked();
                // is grey
                if cell.header.flags.is_grey(){
                    cell.trace();
                }
            }
        }
    }

    pub fn sweep(&self){
        let ptr = self.data as *mut u8;

        for i in 0..(4096 * 4)/N{
            unsafe{
                let cell = (ptr.add(N * i) as *mut Cell).as_mut().unwrap_unchecked();
                // is white and allocated
                if cell.header.flags.set_white() && cell.header.flags.is_allocated(){
                    if let Some(dtor) = cell.header.dtor{
                        // remove destructor
                        cell.header.dtor = None;
                        // call destructor
                        dtor(cell.payload.as_mut_ptr());
                    }
                    // deallocate
                    cell.header.flags.clear();
                }
            }
        }
        // reset cursor
        self.cursor.store(0, Ordering::SeqCst);
    }
}