extern crate alloc;

mod heap;
mod cell;
mod thread;

use std::sync::atomic::{AtomicUsize, Ordering, AtomicBool};

use cell::{Flags, CellHeader};
use crossbeam_channel::{Sender, Receiver};
use heap::Heap;

const ALLOCATION_PER_GC: usize = 4096 * 4;

pub struct Allocator{
    allocations: AtomicUsize,

    gc_running: AtomicBool,
    gc_marking_phase: AtomicBool,
    gc_grey_scanning_phase: AtomicBool,
    gc_sweeping_phase: AtomicBool,

    threads_at_safe_point: AtomicUsize,

    num_threads: AtomicUsize,
    threads: [Option<()>;32],
    gc_thread_handle: Option<std::thread::JoinHandle<()>>,

    heap16: Heap<16>,
    heap32: Heap<32>,
    heap64: Heap<64>,
    heap128: Heap<128>,
    heap256: Heap<256>,
    heap512: Heap<512>,
}

impl Allocator{
    pub fn is_gc_marking_phase(&self) -> bool{
        false
    }
    pub fn is_grey_scanning_phase(&self) -> bool{
        false
    }
}

static mut ALLOCATOR: Allocator = Allocator{
    allocations: AtomicUsize::new(0),

    gc_running: AtomicBool::new(false),
    gc_marking_phase: AtomicBool::new(false),
    gc_grey_scanning_phase: AtomicBool::new(false),
    gc_sweeping_phase: AtomicBool::new(false),

    num_threads: AtomicUsize::new(0),
    threads: [None; 32],
    threads_at_safe_point: AtomicUsize::new(0),
    gc_thread_handle: None,

    heap16: Heap::new(),
    heap32: Heap::new(),
    heap64: Heap::new(),
    heap128: Heap::new(),
    heap256: Heap::new(),
    heap512: Heap::new()
};

lazy_static::lazy_static!{
    static ref WORK:(Sender<Box<dyn Fn() + Send>>, Receiver<Box<dyn Fn() + Send>>) = crossbeam_channel::unbounded();
}

pub fn safe_point(){
    unsafe{
        if ALLOCATOR.gc_grey_scanning_phase.load(Ordering::SeqCst) {
            ALLOCATOR.threads_at_safe_point.fetch_add(1, Ordering::SeqCst);
            while ALLOCATOR.is_grey_scanning_phase(){
                if let Ok(work) = WORK.1.try_recv(){
                    // do the work
                    work();
                }
            }
            ALLOCATOR.threads_at_safe_point.fetch_sub(1, Ordering::SeqCst);
        }
    }
}

pub fn write_barrier(ptr: &mut cell::Cell, slot: &mut *mut cell::Cell, value: &mut cell::Cell){
    unsafe{
        if ALLOCATOR.gc_running.load(Ordering::Relaxed){
            ptr.header.flags.set_grey();
            value.header.flags.set_grey();
            
            *slot = value;
            // a possible safe point
            safe_point();
        } else{
            *slot = value;
        }
    }
    
}

pub fn allocate(size: usize) -> &'static mut cell::Cell{
    // a possible safe point
    safe_point();

    unsafe{
        if ALLOCATOR.allocations.fetch_add(1, Ordering::Relaxed) == ALLOCATION_PER_GC{
            ALLOCATOR.allocations.store(0, Ordering::SeqCst);
            garbage_collect();
        }
        
        match size + core::mem::size_of::<CellHeader>(){
            16 => ALLOCATOR.heap16.allocate(),
            32 => ALLOCATOR.heap32.allocate(),
            64 => ALLOCATOR.heap64.allocate(),
            128 => ALLOCATOR.heap128.allocate(),
            256 => ALLOCATOR.heap256.allocate(),
            512 => ALLOCATOR.heap512.allocate(),
            _ => {
                todo!()
            }
        }
    }
    
}

pub fn shade(cell: &mut cell::Cell){
    cell.header.flags.set_grey();
}

pub unsafe fn garbage_collect(){
    // set gc running to true
    if ALLOCATOR.gc_running.swap(true, Ordering::SeqCst){
        // gc already running
        return;
    }

    static GC_THREAD_LAUNCHED: AtomicBool = AtomicBool::new(false);

    if !GC_THREAD_LAUNCHED.swap(true, Ordering::SeqCst){
        let handle = std::thread::spawn(||unsafe{
            loop{
                if ALLOCATOR.gc_running.load(Ordering::SeqCst){
                    // start marking phase
                    ALLOCATOR.gc_marking_phase.store(true, Ordering::SeqCst);

                    // mark roots

                    // finish marking phase
                    ALLOCATOR.gc_marking_phase.store(false, Ordering::SeqCst);

                    // grey scanning
                    ALLOCATOR.gc_grey_scanning_phase.store(true, Ordering::SeqCst);

                    // wait for threads to reach safe point
                    while ALLOCATOR.threads_at_safe_point.load(Ordering::Relaxed) < ALLOCATOR.num_threads.load(Ordering::Relaxed){}

                    // actual grey scanning
                    ALLOCATOR.heap16.grey_scan();
                    ALLOCATOR.heap32.grey_scan();
                    ALLOCATOR.heap64.grey_scan();
                    ALLOCATOR.heap128.grey_scan();
                    ALLOCATOR.heap256.grey_scan();
                    ALLOCATOR.heap512.grey_scan();

                    // finish scanning
                    ALLOCATOR.gc_grey_scanning_phase.store(false, Ordering::SeqCst);

                    // sweeping
                    ALLOCATOR.gc_sweeping_phase.store(true, Ordering::SeqCst);

                    ALLOCATOR.heap16.sweep();
                    ALLOCATOR.heap32.sweep();
                    ALLOCATOR.heap64.sweep();
                    ALLOCATOR.heap128.sweep();
                    ALLOCATOR.heap256.sweep();
                    ALLOCATOR.heap512.sweep();

                    // finish sweeping
                    ALLOCATOR.gc_sweeping_phase.store(false, Ordering::SeqCst);

                    // finish gc
                    ALLOCATOR.gc_running.store(false, Ordering::SeqCst);
                }
                std::thread::park();
            };
        });
        ALLOCATOR.gc_thread_handle = Some(handle);
    }

    // unpark the gc thread
    ALLOCATOR.gc_thread_handle.as_ref().unwrap().thread().unpark();
    // return and continue execution
    return
}