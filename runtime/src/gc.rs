/*
 * Copyright 2023 YC. Lam. All rights reserved.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

// api wrapper for the bdwgc allocator

use crate::types::Any;

use core::alloc::Layout;
use core::marker::PhantomData;
use core::sync::atomic::{
    AtomicBool,
    AtomicUsize, Ordering
};

use alloc::collections::VecDeque;
use alloc::vec::Vec;
use parking_lot::{Mutex, RwLock};

/// allocater is a state structure that implements alloc::Allocator interface for use with structures such as HashMap and Vec.
#[derive(Debug, Clone, Copy, Default)]
pub struct Allocater;

impl minicoroutine::Allocator for Allocater{
    unsafe fn allocate(&self, size:usize) -> *mut u8 {
        unsafe{
            let ptr = GcPtr::<u8>::malloc_array(size);
            ptr.set_root();
            return ptr.as_mut()
        }
    }

    unsafe fn deallocate(&self, ptr:*mut u8) {
        
    }
}

// the lib c global
struct LibcAllocator;

unsafe impl core::alloc::GlobalAlloc for LibcAllocator{
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        libc::malloc(layout.size()) as _
    }

    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        let s = layout.size();
        let ptr = libc::malloc(s);
        libc::memset(ptr, 0, s);
        return ptr as _
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        libc::free(ptr as _)
    }

    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        libc::realloc(ptr as _, new_size) as _
    }
}

#[global_allocator]
static LIBC_GLOBAL:LibcAllocator = LibcAllocator;

/// safe binding to a garbage collected pointer.
#[repr(transparent)]
pub struct GcPtr<T>{
    masked: usize,
    _d: PhantomData<T>
}

#[repr(C)]
pub struct GcCell<T>{
    /// is the cell marked by GC
    is_marked: AtomicBool,
    /// if true, the cell is alive
    is_new: AtomicBool,
    /// the cell is in the remember queue (write barrier)
    is_remembered: AtomicBool,
    /// the data does not contain pointers of atomic
    is_atomic: bool,

    finaliser: fn(T),

    /// user data
    data: T
}

fn place_holder_finaliser<T>(v:T){

}

#[repr(C)]
struct Block<const SIZE:usize>{
    is_new: bool,
    mark_version: AtomicUsize,
    new_version: AtomicUsize,
    data: *mut [u8;4096*4],
    cursor: usize,
}

struct Heap<const SIZE:usize>{
    blocks: RwLock<Vec<Block<SIZE>>>,
    list: *mut GcCell<*mut GcCell<()>>,
}

struct Eden{
    version: AtomicUsize,
    // the total data
    data: *mut [u8;4096 * 512],
    /// current offset in bytes
    cursor: usize,
}

const MASK_BITS:usize = 0b1111111111111111000000000000000000000000000000000000000000000000;

pub struct GcAllocator{
    is_gc_running: AtomicBool,
    is_full_gc_running: AtomicBool,
    is_gc_mark_phase: AtomicBool,
    should_collect: AtomicBool,

    magic_mask1: usize,
    magic_mask2: usize,
    magic_mask3: usize,

    heap_16: Heap<16>,
    heap_32: Heap<32>,
    heap_64: Heap<64>,
    heap_128: Heap<128>,
    heap_256: Heap<256>,
    heap_512: Heap<512>,
    heap_1024: Heap<1024>,
    heap_2048: Heap<2048>,
    allocations: RwLock<Vec<*mut GcCell<()>>>,

    stack_bottom: *mut u64,

    num_blocks: usize,

    mark_version: AtomicUsize,
    new_version: AtomicUsize,

    /// the smallest address overall
    heap_min: usize,
    /// the largest address overall
    heap_max: usize,

    roots: RwLock<VecDeque<*mut GcCell<()>>>,
    queue: VecDeque<*mut GcCell<()>>,
    remembered: RwLock<VecDeque<*mut GcCell<()>>>,
}

unsafe impl Sync for GcAllocator{}
unsafe impl Send for GcAllocator{}

static mut GC:GcAllocator = GcAllocator{
    is_gc_running: AtomicBool::new(false),
    is_full_gc_running: AtomicBool::new(false),
    is_gc_mark_phase: AtomicBool::new(false),
    should_collect: AtomicBool::new(false),
    
    magic_mask1: Any::OBJECT_TAG as usize,
    magic_mask2: Any::STRING_TAG as usize,
    magic_mask3: Any::BIGINT_TAG as usize,

    heap_16: Heap::new(),
    heap_32: Heap::new(),
    heap_64: Heap::new(),
    heap_128: Heap::new(),
    heap_256: Heap::new(),
    heap_512: Heap::new(),
    heap_1024: Heap::new(),
    heap_2048: Heap::new(),
    allocations: RwLock::new(Vec::new()),

    stack_bottom: 0 as _,

    num_blocks: 0,

    mark_version: AtomicUsize::new(0),
    new_version: AtomicUsize::new(0),

    heap_min: 0,
    heap_max: 0,

    roots: RwLock::new(VecDeque::new()),
    queue: VecDeque::new(),
    remembered:RwLock::new(VecDeque::new())

};

impl<T> GcCell<T>{
    #[inline]
    pub fn is_alive(&self) -> bool{
        self.is_marked.load(Ordering::SeqCst) || self.is_new.load(Ordering::SeqCst)
    }

    #[inline]
    pub fn is_dead(&self) -> bool{
        !self.is_alive()
    }

    #[inline]
    unsafe fn write_barriar<A>(&self, other:&GcCell<A>){
        if self.is_remembered.load(Ordering::SeqCst){
            return;
        }

        if self.is_marked.load(Ordering::SeqCst){
            if !other.is_marked.load(Ordering::SeqCst){
                if !self.is_remembered.swap(true, Ordering::SeqCst){

                    if GC.is_gc_running(){
                        // acquire lock
                        let mut remembered = GC.remembered.write();

                        // push pointer to remembered list
                        remembered.push_back(other as *const GcCell<A> as *mut GcCell<()>);

                        // release lock immediately
                        drop(remembered);

                    } else{
                        GC.remembered.get_mut().push_back(other as *const GcCell<A> as _);
                    }
                }
            }
        }
    }
}

impl<const SIZE:usize> Block<SIZE>{
    #[inline]
    unsafe fn cell_contains_live_object(&self, cell:&GcCell<()>) -> bool{
        if self.new_version.load(Ordering::SeqCst) == GC.new_version.load(Ordering::SeqCst){
            return cell.is_new.load(Ordering::SeqCst)
        }

        if self.is_markbit_logically_cleared(){
            return false
        }

        return cell.is_marked.load(Ordering::SeqCst)
    }

    #[inline]
    unsafe fn is_markbit_logically_cleared(&self) -> bool{
        if self.mark_version.load(Ordering::SeqCst) == GC.mark_version.load(Ordering::SeqCst){
            return false
        }

        if GC.is_full_gc_running() && GC.is_gc_mark_phase(){
            self.mark_version.store(GC.mark_version.load(Ordering::SeqCst) -1 , Ordering::SeqCst);
            return false
        }

        return true
    }

    #[inline]
    unsafe fn about_to_mark(&self){
        let g_mark_version = GC.mark_version.load(Ordering::SeqCst);
        if self.mark_version.load(Ordering::SeqCst) == g_mark_version{
            return
        }

        // check if is_mark is logically cleared
        if self.is_markbit_logically_cleared(){
            for i in 0..(4096 * 4)/SIZE{
                let ptr = (self.data as *mut u8).add(SIZE * i);
                let ptr = (ptr as *mut GcCell<()>).as_mut().unwrap_unchecked();
                // is_marked = false
                ptr.is_marked.store(false, Ordering::SeqCst);
            }
        } else{
            if self.new_version.load(Ordering::SeqCst) < GC.new_version.load(Ordering::SeqCst){

                for i in 0..(4096 * 4)/SIZE{
                    let ptr = (self.data as *mut u8).add(SIZE * i);
                    let ptr = (ptr as *mut GcCell<()>).as_mut().unwrap_unchecked();
                    // is_new = is_marked
                    ptr.is_new.store(ptr.is_marked.load(Ordering::SeqCst), Ordering::SeqCst);
                    // is_marked = false
                    ptr.is_marked.store(false, Ordering::SeqCst);
                }

                let gc_new_v = GC.new_version.load(Ordering::SeqCst);
                self.new_version.store(gc_new_v, Ordering::SeqCst);
            } else{

                for i in 0..(4096 * 4)/SIZE{
                    let ptr = (self.data as *mut u8).add(SIZE * i);
                    let ptr = (ptr as *mut GcCell<()>).as_mut().unwrap_unchecked();
                    // is_marked = false
                    ptr.is_marked.store(false, Ordering::SeqCst);
                }
            }
        }

        // finish updating marks
        self.mark_version.store(g_mark_version, Ordering::SeqCst);
    }
}

impl<const SIZE:usize> Heap<SIZE>{
    const fn new() -> Self{
        Self { 
            blocks: RwLock::new(Vec::new()),
            list: core::ptr::null_mut()
        }
    }

    #[inline]
    const fn num_cell(&self) -> usize{
        return (4096 * 4) / SIZE
    }

    #[inline]
    unsafe fn about_to_mark(&self){
        let blocks = self.blocks.read();
        for i in blocks.iter(){
            i.about_to_mark();
        }
        drop(blocks);
    }

    unsafe fn grow(&mut self){

        let addr = libc::malloc(4096 * 4) as *mut [u8;4096*4];
        GC.heap_min = usize::min(addr as usize, GC.heap_min);
        GC.heap_max = usize::max(addr as usize + 4096 * 4, GC.heap_max);

        GC.num_blocks += 1;

        libc::memset(addr as _, 0, 4096 * 4);

        if GC.is_gc_running(){
            let mut lock = self.blocks.write();

            lock.push(Block {
                is_new: true,
                new_version: AtomicUsize::new(0), 
                mark_version: AtomicUsize::new(0),
                data: addr,
                cursor: 0,
            });
            drop(lock);

        } else{
            self.blocks.get_mut().push(Block { 
                is_new: true,
                new_version: AtomicUsize::new(0), 
                mark_version: AtomicUsize::new(0),
                data: addr,
                cursor: 0,
            });
        }

        if GC.num_blocks >= 32 && GC.num_blocks % 32 == 0{
            // minor gc
        } else if GC.num_blocks >= 128 && GC.num_blocks % 128 == 0{
            // major gc
            GC.full_gc();
        }
    }

    unsafe fn allocate(&mut self) -> *mut GcCell<()>{

        if !self.list.is_null(){
            let cell = self.list;
            self.list = cell.read().data as *mut GcCell<*mut GcCell<()>>;
            
            let cell = cell as *mut GcCell<()>;

            cell.write(GcCell { 
                is_marked: AtomicBool::new(false), 
                is_new: AtomicBool::new(true), 
                is_remembered: AtomicBool::new(false), 
                is_atomic: false,
                finaliser: place_holder_finaliser, 
                data: () 
            });

            return cell
        }

        if self.blocks.get_mut().is_empty(){
            self.grow()
        }
        
        let mut block = self.blocks.get_mut().last_mut().unwrap_unchecked();
        
        if block.is_new{
            block.is_new = false;
            block.new_version.store(GC.new_version.load(Ordering::SeqCst), Ordering::SeqCst);
        }

        while block.cursor <= (4096 * 4)/ SIZE{
            let ptr = (block.data as *mut u8).add(block.cursor * SIZE) as *mut GcCell<()>;

            if !block.cell_contains_live_object(ptr.as_ref().unwrap_unchecked()){
                ptr.write(GcCell { 
                    is_marked: AtomicBool::new(false), 
                    is_new: AtomicBool::new(true), 
                    is_remembered: AtomicBool::new(false), 
                    is_atomic: false, 
                    finaliser: place_holder_finaliser,
                    data: () 
                });

                return ptr
            }
            block.cursor += 1;
        };

        self.grow();

        block = self.blocks.get_mut().last_mut().unwrap_unchecked();
        
        if block.is_new{
            block.is_new = false;
            block.new_version.store(GC.new_version.load(Ordering::SeqCst), Ordering::SeqCst);
        }

        let ptr = block.data as *mut GcCell<()>;

        ptr.write(GcCell { 
            is_marked: AtomicBool::new(false), 
            is_new: AtomicBool::new(true), 
            is_remembered: AtomicBool::new(false), 
            is_atomic: false, 
            finaliser: place_holder_finaliser,
            data: () 
        });

        block.cursor += 1;

        return ptr
    }
}

impl GcAllocator{
    pub fn is_gc_running(&self) -> bool{
        self.is_gc_running.load(Ordering::SeqCst)
    }

    pub fn is_full_gc_running(&self) -> bool{
        self.is_full_gc_running.load(Ordering::SeqCst)
    }

    pub fn is_gc_mark_phase(&self) -> bool{
        self.is_gc_mark_phase.load(Ordering::SeqCst)
    }

    pub fn allocate_ty<T>(&mut self) -> GcPtr<T>{
        let size = core::mem::size_of::<GcCell<T>>();
        let ptr = self.allocate(size);
        GcPtr { 
            masked: ptr.masked, 
            _d: PhantomData 
        }
    }
    
    pub fn allocate(&mut self, size:usize) -> GcPtr<()>{

        if self.should_collect.swap(false, Ordering::SeqCst){
            self.gc_collection();
        }

        unsafe{
            let ptr = if size <= 16{
                self.heap_16.allocate()
            } else if size <= 32{
                self.heap_32.allocate()
            } else if size <= 64{
                self.heap_64.allocate()
            } else if size <= 128{
                self.heap_128.allocate()
            } else if size <= 256{
                self.heap_256.allocate()
            } else if size <= 512{
                self.heap_512.allocate()
            } else if size <= 1024{
                self.heap_1024.allocate()
            } else if size <= 2048{
                self.heap_2048.allocate()
            } else{
                let ptr = libc::malloc(size) as *mut GcCell<()>;
                ptr.write(GcCell { 
                    is_marked: AtomicBool::new(false), 
                    is_new: AtomicBool::new(true), 
                    is_remembered: AtomicBool::new(false), 
                    is_atomic: false, 
                    finaliser: place_holder_finaliser,
                    data: () 
                });
                ptr
            };

            return GcPtr{
                masked: ptr as usize | self.magic_mask1,
                _d: PhantomData
            };
        }
    }

    fn is_ptr_in_range(&self, ptr:*const ()) -> bool{
        let s = ptr as usize;

        // fast path
        if s >= self.heap_min && s <= self.heap_max{
            return true
        }

        return false
    }

    unsafe fn full_gc(&mut self){
        self.is_gc_running.store(true, Ordering::SeqCst);
        self.is_full_gc_running.store(true, Ordering::SeqCst);
        self.full_gc_prepare_mark();

        self.mark_static_roots();
    }

    unsafe fn young_gc(&mut self){
        self.is_gc_running.store(true, Ordering::SeqCst);
        self.is_full_gc_running.store(true, Ordering::SeqCst);

        self.mark_static_roots();
    }

    fn full_gc_prepare_mark(&mut self){
        let allocs = self.allocations.read();
        for p in allocs.as_slice(){
            unsafe{
                let p = p.as_mut().unwrap_unchecked();
                p.is_new.store(p.is_new.load(Ordering::SeqCst) | p.is_marked.load(Ordering::SeqCst), Ordering::SeqCst);
                p.is_marked.store(false, Ordering::SeqCst);
            }
        }
        drop(allocs);

        self.mark_version.fetch_add(1, Ordering::SeqCst);
    }

    #[inline]
    fn is_value_ptr(&self, value: usize) -> Option<*mut GcCell<()>>{
        let mask = value & MASK_BITS;
        let unmasked = value & (!MASK_BITS);

        if mask == self.magic_mask1 || mask == self.magic_mask2 || mask == self.magic_mask3{
            if unmasked >= self.heap_min && value < self.heap_max{
                return Some(unmasked as _)
            }
        }
        None
    }

    fn append_child_to_queue(&mut self, cell:&mut GcCell<()>){
        todo!()
    }

    /// the world is stoped while doing so
    unsafe fn mark_static_roots(&mut self){
        let mut stack_top = 0u64;
        let stack_top = &mut stack_top as *mut u64;

        self.is_gc_mark_phase.store(true, Ordering::SeqCst);

        let mut rax:u64;
        let mut rbx:u64;
        let mut rcx:u64;
        let mut rdx:u64;
        let mut rsi:u64;
        let mut rdi:u64;
        let mut r8:u64;
        let mut r9:u64;
        let mut r10:u64;
        let mut r11:u64;


        // scan the registers
        core::arch::asm!(
            "mov {0}, rax",
            "mov {1}, rbx",
            "mov {2}, rcx",
            "mov {3}, rdx",
            "mov {4}, rsi",
            "mov {5}, rdi",
            "mov {6}, r8",
            "mov {7}, r9",
            "mov {8}, r10",
            "mov {9}, r11",
            out(reg) rax,
            out(reg) rbx,
            out(reg) rcx,
            out(reg) rdx,
            out(reg) rsi,
            out(reg) rdi,
            out(reg) r8,
            out(reg) r9,
            out(reg) r10,
            out(reg) r11,
        );

        if let Some(ptr) = self.is_value_ptr(rax as usize){
            self.queue.push_back(ptr);
        }

        if self.stack_bottom == 0 as _{
            panic!("Gc malloc must be called after init()")
        }


    }

    unsafe fn gc_mark(&mut self){
        // does nothing in eden collection since 
        self.heap_16.about_to_mark();
        self.heap_32.about_to_mark();
        self.heap_64.about_to_mark();
        self.heap_128.about_to_mark();
        self.heap_256.about_to_mark();
        self.heap_512.about_to_mark();
        self.heap_1024.about_to_mark();
        self.heap_2048.about_to_mark();

        let mut i = 0;
        loop{
            let roots = self.roots.read();

            if i >= roots.len(){
                break;
            }

            if let Some(p) = roots.get(i){
                let cell = p.as_mut().unwrap_unchecked();

                if !cell.is_marked.swap(true, Ordering::SeqCst){
                    // add children to queue
                    GC.append_child_to_queue(cell);
                };
            }
            i += 1;
        }

        loop{
            
            let mut remembered = self.remembered.write();

            if self.queue.is_empty() && remembered.is_empty(){
                break;
            }

            let cell: &mut GcCell<()>;

            if let Some(p) = self.queue.pop_front(){
                cell = p.as_mut().unwrap_unchecked();
                drop(remembered);

            } else{
                if let Some(p) = remembered.pop_front(){
                    cell = p.as_mut().unwrap_unchecked();
                } else{
                    unreachable!()
                }
                drop(remembered);
            }

            if !cell.is_marked.swap(true, Ordering::SeqCst){
                // add chidren to queue
                GC.append_child_to_queue(cell);
            };

            i += 1;
        }

        self.is_gc_mark_phase.store(false, Ordering::SeqCst);
    }

    unsafe fn gc_sweep(&self){
        
        self.should_collect.store(true, Ordering::SeqCst);
    }

    fn gc_collection(&self){
        let mut allocs = self.allocations.write();

        let mut i = 0;
        while i < allocs.len(){
            unsafe{
                let p = allocs[i];
                let cell = p.as_mut().unwrap_unchecked();
                
                if cell.is_marked.load(Ordering::SeqCst){
                    cell.is_new.store(false, Ordering::SeqCst);
                    i += 1;
                } else{
                    libc::free(cell as *mut GcCell<()> as *mut _);
                    allocs.swap_remove(i);
                }
            }
        }
        drop(allocs);

        self.new_version.fetch_add(1, Ordering::SeqCst);

        self.is_gc_running.store(false, Ordering::SeqCst);
        self.is_full_gc_running.store(false, Ordering::SeqCst);
    }
}

pub fn init(){
    unsafe{
        let mut bottom = 0u64;
        GC.stack_bottom = &mut bottom as *mut u64;
    }
}

impl<T> Clone for GcPtr<T>{
    fn clone(&self) -> Self {
        GcPtr { masked: self.masked, _d: PhantomData }
    }
}
impl<T> Copy for GcPtr<T>{}

impl<T> PartialEq for GcPtr<T>{
    fn eq(&self, other: &Self) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl<T> Eq for GcPtr<T>{}

impl<T> GcPtr<T>{
    pub fn new(value:T) -> Self{
        unsafe{
            let ptr:GcPtr<T> = GC.allocate_ty();
            ptr.as_cell_mut().finaliser = drop;
            ptr.as_mut().write(value);
            return ptr
        }
    }

    pub fn new_array(slice:&[T]) -> Self{
        unsafe{
            let ptr:GcPtr<T> = GC.allocate(slice.len() * core::mem::size_of::<T>()).cast();
            core::ptr::copy_nonoverlapping(slice.as_ptr(), ptr.as_mut(), slice.len());
            return ptr
        }
    }

    pub unsafe fn malloc() -> Self{
        GC.allocate_ty()
    }

    pub unsafe fn malloc_array(size:usize) -> Self{
        GC.allocate(size * core::mem::size_of::<T>()).cast()
    }

    pub unsafe fn cast<A>(self) -> GcPtr<A>{
        GcPtr { masked: self.masked, _d: PhantomData }
    }

    pub unsafe fn from_cell(ptr: *mut GcCell<T>) -> GcPtr<T>{
        return GcPtr { 
            masked: ptr as usize | GC.magic_mask1, 
            _d: PhantomData
        }
    }

    pub unsafe fn from_ptr(ptr:*mut T) -> Option<GcPtr<T>>{
        if GC.is_ptr_in_range(ptr as _){
            let offset = core::mem::offset_of!(GcCell<T>, data);
            let ptr = (ptr as *mut u8).sub(offset);

            return Some(GcPtr { 
                masked: ptr as usize | GC.magic_mask1, 
                _d: PhantomData
            })
        }

        return None
    }

    #[inline]
    pub fn write_barriar<A>(&self, other:GcPtr<A>){
        unsafe{self.as_cell().write_barriar(other.as_cell())}
    }

    pub fn as_cell(&self) -> &GcCell<T>{
        self.as_cell_mut()
    }

    pub fn as_cell_mut(&self) -> &mut GcCell<T>{
        let ptr = self.masked & 0b0000000000000000111111111111111111111111111111111111111111111111;
        let ptr = ptr as *mut GcCell<T>;
        return unsafe{ptr.as_mut().unwrap_unchecked()}
    }

    pub fn as_ptr(self) -> *const T{
        &self.as_cell().data
    }

    pub fn as_mut(self) -> *mut T{
        &mut self.as_cell_mut().data
    }

    /// set self as a gc root
    pub fn set_root(self){
        unsafe{
            if GC.is_gc_running(){
                GC.roots.write().push_back(self.as_cell() as *const GcCell<T> as *mut GcCell<()>);
            } else{
                GC.roots.get_mut().push_back(self.as_cell() as *const GcCell<T> as *mut GcCell<()>);
            }
        }
        
    }
}

impl<T:Copy> GcPtr<T>{
    pub fn read(self) -> T{
        unsafe{self.as_ptr().read()}
    }
}

impl<T> GcPtr<GcPtr<T>>{
    pub fn as_double_ptr(self) -> *const *const T{
        unsafe{core::mem::transmute(self.as_ptr())}
    }
}

impl<T> core::ops::Deref for GcPtr<T>{
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe{self.as_ptr().as_ref().unwrap_unchecked()}
    }
}

impl<T> core::ops::DerefMut for GcPtr<T>{
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe{self.as_mut().as_mut().unwrap_unchecked()}
    }
}

unsafe impl<T:Sync> Sync for GcPtr<T>{}
unsafe impl<T:Send> Send for GcPtr<T> {}

#[test]
fn test_size(){

}