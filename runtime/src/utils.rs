/*
 * Copyright 2023 YC. Lam. All rights reserved.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

// Provides utility types not supported under no_std environment

use core::alloc::{Allocator, Layout};

use hashbrown::hash_map::DefaultHashBuilder;

use iron_gc::GcPtr;

#[derive(Debug, Clone, Copy, Default)]
pub struct GcAllocator;

unsafe impl core::alloc::Allocator for GcAllocator{
    fn allocate(&self, layout: Layout) -> Result<core::ptr::NonNull<[u8]>, core::alloc::AllocError> {
        unsafe{
            let ptr = GcPtr::<u8>::malloc_array(layout.size());
            ptr.set_root();
            
            return Ok(core::mem::transmute(core::slice::from_raw_parts(ptr.as_mut_ptr(), layout.size())))
        }
    }

    unsafe fn deallocate(&self, ptr: core::ptr::NonNull<u8>, layout: Layout) {
        
    }
}

pub type GcHashMap<K, V> = hashbrown::HashMap<K, V, DefaultHashBuilder, GcAllocator>;