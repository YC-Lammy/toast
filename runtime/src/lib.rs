/*
 * Copyright 2023 YC. Lam. All rights reserved.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

#![feature(allocator_api)]
#![feature(iter_collect_into)]
#![feature(c_variadic)]
#![feature(offset_of)]
#![feature(core_intrinsics)]
#![no_std]

extern crate alloc;

mod event_loop;
mod coro;
mod ecma;
mod rt;
mod types;
mod utils;
mod coroutine;
mod allocator;
mod runtime;
mod unwinding;
mod asynchronous;