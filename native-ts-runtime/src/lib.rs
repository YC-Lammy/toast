/*
 * Copyright 2023 YC. Lam. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

#![no_std]

extern crate alloc;

mod global_allocator;
pub mod asynchronous;
mod event_loop;
mod exception;
pub mod gc;
pub mod runtime;
pub mod types;
pub mod std;