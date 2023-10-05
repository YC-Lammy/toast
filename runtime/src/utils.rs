/*
 * Copyright 2023 YC. Lam. All rights reserved.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

// Provides utility types not supported under no_std environment

use core::alloc::Layout;

use hashbrown::hash_map::DefaultHashBuilder;

use iron_gc::GcPtr;