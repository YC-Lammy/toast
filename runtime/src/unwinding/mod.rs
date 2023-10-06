

// References:
// https://github.com/rust-lang/rust/blob/c4be230b4a30eb74e3a3908455731ebc2f731d3d/library/panic_unwind/src/gcc.rs
// https://github.com/rust-lang/rust/blob/c4be230b4a30eb74e3a3908455731ebc2f731d3d/library/panic_unwind/src/dwarf/eh.rs
// https://docs.rs/gimli/0.25.0/src/gimli/read/cfi.rs.html
mod arch;
mod personality;
mod panic;
mod panicking;

pub use personality::rust_eh_personality;
pub use panic::begin_panic;

use crate::types::Any;


pub fn throw(value: Any) -> !{
    todo!()
}