extern crate alloc;

pub mod builder;
mod context;
mod function;
pub mod mir;
pub mod types;
mod util;
mod value;

pub use builder::{Block, Builder, StackSlot};
pub use context::Context;
pub use function::{Function, Linkage};
pub use types::Type;
pub use value::Value;
