extern crate alloc;

pub mod builder;
mod context;
mod function;
pub mod mir;
pub mod passes;
pub mod runtime;
pub mod types;
pub mod backend;
mod util;
mod value;

pub use builder::{Block, Builder, StackSlot};
pub use context::{Context, Linkage};
pub use function::Function;
pub use types::Type;
pub use value::Value;
