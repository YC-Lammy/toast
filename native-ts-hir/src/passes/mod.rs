use crate::{ast::Program, error::Error};

pub mod class_constructor;
pub mod class_defult_initialiser;

pub fn run(program: &mut Program) -> Result<(), Vec<Error>> {
    let mut errors = Vec::new();
    if let Err(e) = class_constructor::run(program) {
        errors.extend(e);
    };

    if !errors.is_empty() {
        return Err(errors);
    }

    return Ok(());
}
