mod inlining;
mod dce;

use itertools::Itertools;

use crate::ir_builder::IRPackage;

pub fn optimise_package(pkg: &mut IRPackage) {
    
    inlining::resolve_maybe_static_call(pkg);
    remove_duplicate_capture_pass(pkg);

    inlining::inline_static_functions(pkg);

    dce::remove_dead_code_pass(pkg);
}

fn remove_duplicate_capture_pass(pkg: &mut IRPackage) {
    pkg.heap_variables = pkg.heap_variables.iter().map(|v| *v).unique().collect();

    for (_id, f) in &mut pkg.functions {
        f.captures = f.captures.iter().map(|v| *v).unique().collect();
    }
}