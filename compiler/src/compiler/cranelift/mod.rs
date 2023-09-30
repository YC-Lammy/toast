mod buitins;
mod runtime;

use std::collections::HashMap;

use cranelift::{codegen::ir::FuncRef, prelude::Configurable};
use cranelift_module::{FuncId, Module};

use parking_lot::RwLock;

use crate::Configuration;

pub struct Compiler {
    pub config: Configuration,
    pub module: Box<RwLock<dyn Module>>,

    pub funcs: HashMap<String, FuncId>,
}

impl Compiler {
    pub fn new(config: Configuration) -> Result<Self, String> {
        let os = std::env::consts::OS;
        let arch = std::env::consts::ARCH;

        let mut shared_builder = cranelift::codegen::settings::builder();
        shared_builder
            .set("opt_level", "speed")
            .map_err(|e| format!("error setting flags: {}", e))?;

        // build the flags
        let flags = cranelift::codegen::settings::Flags::new(shared_builder);

        let isa = if config.target_os == os && arch == config.target_arch {
            // configure the native isa
            let b = cranelift_native::builder().map_err(|e| e.to_string())?;

            // finalize and build isa
            b.finish(flags)
                .map_err(|e| format!("error configuring isa: {}", e))?
        } else {
            let mut b = cranelift::codegen::isa::lookup_by_name(&format!(
                "{}-{}-{}",
                config.target_arch, config.target_vendor, config.target_os
            ))
            .map_err(|e| format!("error looking up target: {}", e))?;

            // settings for individual arch
            if config.target_arch == "x86_64" {
                // todo: enable sse and avx features
            } else if config.target_arch == "aarch64" {
                if config.target_os == "macos" {
                    // Pointer authentication is always available on Apple Silicon.
                    b.enable("sign_return_address")
                        .map_err(|e| format!("error setting isa flags: {}", e))?;
                    // macOS enforces the use of the B key for return addresses.
                    b.enable("sign_return_address_with_bkey")
                        .map_err(|e| format!("error setting isa flags: {}", e))?;
                }
            } else if config.target_arch == "s390x" {
                if config.target_os == "linux" {}
            } else if config.target_arch == "riscv64" {
            };

            // finalize and build isa
            b.finish(flags)
                .map_err(|e| format!("error configuring isa: {}", e))?
        };

        let b = cranelift_object::ObjectBuilder::new(
            isa.clone(),
            "",
            cranelift_module::default_libcall_names(),
        );
        let b = b.map_err(|e| format!("error creating object file: {}", e))?;

        let m = cranelift_object::ObjectModule::new(b);

        let mut compiler = Self {
            config: config,
            module: Box::new(RwLock::new(m)),

            funcs: Default::default(),
        };

        runtime::build_runtime(&mut compiler)
            .map_err(|e| format!("error building runtime: {}", e))?;

        return Ok(compiler);
    }
}
