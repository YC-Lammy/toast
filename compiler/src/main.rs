mod common;
mod compiler;
mod console;
mod dependencies;
mod error;
mod ir_builder;
mod ir_optimiser;
mod parser;

#[cfg(test)]
mod test;

use std::io::Write;

use serde::{Deserialize, Serialize};

use clap::Parser;
use parser::ParsedPackage;
use swc_common::{sync::Lrc, FilePathMapping, SourceMap};

#[derive(Parser)]
#[command(name = "rtc")]
#[command(bin_name = "rtc")]
#[command(version)]
enum Cli {
    Check(CheckArgs),
    Build(BuildArgs),
    Init(InitArgs),
}

#[derive(clap::Args)]
#[command(about = "Check a local package and all of its dependencies for errors")]
struct CheckArgs {
    file: Option<String>,
}

#[derive(clap::Args)]
#[command(about = "Compile a local package and all of its dependencies")]
struct BuildArgs {
    file: Option<String>,

    #[arg(long, short, help = "build in release mode")]
    release: bool,

    #[arg(long, help = "emit bundled source code.")]
    emit_bundled_js: bool,
    #[arg(long, help = "emit compiler ir codes.")]
    emit_ir: bool,
    #[arg(long, help = "emit llvm ir codes.")]
    emit_llvm_ir: bool,
}

#[derive(clap::Args)]
#[command(about = "Initialize a local package")]
struct InitArgs {
    #[arg(long, short, help = "library package")]
    lib: bool,
}

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct Configuration {
    pub emit_bundled_js: bool,
    pub emit_ir: bool,
    pub emit_llvm_ir: bool,

    pub target_arch: &'static str,
    pub target_feature: &'static str,
    pub target_os: &'static str,
    pub target_family: &'static str,
    pub target_env: &'static str,
    pub target_endian: &'static str,
    pub target_pointer_width: u16,
    pub target_vendor: &'static str,
    pub test: bool,
    pub release: bool,
    pub panic_abort: bool,

    pub ecma_8: bool,
    pub ecma_9: bool,
    pub ecma_10: bool,
    pub ecma_11: bool,
    pub ecma_12: bool,
    pub ecma_13: bool,

    pub opt_level: u8,
    pub garbage_collect_multithread: bool,

    pub features: Vec<String>,
}

impl Configuration {
    fn from_args(args: &BuildArgs) -> Self {
        Self {
            emit_bundled_js: args.emit_bundled_js,
            emit_ir: args.emit_ir,
            emit_llvm_ir: args.emit_llvm_ir,

            release: args.release,
            ..Default::default()
        }
    }
    pub fn matches(&self, name: &str, value: &str) -> bool {
        match name {
            "target_arch" => self.target_arch == value,
            "target_feature" => self.target_feature == value,
            "target_os" => self.target_os == value,
            "target_family" => self.target_family == value,
            "target_env" => self.target_env == value,
            "target_endian" => self.target_endian == value,
            "target_pointer_width" => self.target_pointer_width.to_string() == value,
            "target_vendor" => self.target_vendor == value,
            "test" => self.test,
            "panic" => {
                value == "abort" && self.panic_abort || value == "unwind" && !self.panic_abort
            }
            "es8" => self.ecma_8,
            "es9" => self.ecma_9,
            "es10" => self.ecma_10,
            "es11" => self.ecma_11,
            "es12" => self.ecma_12,
            "es13" => self.ecma_13,
            "feature" => self.features.iter().any(|f| f == value),
            _ => false,
        }
    }
}

fn main() {
    let cli = Cli::parse();
    let cli_start_time = std::time::Instant::now();

    match cli {
        Cli::Check(args) => {
            let config = Configuration::default();

            let main_file = args
                .file
                .as_ref()
                .map(|f| f.as_str())
                .unwrap_or("./index.ts");

            let re = parse_and_type_check(config.clone(), main_file);

            let (package, srcmap) = match re {
                Ok(m) => m,
                Err(e) => {
                    print_error(e);
                    std::process::abort();
                }
            };

            let ir_builder = ir_builder::IRBuilder::new(config.clone());
            let re = ir_builder.build(package);

            let _package = match re {
                Ok(m) => m,
                Err(e) => {
                    println!("{}", e.display(&srcmap));
                    std::process::abort();
                }
            };

            println!(
                "Finished dev [check + debuginfo] target(s) in {}s",
                cli_start_time.elapsed().as_secs_f32()
            );
        }

        Cli::Build(args) => {
            let config = Configuration::from_args(&args);

            let main_file = args
                .file
                .as_ref()
                .map(|f| f.as_str())
                .unwrap_or("./index.ts");

            let re = parse_and_type_check(config.clone(), main_file);

            let (package, srcmap) = match re {
                Ok(m) => m,
                Err(e) => {
                    print_error(e);
                    std::process::abort();
                }
            };

            let ir_builder = ir_builder::IRBuilder::new(config.clone());
            let re = ir_builder.build(package);

            let mut package = match re {
                Ok(m) => m,
                Err(e) => {
                    println!("{}", e.display(&srcmap));
                    std::process::abort();
                }
            };

            ir_optimiser::optimise_package(&mut package);

            #[cfg(test)]
            println!("{:#?}", package);

            compiler::llvm::compile(config, package);

            println!(
                "Finished dev [build] target(s) in {}s",
                cli_start_time.elapsed().as_secs_f32()
            );
        }

        Cli::Init(_args) => {
            if !std::path::Path::new("tsconfig.toml").exists() {
                if std::path::Path::new("tsconfig.json").exists() {
                    return;
                }
            }

            match std::fs::OpenOptions::new()
                .create_new(true)
                .write(true)
                .open("tsconfig.toml")
            {
                Ok(mut f) => {
                    match f.write_all(include_bytes!("../assets/tsconfig.toml")) {
                        Err(e) => {
                            println!("failed to create tsconfig.toml: {}", e);
                            std::process::abort();
                        }
                        _ => {}
                    };
                }
                Err(e) => {
                    println!("failed to create tsconfig.toml: {}", e);
                    std::process::abort();
                }
            };

            if !std::path::Path::new("./index.ts").exists() {
                std::fs::File::create("").err().map(|e| {
                    println!("failed to create index.ts: {}", e);
                    std::process::abort();
                });
            }
        }
    };
}

pub fn print_error(err: anyhow::Error) {
    println!("error: {}", err);
    err.chain().for_each(|e| {
        println!("{}", e);
    });
}

pub fn parse_and_type_check(
    config: Configuration,
    main_file: &str,
) -> Result<(ParsedPackage, Lrc<SourceMap>), anyhow::Error> {
    println!("using {} as main.", main_file);

    let cm = Lrc::new(SourceMap::new(FilePathMapping::empty()));

    let package = parser::ParsedPackage::new(config.clone(), main_file, cm.clone())?;

    return Ok((package, cm));
}
