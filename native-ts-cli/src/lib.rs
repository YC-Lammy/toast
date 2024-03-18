use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct Cli{
    /// Do not print log messages
    #[arg(short, long)]
    quiet: bool,
    /// maximum number of parallel jobs, defaults to # of CPUs.
    #[arg(short, long, default_value="4")]
    jobs: usize,
    /// check in release mode
    #[arg(short, long)]
    release: bool,
    /// specifies the build target
    #[arg(long)]
    target: Option<String>,
    /// check without 
    #[arg(long)]
    offline: bool,
    #[command(subcommand)]
    command: Commands
}

#[derive(Subcommand, Clone)]
#[command(version, about, long_about = None)]
pub enum Commands{
    /// Analyze the current package and report errors, but don't build object files
    Check(CheckCommand),
    /// Compile the current package
    Build(BuildCommand),
    /// Create a new typescipt project in an existing directory
    Init(InitCommand),
}

#[derive(Parser, Clone)]
pub struct CheckCommand{
    /// Exclude a file from check
    #[arg(short, long)]
    exclude: Option<String>,
    /// check only the libraries
    #[arg(long)]
    lib: bool,
}

#[derive(Parser, Clone)]
pub struct BuildCommand{
    /// the feature to turn on
    #[arg(long)]
    features: Option<String>
}

#[derive(Parser, Clone)]
pub struct InitCommand{
    /// place the configuration in library mode
    #[arg(long)]
    lib: bool,
    /// The project is bounded to a specific version of the compiler
    #[arg(long)]
    version: Option<String>,
    /// The project is bounded to a specific version of Typescript
    #[arg(long)]
    ts_version: Option<String>,
}