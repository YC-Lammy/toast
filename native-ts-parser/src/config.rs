use std::collections::HashMap;

use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct Config {
    pub project: ProjectConfig,
    #[serde(default)]
    pub lib: LibConfig,
    #[serde(default)]
    pub bin: BinConfig,
    #[serde(default)]
    pub dependencies: Dependencies,
    #[serde(default)]
    pub target: Target,
    #[serde(default)]
    pub features: Features,
    #[serde(default)]
    pub profile: ProfileConfig,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ProjectConfig {
    pub name: String,
    pub version: String,
    pub authors: Option<Vec<String>>,
    #[serde(rename = "compiler-version")]
    pub compiler_version: Option<String>,
    #[serde(rename = "ts-version")]
    pub ts_version: Option<String>,
    pub description: Option<String>,
    pub documentation: Option<String>,
    pub readme: Option<String>,
    pub homepage: Option<String>,
    pub repository: Option<String>,
    pub licence: Option<String>,
    #[serde(rename = "licence-file")]
    pub licence_file: Option<String>,
    pub keywords: Option<Vec<String>>,
    pub categories: Option<Vec<String>>,
    pub exclude: Option<Vec<String>>,
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct LibConfig {
    #[serde(default)]
    pub test: bool,
    pub lib_type: Option<String>,
    pub features: Option<Vec<String>>,
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct BinConfig {
    #[serde(default)]
    pub test: bool,
    pub features: Option<Vec<String>>,
}

pub type Dependencies = HashMap<String, Dependency>;

#[derive(Debug, Serialize, Deserialize)]
pub struct Dependency {
    pub path: Option<String>,
    pub url: Option<String>,
    pub git: Option<String>,
    pub version: Option<String>,
    pub features: Option<Vec<String>>,
    #[serde(default)]
    pub optional: bool,
}

#[derive(Debug, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct Target {
    pub windows: TargetConfig,
    pub unix: TargetConfig,
    pub linux: TargetConfig,
    pub darwin: TargetConfig,
    pub macos: TargetConfig,
    pub ios: TargetConfig,
    pub freebsd: TargetConfig,
    pub openbsd: TargetConfig,
    pub redox: TargetConfig,
    pub android: TargetConfig,
    pub x86: TargetConfig,
    pub x86_64: TargetConfig,
    pub arm: TargetConfig,
    pub aarch64: TargetConfig,
    pub riscv: TargetConfig,
    pub wasm32: TargetConfig,
}

#[derive(Debug, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct TargetConfig {
    pub dependencies: Option<Dependencies>,
    pub features: Option<Vec<String>>,
}

pub type Features = HashMap<String, Vec<String>>;

#[derive(Default, Debug, Serialize, Deserialize)]
#[serde(default)]
pub struct ProfileConfig {
    #[serde(default)]
    pub debug: Profile,
    #[serde(default)]
    pub release: Profile,
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Profile {
    #[serde(rename = "opt-level")]
    pub opt_level: Option<u8>,
    pub debug: Option<bool>,
    pub lto: Option<bool>,
    pub incremental: Option<bool>,
}

#[test]
pub fn test() {
    let s = r#"
    [project]
name = "win"
version = "0.1.1"

[profile.debug]
opt-level = 3
"#;

    let config: Config = toml::from_str(s).expect("error");
    println!("{}", toml::to_string_pretty(&config).unwrap());
}
