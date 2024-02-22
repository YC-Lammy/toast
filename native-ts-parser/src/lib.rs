use std::collections::HashMap;
use std::io::Write;
use std::path::{Path, PathBuf};

use swc_core::common::{BytePos, Spanned};

pub use swc_core;

pub mod config;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(usize);

#[derive(Debug)]
pub struct ParsedModule {
    /// the canonicalised name
    pub path: PathBuf,
    pub id: ModuleId,
    pub dependencies: Vec<ModuleId>,
    pub module: swc_core::ecma::ast::Module,
}

#[derive(Debug)]
pub struct ParsedProgram {
    pub modules: HashMap<ModuleId, ParsedModule>,
}

#[derive(Default)]
pub struct Parser {
    src: swc_core::common::SourceMap,
    modules: HashMap<ModuleId, ParsedModule>,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            src: swc_core::common::SourceMap::new(Default::default()),
            modules: HashMap::new(),
        }
    }
}

impl Parser {
    pub fn resolve_dependency(&self, base_path: &Path, name: &str) -> Result<PathBuf, String> {
        if name.starts_with("./") {
            return Ok(base_path.join(name));
        }

        if name.starts_with("http://") || name.starts_with("https://") {
            log::info!("GET from {}", name);

            match ureq::get(name).call() {
                Ok(response) => {
                    match response.into_string() {
                        Ok(body) => {
                            // creates a random file name
                            let tmp = std::env::temp_dir().join(
                                std::time::SystemTime::now()
                                    .duration_since(std::time::UNIX_EPOCH)
                                    .unwrap()
                                    .as_nanos()
                                    .to_string(),
                            );
                            // create the tmp file
                            let mut f = std::fs::File::create(&tmp).expect("faield to open file");
                            f.write_all(body.as_bytes()).expect("error writing file");

                            return Ok(tmp);
                        }
                        Err(e) => return Err(e.to_string()),
                    }
                }
                Err(e) => return Err(e.to_string()),
            };
        }

        match base_path.join(name).canonicalize() {
            Ok(path) => Ok(path),
            Err(e) => Err(e.to_string()),
        }
    }

    pub fn parse_str(mut self, name: String, src: String) -> Result<ParsedProgram, String> {
        let file = self
            .src
            .new_source_file(swc_core::common::FileName::Custom(name), src);

        self.parse_file(PathBuf::new(), &file.src, file.start_pos, file.end_pos)?;

        self.check_cyclic_dependency()?;

        return Ok(ParsedProgram {
            modules: self.modules,
        });
    }

    pub fn parse(mut self, main: PathBuf) -> Result<ParsedProgram, String> {
        self.parse_module(main)?;

        self.check_cyclic_dependency()?;

        return Ok(ParsedProgram {
            modules: self.modules,
        });
    }

    fn parse_module(&mut self, path: PathBuf) -> Result<ModuleId, String> {
        // path must be canonicalised
        let path = match path.canonicalize() {
            Ok(p) => p,
            Err(e) => return Err(e.to_string()),
        };

        // module is already parsed
        if let Some(m) = self.modules.values().find(|m| m.path == path) {
            return Ok(m.id);
        }

        let file = match self.src.load_file(&path) {
            Ok(file) => file,
            Err(e) => return Err(e.to_string()),
        };

        return self.parse_file(path, &file.src, file.start_pos, file.end_pos);
    }

    fn parse_file(
        &mut self,
        path: PathBuf,
        input: &str,
        start: BytePos,
        end: BytePos,
    ) -> Result<ModuleId, String> {
        let input = swc_core::common::input::StringInput::new(&input, start, end);

        let mut parser = swc_core::ecma::parser::Parser::new(
            swc_core::ecma::parser::Syntax::Typescript(swc_core::ecma::parser::TsConfig::default()),
            input,
            None,
        );

        let re = parser.parse_typescript_module();

        let mut module = match re {
            Err(e) => {
                let loc = self.src.lookup_char_pos(e.span_lo());
                return Err(format!(
                    "{}:{}:{}: {}",
                    loc.file.name,
                    loc.line,
                    loc.col_display,
                    e.kind().msg()
                ));
            }
            Ok(m) => m,
        };

        let mut dependencies = Vec::new();

        for item in &mut module.body {
            if let swc_core::ecma::ast::ModuleItem::ModuleDecl(m) = item {
                match m {
                    swc_core::ecma::ast::ModuleDecl::Import(i) => {
                        let p = self.resolve_dependency(&path, &i.src.value)?;
                        i.src.raw = None;
                        i.src.value = p.to_string_lossy().into();
                        // parse the dependency
                        let id = self.parse_module(p)?;
                        dependencies.push(id);
                    }
                    swc_core::ecma::ast::ModuleDecl::ExportAll(e) => {
                        let p = self.resolve_dependency(&path, &e.src.value)?;
                        e.src.raw = None;
                        e.src.value = p.to_string_lossy().into();
                        // parse the dependency
                        let id = self.parse_module(p)?;
                        dependencies.push(id);
                    }
                    swc_core::ecma::ast::ModuleDecl::ExportNamed(n) => {
                        if let Some(src) = &mut n.src {
                            let p = self.resolve_dependency(&path, &src.value)?;
                            src.raw = None;
                            src.value = p.to_string_lossy().into();
                            // parse the dependency
                            let id = self.parse_module(p)?;
                            dependencies.push(id);
                        }
                    }
                    _ => {}
                }
            }
        }

        let id = self.modules.len();

        // insert into map
        self.modules.insert(
            ModuleId(id),
            ParsedModule {
                path: path,
                id: ModuleId(id),
                dependencies,
                module: module,
            },
        );

        return Ok(ModuleId(id));
    }

    fn check_cyclic_dependency(&self) -> Result<(), String> {
        if self.modules.len() == 0 {
            return Ok(());
        }
        // allocate visited stack
        let mut visited = Vec::with_capacity(self.modules.len());
        // allocate recurring stack
        let mut rec_stack = Vec::with_capacity(self.modules.len());

        // set all value to false
        visited.resize(self.modules.len(), false);
        rec_stack.resize(self.modules.len(), false);

        // loop over every module
        for id in self.modules.keys() {
            // not visited
            if !visited[id.0] && self.is_cyclic_until(*id, &mut visited, &mut rec_stack) {
                // cyclic dependency detected
                let mut msg = "cyclic dependency detected: ".to_string();

                // format the error message
                // find any module that is recurring
                for (i, recurring) in rec_stack.into_iter().enumerate() {
                    // is recurring
                    if recurring {
                        msg.push_str(
                            &self
                                .modules
                                .get(&ModuleId(i))
                                .unwrap()
                                .path
                                .to_string_lossy(),
                        );
                        msg.push_str(" -> ");
                    }
                }

                return Err(msg);
            }
        }

        return Ok(());
    }

    fn is_cyclic_until(&self, id: ModuleId, visited: &mut [bool], rec_stack: &mut [bool]) -> bool {
        // not visited
        if !visited[id.0] {
            // set visited
            visited[id.0] = true;
            // set recurring
            rec_stack[id.0] = true;

            // visit every dependency
            for dep in &self.modules.get(&id).unwrap().dependencies {
                // dependency not visited
                if !visited[dep.0] && self.is_cyclic_until(*dep, visited, rec_stack) {
                    return true;
                } else if rec_stack[dep.0] {
                    // dependency is recurring
                    return true;
                }
            }
        }
        // set recurring to false
        rec_stack[id.0] = false;
        return false;
    }
}
