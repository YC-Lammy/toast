use std::collections::HashMap;
use std::io::Write;
use std::path::{Path, PathBuf};

use swc_core::common::{BytePos, Spanned};

pub use swc_core;

/// parser or configuration file
pub mod config;

/// unique id of module
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(usize);

/// a parsed module
#[derive(Debug)]
pub struct ParsedModule {
    /// the canonicalised name
    pub path: PathBuf,
    /// the unique id
    pub id: ModuleId,
    /// dependencies
    pub dependencies: Vec<ModuleId>,
    /// the ast of module
    pub module: swc_core::ecma::ast::Module,
}

/// a set of parsed modules
#[derive(Debug)]
pub struct ParsedProgram {
    pub modules: HashMap<ModuleId, ParsedModule>,
}

/// a temperary structure used to parse source code
#[derive(Default)]
pub struct Parser {
    /// source map holds the files and paths
    src: swc_core::common::SourceMap,
    /// the already parsed modules
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
    /// try to resolve dependency
    pub fn resolve_dependency(&self, base_path: &Path, name: &str) -> Result<PathBuf, String> {
        // it is a local file
        if name.starts_with("./") {
            return Ok(base_path.join(name));
        }

        // it is a web file
        if name.starts_with("http://") || name.starts_with("https://") {
            log::info!("GET from {}", name);

            // try to get the file from server
            match ureq::get(name).call() {
                Ok(response) => {
                    match response.into_string() {
                        // a response has been returned
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
                            // write the content to file
                            f.write_all(body.as_bytes()).expect("error writing file");

                            // return the tmp file path
                            return Ok(tmp);
                        }
                        // http error
                        Err(e) => return Err(e.to_string()),
                    }
                }
                // connection error
                Err(e) => return Err(e.to_string()),
            };
        }

        // join the path and return
        match base_path.join(name).canonicalize() {
            Ok(path) => Ok(path),
            Err(e) => Err(e.to_string()),
        }
    }

    /// parses a string file
    pub fn parse_str(mut self, name: String, src: String) -> Result<ParsedProgram, String> {
        // register the string source to source map
        let file = self
            .src
            .new_source_file(swc_core::common::FileName::Custom(name), src);

        // parse the file
        self.parse_file(PathBuf::new(), &file.src, file.start_pos, file.end_pos)?;

        // check if any cyclic dependency occoured
        self.check_cyclic_dependency()?;

        // return the parsed program
        return Ok(ParsedProgram {
            modules: self.modules,
        });
    }

    // parse a file from the gven path
    pub fn parse(mut self, main: PathBuf) -> Result<ParsedProgram, String> {
        // parse file as a module
        self.parse_module(main)?;

        // check if any cyclic dependencies occoured
        self.check_cyclic_dependency()?;

        // return the parsed program
        return Ok(ParsedProgram {
            modules: self.modules,
        });
    }

    // parse a file as module with a given path
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

        // load the file
        let file = match self.src.load_file(&path) {
            Ok(file) => file,
            // file read error
            Err(e) => return Err(e.to_string()),
        };

        // parse the file
        return self.parse_file(path, &file.src, file.start_pos, file.end_pos);
    }

    /// parse the file
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

        // parse the module
        let re = parser.parse_typescript_module();

        let mut module = match re {
            Err(e) => {
                // lookup the position
                let loc = self.src.lookup_char_pos(e.span_lo());
                // format error
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

        // loop through statements and fin dependencies
        for item in &mut module.body {
            if let swc_core::ecma::ast::ModuleItem::ModuleDecl(m) = item {
                match m {
                    // import from
                    swc_core::ecma::ast::ModuleDecl::Import(i) => {
                        let p = self.resolve_dependency(&path, &i.src.value)?;
                        i.src.raw = None;
                        i.src.value = p.to_string_lossy().into();
                        // parse the dependency
                        let id = self.parse_module(p)?;
                        dependencies.push(id);
                    }
                    // export from
                    swc_core::ecma::ast::ModuleDecl::ExportAll(e) => {
                        let p = self.resolve_dependency(&path, &e.src.value)?;
                        e.src.raw = None;
                        e.src.value = p.to_string_lossy().into();
                        // parse the dependency
                        let id = self.parse_module(p)?;
                        dependencies.push(id);
                    }
                    // export from
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

    // check if a cyclic dependency chain occoured
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

    // recurring function to find cycles
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

#[cfg(test)]
macro_rules! test_case {
    ($($id:expr => ($($dep:expr),*)),*) => {
        // construct Parser
        {let mut case = Parser{
            src: Default::default(),
            modules: Default::default()
        };
        $(
            // insert id to hashmap
            case.modules.insert(
                // module id
                ModuleId($id),
                // a dummy module
                ParsedModule{
                    // Path is same as module id
                    path: PathBuf::from(stringify!($id)),
                    // the module id
                    id: ModuleId($id),
                    // add the dependencies
                    dependencies: vec![$(ModuleId($dep)),*],
                    // dummy ast
                    module: swc_core::ecma::ast::Module{
                        span: Default::default(),
                        body: Vec::new(),
                        shebang: None
                    }
                }
            );
        )*
        case}
    };
}

#[test]
fn test1(){
    let test_case = test_case!(
        0 => (1, 2, 3),
        1 => (5),
        2 => (3, 4, 5),
        3 => (4, 5),
        4 => (),
        5 => ()
    );
    test_case.check_cyclic_dependency().expect("test failed");
}

#[test]
fn test2(){
    let test_case = test_case!(
        0 => (1, 2),
        1 => (2),
        2 => (0, 1)
    );
    test_case.check_cyclic_dependency().expect_err("test failed");
}

#[test]
fn test3(){
    let test_case = test_case!(
        0 => (1, 2),
        1 => (2),
        2 => (3, 4),
        3 => (1),
        4 => ()
    );
    test_case.check_cyclic_dependency().expect_err("test failed");
}

#[test]
fn test4(){
    let test_case = test_case!(
        0 => (4, 2),
        1 => (2),
        2 => (3, 4),
        3 => (1),
        4 => ()
    );
    test_case.check_cyclic_dependency().expect_err("test failed");
}

#[test]
fn test5(){
    let test_case = test_case!(
        0 => (),
        1 => (),
        2 => (3),
        3 => (2)
    );
    test_case.check_cyclic_dependency().expect_err("test failed");
}

#[test]
fn test6(){
    let test_case = test_case!(
        0 => (2, 3, 4, 5),
        1 => (),
        2 => (),
        3 => (),
        4 => (3, 2),
        5 => (3, 2)
    );
    test_case.check_cyclic_dependency().expect("test failed");
}

#[test]
fn test7(){
    let test_case = test_case!(
        0 => (4, 5),
        1 => (2),
        2 => (),
        3 => (1),
        4 => (2, 5),
        5 => (1, 3)
    );
    test_case.check_cyclic_dependency().expect("test failed");
}