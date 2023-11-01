use std::collections::HashMap;
use std::io::Write;
use std::path::{PathBuf, Path};


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(usize);

pub struct ParsedModule{
    /// the canonicalised name
    pub path: PathBuf,
    pub id: ModuleId,
    pub module: swc_ecmascript::ast::Module,
}

pub struct ParsedProgram{
    pub modules: HashMap<ModuleId, ParsedModule>,
}

#[derive(Debug, Default)]
pub struct SourceMap{
    names: HashMap<PathBuf, ModuleId>,
}



#[derive(Default)]
pub struct Parser{
    src: swc_common::SourceMap,
    src_map: SourceMap,
    modules: HashMap<ModuleId, ParsedModule>
}

impl Parser{
    pub fn new() -> Self{
        Self { 
            src: swc_common::SourceMap::new(Default::default()) ,
            src_map: Default::default(),
            modules: HashMap::new(),
        }
    }
}

impl Parser{
    pub fn resolve_dependency(&self, base_path: &Path, name: &str) -> Result<PathBuf, String>{
        if name.starts_with("./"){
            return Ok(base_path.join(name))
        }

        if name.starts_with("http://") || name.starts_with("https://"){
            log::info!("reading from {}", name);

            match ureq::get(name).call(){
                Ok(response) => {
                    match response.into_string(){
                        Ok(body) =>{
                            let tmp = std::env::temp_dir().join(std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos().to_string());
                            let mut f = std::fs::File::create(&tmp).expect("faield to open file");
                            f.write_all(body.as_bytes()).expect("error writing file");

                            return Ok(tmp)
                        }
                        Err(e) => return Err(e.to_string())
                    }
                },
                Err(e) => return Err(e.to_string()),
            };
        }

        match base_path.join(name).canonicalize(){
            Ok(path) => Ok(path),
            Err(e) => Err(e.to_string())
        }
    }

    pub fn parse(mut self, main: PathBuf) -> Result<ParsedProgram, String>{
        self.parse_module(main)?;

        return Ok(ParsedProgram { modules: self.modules })
    }

    pub fn parse_module(&mut self, path: PathBuf) -> Result<ModuleId, String>{
        // path must be canonicalised
        let path = match path.canonicalize(){
            Ok(p) => p,
            Err(e) => return Err(e.to_string())
        };

        // if id is already in source map, this module is parsed
        if let Some(id) = self.src_map.names.get(&path){
            return Ok(*id)
        }

        let file = match self.src.load_file(&path){
            Ok(file) => file,
            Err(e) => {
                return Err(e.to_string())
            }
        };

        let input = swc_common::input::StringInput::new(
            &file.src, 
            file.start_pos, 
            file.end_pos
        );

        let mut parser = swc_ecmascript::parser::Parser::new(
            swc_ecmascript::parser::Syntax::Typescript(
                swc_ecmascript::parser::TsConfig::default()
            ), 
            input, 
            None
        );
        
        let re = parser.parse_typescript_module();

        let module = match re{
            Err(e) => return Err(e.kind().msg().to_string()),
            Ok(m) => m
        };

        for item in &module.body{
            if let swc_ecmascript::ast::ModuleItem::ModuleDecl(m) = item{
                match m{
                    swc_ecmascript::ast::ModuleDecl::Import(i) => {
                        let src = self.resolve_dependency(
                            &path, 
                            &i.src.value
                        )?;
                        // parse the dependency
                        self.parse_module(src)?;
                    }
                    swc_ecmascript::ast::ModuleDecl::ExportAll(e) => {
                        let src = self.resolve_dependency(
                            &path, 
                            &e.src.value
                        )?;
                        // parse the dependency
                        self.parse_module(src)?;
                    }
                    _ => {}
                }
            }
        }
        
        let id = self.modules.len();

        self.modules.insert(ModuleId(id), 
            ParsedModule{
                path: path,
                id: ModuleId(id),
                module: module
            }
        );

        return Ok(ModuleId(id))
    }
}