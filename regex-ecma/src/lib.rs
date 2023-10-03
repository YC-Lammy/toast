mod ast;
mod compiler;
pub mod parser;
pub mod ir;
//pub mod clousure;

pub struct IRRegex {}

impl IRRegex {
    pub fn new(pattern: &[char], flags: &str) -> Result<Self, String> {
        let mut i = false;
        let mut m = false;
        let mut s = false;
        let mut u = false;
        for c in flags.chars() {
            match c {
                'd' => {}
                'g' => {}
                'i' => i = true,
                'm' => m = true,
                's' => s = true,
                'u' => u = true,
                'y' => {}
                _ => return Err(format!("unknown regex flag: {}", c)),
            }
        }

        let pat = parser::parse(pattern, u);

        let pat = match pat {
            Ok(p) => p,
            Err(e) => return Err(e.to_string()),
        };

        let mut compiler = compiler::Compiler::new(i, m, s, u);

        compiler.compile(&pat);

        todo!()
    }
}