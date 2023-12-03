

extern crate alloc;

mod ast;
pub mod compiler;
//pub mod parser;
pub mod pest_parser;
pub mod ir;
//pub mod clousure;

mod optimise;
mod mir;

pub struct IRRegex {}

impl IRRegex {
    pub fn new(pattern: &str, flags: &str) -> Result<Self, String> {
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

        let pat = pest_parser::parse(pattern, u);

        let mut pat = match pat {
            Ok(p) => p,
            Err(e) => return Err(e.to_string()),
        };

        optimise::optimise(&mut pat);

        let mut generator = mir::IRGenerator::new(i, m, s, u);
        generator.translate_pattern(&pat);

        println!("{:#?}", generator.ir);

        return Err("todo".to_string())
    }
}

#[test]
pub fn test_lookbehind(){
    
    match IRRegex::new("(?<=y)x|u", ""){
        Err(e) => println!("{}", e),
        Ok(e) => {}
    };
}