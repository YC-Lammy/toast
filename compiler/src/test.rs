use super::*;

#[test]
pub fn test() {
    let config = Configuration::default();

    let main_file = "tests/index.ts";

    let re = parse_and_type_check(config.clone(), main_file);

    let (package, srcmap) = match re {
        Ok(m) => m,
        Err(e) => {
            print_error(e);
            panic!()
        }
    };

    let ir_builder = ir_builder::IRBuilder::new(config.clone());
    let re = ir_builder.build(package);

    let mut package = match re {
        Ok(m) => m,
        Err(e) => {
            println!("{}", e.display(&srcmap));
            panic!()
        }
    };

    ir_optimiser::optimise_package(&mut package);

    println!("{:#?}", package);

    compiler::llvm::compile(config, package);
}
