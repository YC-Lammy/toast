use native_ts_hir::ast::format::Formatter;
use native_ts_hir::interpreter::Interpreter;
use native_ts_hir::transform::Transformer;

pub fn run_test(script: &str) {
    let parser = native_ts_parser::Parser::new();
    let m = parser
        .parse_str("test".to_string(), script.to_string())
        .expect("parse failed");

    let translator = Transformer::new();

    let p = translator.transform_program(&m).expect("transform error");

    for (_id, module) in &p.modules {
        let mut formatter = Formatter::new(&p.table);
        formatter.format_module(&module);

        let formated = formatter.emit_string();
        println!("{}", formated);
    }

    let intpr = Interpreter::new();

    let re = intpr.run(&p);

    println!("{:?}", re);
}
