use native_ts_hir::ast::format::Formatter;
use native_ts_hir::interpreter::Interpreter;
use native_ts_hir::transform::Transformer;

pub fn run_test(script: &str) {
    let mut map = native_ts_parser::SourceMap::default();

    let parser = native_ts_parser::Parser::new(&mut map);
    let m = parser
        .parse_str(
            "test".to_string(),
            include_str!("./assert.ts").to_string() + script,
        )
        .expect("parse failed");

    let translator = Transformer::new();

    let p = match translator.transform_program(&m) {
        Ok(v) => v,
        Err(e) => {
            panic!("{}", e.format_error(&map));
        }
    };

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
