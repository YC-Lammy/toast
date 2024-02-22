use native_ts_hir::ast::format::Formatter;
use native_ts_hir::transform::Transformer;

#[test]
fn test_for_in_loop() {
    let s = r#"
    for (let i in []){
        i += (99)
    }
    "#;
    let parser = native_ts_parser::Parser::new();
    let m = parser
        .parse_str("test".to_string(), s.to_string())
        .expect("parse failed");

    for (_id, module) in m.modules {
        let mut t = Transformer::new();

        let re = t.transform_module(&module.module).expect("parse error");
        let mut formatter = Formatter::new(&re.table);
        formatter.format_module(&re);

        let formated = formatter.emit_string();
        println!("{}", formated);
    }
}

#[test]
fn test_for_of_loop() {
    let s = r#"
    for (let i of [0, 9, 8]){
        i+=(99);
    }
    "#;
    let parser = native_ts_parser::Parser::new();
    let m = parser
        .parse_str("test".to_string(), s.to_string())
        .expect("parse failed");

    for (_id, module) in m.modules {
        let mut t = Transformer::new();

        let re = t.transform_module(&module.module).expect("parse error");
        let mut formatter = Formatter::new(&re.table);
        formatter.format_module(&re);

        let formated = formatter.emit_string();
        println!("{}", formated);
    }
}
