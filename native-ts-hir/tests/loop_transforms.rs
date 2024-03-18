mod common;

#[test]
fn test_for_in_loop() {
    let s = r#"
    for (let i in []){
        i += (99)
    }
    "#;
    common::run_test(s);
}

#[test]
fn test_for_of_loop() {
    let s = r#"
    for (let i of [0, 9, 8]){
        i+=(99);
    }
    "#;
    common::run_test(s);
}

#[test]
fn test_while_loop() {
    let s = "let i = 0;

    while (i < 100){
        i++;
    }";
    common::run_test(s);
}
