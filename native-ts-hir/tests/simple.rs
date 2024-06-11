mod common;

#[test]
fn test() {
    common::run_test(
        "
    type I = {a?():number};
    let a: I = {};
",
    );
}
