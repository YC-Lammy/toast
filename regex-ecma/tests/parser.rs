use regex_ecma::pest_parser::parse;

#[test]
pub fn test_char_range() {
    let c = "&[z-a]$";

    let e = parse(c, false);
    println!("{:#?}", e);
    assert!(e.is_err());
}

#[test]
pub fn test_quantifier_range(){
    let c = "0{2,1}";

    assert!(parse(&c, false).is_err());
}

#[test]
pub fn test_quantifier_errors(){

    const TESTS:&[&str] = &[
        "a**",
        "++a",
        "?a",
        "??a",
        "x{1}{1,}",
        "x{1,2}{1}",
        "x{1,}{1}",
        "x{0,1}{1,}",
        "a***",
        "a++",
        "a+++",
        "a???",
        "a????",
        "*a",
        "**a",
        "+a",
    ];

    for s in TESTS{
        let re = parse(s, false);

        assert!(re.is_err());
    }
    
}

#[test]
fn test_ok(){
    
}