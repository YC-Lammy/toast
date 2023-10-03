use regex_ecma::parser::parse;

#[test]
pub fn test_char_range() {
    let c = "&[z-a]$".chars().collect::<Vec<char>>();

    assert!(parse(&c, false).is_err());
}

#[test]
pub fn test_quantifier_range(){
    let c = "0{2,1}".chars().collect::<Vec<char>>();

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
        let c = s.chars().collect::<Vec<char>>();
        let re = parse(&c, false);

        assert!(re.is_err());
    }
    
}

#[test]
fn test_ok(){
    
}