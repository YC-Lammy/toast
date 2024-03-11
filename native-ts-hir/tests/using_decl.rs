mod common;

#[test]
fn test_using_decl() {
    common::run_test(
        "
    class Disposable{
        [Symbol.dispose](){
            39
        }
    }
    function my_function(){
        using myobject = new Disposable;
    }
    ",
    )
}
