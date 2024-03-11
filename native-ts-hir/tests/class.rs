mod common;

#[test]
fn test_class() {
    common::run_test(
        "
    class Human{
        age: number;
    
        static log(msg: string){
    
        }
    }
    
    class Vechical{
        weight?: number;
    }",
    )
}
