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

#[test]
fn test_intersect() {
    common::run_test(
        "
    interface A{
        a: number;
    }
    
    interface B{
        a: string
    }
    
    type U = A & B;",
    )
}
