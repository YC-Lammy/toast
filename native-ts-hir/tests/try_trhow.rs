mod common;

#[test]
fn test_try_throw_simple() {
    common::run_test(
        "
try{
    throw 9
} catch(e: number){
    e
}
    ",
    );
}

#[test]
fn test_try_throw_function_recurse() {
    common::run_test(
        "
function recursive_throw(count: number){
    if (count == 12){
        throw 48.78 * count;
    } else{
        recursive_throw(count + 1)
    }
}

try{
    recursive_throw(0);
} catch(e: number){
    throw e
} finally{

}",
    );
}
