mod common;

#[test]
fn test_simple_hash() {
    let s = "
    function simple_hash(arr: number[]): number{
        var hash = 0;
        for (var i = 0; i < arr.length; i++) {
            var char:number = arr[i];
            hash = ((hash<<5)-hash)+char;
            hash = hash & hash; // Convert to 32bit integer
        }
        return hash;
    }
    
    simple_hash([0,9,6,1,5,9,8,4]);";
    common::run_test(s)
}
