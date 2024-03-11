mod common;

#[test]
fn binary_search() {
    let s = "binarySearch([0, 8, 9, 10, 11, 12, 13, 14], 12);
    function round(i: number): number{
        let rem = i % 1;
        let n = i - rem;

        if (rem >= 0.5){
            return n + 1
        }
        return n
    }
    function binarySearch(arr: number[], x: number): number
    {
        let l = 0;
        let r = arr.length - 1;
        let mid: number;
        while (r >= l) {
             mid = l + round((r - l) / 2);
      
            // If the element is present at the middle
            // itself
            if (arr[mid] == x)
                return mid;
      
            // If element is smaller than mid, then
            // it can only be present in left subarray
            if (arr[mid] > x)
                r = mid - 1;

            // Else the element can only be present
            // in right subarray
            else
                l = mid + 1;
        }
      
        // We reach here when element is not
        // present in array
        return -1;
    }";

    common::run_test(s);
}
