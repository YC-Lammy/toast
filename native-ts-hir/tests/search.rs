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

#[test]
fn test_ternery_search() {
    common::run_test(
        "
    function round(i: number): number{
        let rem = i % 1;
        let n = i - rem;

        if (rem >= 0.5){
            return n + 1
        }
        return n
    }
    function ternarySearch(l: number, r: number, key: number, ar: number[]): number {
        if (r >= l) {
    
            // Find the mid1 and mid2
            let mid1 = l + round((r - l) / 3);
            let mid2 = r - round((r - l) / 3);
    
            // Check if key is present at any mid
            if (ar[mid1] == key) {
                return mid1;
            }
            if (ar[mid2] == key) {
                return mid2;
            }
    
            // Since key is not present at mid,
            // check in which region it is present
            // then repeat the Search operation
            // in that region
    
            if (key < ar[mid1]) {
    
                // The key lies in between l and mid1
                return ternarySearch(l, mid1 - 1, key, ar);
            }
            else if (key > ar[mid2]) {
    
                // The key lies in between mid2 and r
                return ternarySearch(mid2 + 1, r, key, ar);
            }
            else {
    
                // The key lies in between mid1 and mid2
                return ternarySearch(mid1 + 1, mid2 - 1, key, ar);
            }
        }
    
        // Key not found
        return -1;
    }
    let arr = [2, 3, 6, 9, 10, 11, 13, 17, 23];
let search_item = 13;
ternarySearch(0, arr.length - 1, search_item, arr)
    ",
    );
}

#[test]
fn test_fib_test() {
    common::run_test(
        "
    function min(a: number, b: number): number{
        if (a > b){
            return b
        }
        return a
    }
    function fibMonaccianSearch(arr: number[], n: number, x: number) :number
    { 
        /* Initialize fibonacci numbers */
        let fibMMm2 = 0; // (m-2)'th Fibonacci No. 
        let fibMMm1 = 1; // (m-1)'th Fibonacci No. 
        let fibM = fibMMm2 + fibMMm1; // m'th Fibonacci 
       
        /* fibM is going to store the smallest Fibonacci 
        Number greater than or equal to n */
        while (fibM < n) 
        { 
            fibMMm2 = fibMMm1; 
            fibMMm1 = fibM; 
            fibM = fibMMm2 + fibMMm1; 
        } 
       
        // Marks the eliminated range from front 
        let offset = -1; 
       
        /* while there are elements to be inspected. Note that 
        we compare arr[fibMm2] with x. When fibM becomes 1, 
        fibMm2 becomes 0 */
        
        while (fibM > 1) 
        { 
            // Check if fibMm2 is a valid location 
            let i = min(offset + fibMMm2, n-1); 
       
            /* If x is greater than the value at index fibMm2, 
            cut the subarray array from offset to i */
            if (arr[i] < x) 
            { 
                fibM = fibMMm1; 
                fibMMm1 = fibMMm2; 
                fibMMm2 = fibM - fibMMm1; 
                offset = i; 
            } 
       
            /* If x is less than the value at index fibMm2, 
            cut the subarray after i+1 */
            else if (arr[i] > x) 
            { 
                fibM = fibMMm2; 
                fibMMm1 = fibMMm1 - fibMMm2; 
                fibMMm2 = fibM - fibMMm1; 
            } 
       
            /* element found. return index */
            else return i; 
        } 
       
        /* comparing the last element with x */
        if(fibMMm1 && arr[n-1] == x){ 
          return n-1 
        } 
       
        /*element not found. return -1 */
        return -1; 
    } 
    let arr = [10, 22, 35, 40, 45, 50, 80, 82,85, 90, 100,235]; 
    let n = arr.length; 
    let x = 235;
    fibMonaccianSearch(arr, n, x)",
    );
}
