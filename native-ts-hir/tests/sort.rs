mod common;

#[test]
fn test_bubble_sort() {
    let s = "function bblSort(arr: number[]): number[] {
 
        for (var i = 0; i < arr.length; i++) {
     
            // Last i elements are already in place  
            for (var j = 0; j < (arr.length - i - 1); j++) {
     
                // Checking if the item at present iteration 
                // is greater than the next iteration
                if (arr[j] > arr[j + 1]) {
     
                    // If the condition is true
                    // then swap them
                    var temp = arr[j]
                    arr[j] = arr[j + 1]
                    arr[j + 1] = temp
                }
            }
        }
     
        return arr
    }
     
    // This is our unsorted array
    var arr = [234, 43, 55, 63, 5, 6, 235, 547];
     
    // Now pass this array to the bblSort() function
    bblSort(arr);";

    common::run_test(s);
}

#[test]
fn test_quick_sort() {
    let s = "function partition(arr: number[], low: number, high: number): number { 
        let pivot = arr[high]; 
        let i = low - 1; 
      
        for (let j = low; j <= high - 1; j++) { 
            // If current element is smaller than the pivot 
            if (arr[j] < pivot) { 
                // Increment index of smaller element 
                i++; 
                // Swap elements 
                [arr[i], arr[j]] = [arr[j], arr[i]];  
            } 
        } 
        // Swap pivot to its correct position 
        [arr[i + 1], arr[high]] = [arr[high], arr[i + 1]];  
        return i + 1; // Return the partition index 
    } 
      
    function quickSort(arr: number[], low: number, high: number) { 
        if (low >= high) return; 
        let pi = partition(arr, low, high); 
      
        quickSort(arr, low, pi - 1); 
        quickSort(arr, pi + 1, high); 
    }
    let arr = [10, 80, 30, 90, 40]; 
    quickSort(arr, 0, arr.length - 1); 
    arr
    ";

    common::run_test(s);
}

#[test]
fn test_insertion_sort() {
    common::run_test(
        "
    // Function to implement insertion sort 
function insertionSort(arr: number[]): number[] { 
  
    // Getting the array length  
    let n = arr.length; 
      
    // To store value temporarily 
    let key: number = 0; 
      
    // For iterations
    let j: number = 0; 
      
    // Iterate array in forward direction 
    for (let i = 0; i < n ; ++i) { 
        key = arr[i]; 
        j = i - 1; 

        // Iterate and swap elements in backward direction 
        // till number is greater then the key 
        for (j; j >= 0 && arr[j]>key; --j){ 
            arr[j+1]=arr[j]; 
        } 
        // Swap the key to right position 
        arr[j+1]=key; 
    }

    return arr
} 
insertionSort([9,6,3,4,3,12,4])
",
    );
}

#[test]
fn test_switch() {
    common::run_test(
        "
    let i = 99;
switch (i){
    case 1:
        'case 1';
        break;
    default:
        'default case';
    case 7:
        'case 7';
        break;
}
    ",
    );
}
