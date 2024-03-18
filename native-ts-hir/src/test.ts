class Human{
    age: number;

    static log(msg: string){

    }
}

class Vechical{
    weight?: number;
}

class Car extends Vechical implements Thing{
    constructor(a:boolean){
        if (a){
            super()
        }
    }
}

class Plane<T extends Thing>{

}

interface Thing{
    
}

interface A{
    a: number;
}

interface B{
    a: string
}

type U = A & B;

let human = new Human;
var car = new Car(false);

let i = 0;
i = i + 1;

let r:[number, string, bigint] = [0, "", 330n];

for (var e of r){

}

for (let i in []){
    i += (99)
}

for (let i of [0, 9, 8]){
    i+=(99);
}

let my_array = [0, 9];

fun1([],0)
function fun1(this:any, var0:number[], var1:number):number{
    var var2:number
    var2=0 as number
    var var3:number
    var3=(var0.length)-(1) as number
    var var4:number
    for (;;){
        if ((var3)>=(var2)){
            break
        }
        var4=(var2)+(((var3)-(var2))/(2 as number))
        if ((var0[var4])==(var1)){
            return var4
        }
        if ((var0[var4])>(var1)){
            var3=(var4)-(1 as number)
        }
        else {
            var2=(var4)+(1 as number)
        }
    }    return -(1)
}

let t = {i:0, u: "i"};

binarySearch([0, 8, 9, 10, 11, 12, 13, 14], 12);
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
}

// Bubble sort Implementation using Javascript
 
// Creating the bblSort function
function bblSort(arr: number[]) {
 
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
 
    // Print the sorted array
    console.log(arr);
}
 
// This is our unsorted array
var arr = [234, 43, 55, 63, 5, 6, 235, 547];
 
// Now pass this array to the bblSort() function
bblSort(arr);

class K{
    a: number
}

interface A{
    a: number;
}

interface B{
    a: string
}

type I = A & B;

function simple_hash(arr: number[]){
    var hash = 0;
    for (var i = 0; i < arr.length; i++) {
        var char = arr[i];
        hash = ((hash<<5)-hash)+char;
        hash = hash & hash; // Convert to 32bit integer
    }
    return hash;
}

simple_hash([0,9,6,1,5,9,8,4]);

type u = keyof object;
var a;
interface A{}
interface A{}

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
fibMonaccianSearch(arr, n, x)

let a = 0;
for (let i = 0; i < 100;i++){
    for (let j=0; j < 100;j++){
        a++;
    }
}

let y = [0, ""];
let [a, b]:[number, string] = [0, ""];

function recurring(){
    for (let i=0;i< 100;i++){
        recurring()
    }
}

do{

} while(true);

type a = [number, string, undefined, object, bigint[], {}]