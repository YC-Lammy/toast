binarySearch([], 0)

function binarySearch(arr: number[], x: number): number
{    
    let l = 0;
    let r = arr.length - 1;
    let mid: number;
    while (r >= l) {
         mid = l + (r - l) / 2;
  
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

fun1([],0)
function fun1(this:any, var7:number[], var8:number):number{
    var var4:number
    var4=0 as number
    var var5:number
    var5=(var2.length)-(1) as number
    var var6:number
    for (;;){
        if ((var5)>=(var4)){
            break
        }
        var6=(var4)+(((var5)-(var4))/(2 as number))
        if ((var2[var6])==(var3)){
            return var6
        }
        if ((var2[var6])>(var3)){
            var5=(var6)-(1 as number)
        }
        else {
            var4=(var6)+(1 as number)
        }
    }    return -(1)
    var var9:number
    var9=0 as number
    var var10:number
    var10=(var7.length)-(1) as number
    var var11:number
    for (;;){
        if ((var10)>=(var9)){
            break
        }
        var11=(var9)+(((var10)-(var9))/(2 as number))
        if ((var7[var11])==(var8)){
            return var11
        }
        if ((var7[var11])>(var8)){
            var10=(var11)-(1 as number)
        }
        else {
            var9=(var11)+(1 as number)
        }
    }    return -(1)
}