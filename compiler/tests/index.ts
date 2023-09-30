
import * as myimport from "./import";

var myvar = "2333";

async function test_async(){
    await 0;
}

function log(value){

}

{
    let t = /xois/p;
}

var array = [0, 9, 7];

for (let r of array){
    log(r);
}

for (let i in array){
    log(i);
}

let i = 88;
do{
    i -= 1;
}while (i);

;

if (true){
    log(null);
}

label: for (;;){
    break label
}

try{
    throw 0.0
} catch(e){
    log(e)
}

switch (undefined){
    case undefined:{

    }
    default:{
        log("rjeifie")
    }
}