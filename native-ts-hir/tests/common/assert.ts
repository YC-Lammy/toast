


function assert(b: boolean){
    if (!b){
        throw "TestError"
    }
}

function expect_throw(fun: ()=> undefined){
    let thrown = false;
    try{
        fun();
    } catch(e: any){
        thrown = true;
        e
    }

    if (!thrown){
        throw "TestError"
    }
}
