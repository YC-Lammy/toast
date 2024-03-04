class Human{
    age: number;

    static log(msg: string){

    }
}

class Vechical{
    weight?: number;
}

class Car extends Vechical implements Thing{
    constructor(){
        super()
    }
    crash(speed:number, human: Human): boolean{
        if (speed > 60){
            return true
        }
        if (human.age > 70){
            return true
        }
        return false
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
var car = new Car;

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