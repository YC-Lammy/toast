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

var var2:number
var2=0
var var0:number[]
var var1:number
var0=[0 as number,9 as number,8 as number]
var1=var0.length
for (;;){
    if ((var1)===(var2)){
        break
    }
    var var3:number
    var3=var0[var2++]
    var3+=99
}