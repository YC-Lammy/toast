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