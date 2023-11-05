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

type I = RegExp;

let human = new Human;
var car = new Car;

for (var t of [0, 9]){
    let is_dead = car.crash(39, human);
}