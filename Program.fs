// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
printfn "Welcome to my Ford Fanatics App for Simple Classes and Enumerators"
printfn "==================================================================="
type CarType =
    | Tricar = 0
    | FourWheeler = 1
    | Doullies = 2
    | WeirdContraption = 3
    | CrazyMonster = 4


[<AbstractClass>]
type Car(color: string, wheelcount: int) =
    do
        if wheelcount < 3 then
            failwith "We assume cars must have at least 3 wheels"
        if wheelcount > 99 then
            failwith "That's ridiculous"

    let carType = 
        match wheelcount with
        | 3 -> CarType.Tricar
        | 4 -> CarType.FourWheeler
        | 6 -> CarType.Doullies
        | x when x % 2 = 1 -> CarType.WeirdContraption
        | _ -> CarType.CrazyMonster

    let mutable passengerCount = 0

    new() = Car("red", 4)

    member x.Move() = printfn " The %s car (%A) is moving" color carType
    member x.CarType = carType

    abstract PassengerCount: int with get, set

     
type Truck(year: int, color: string, model: string, wheelcount: int) =
    do 
        if year < 1900 then 
            failwith "We assume no cars/trucks existed prior to 1900"
        if year > 2999 then
            failwith "We assume that Earth is gone by 2999 due to climate change"
        if wheelcount < 3 then
            failwith "No car/truck has less than 3 wheels"
        if wheelcount > 10 then
            failwith "Consult Caterpillar for haulers and Mack for semi's"

 // Using pattern-matching on an Enum to deconstruct and also to analyze data     
    let carType =
        match wheelcount with
        | 3 -> CarType.Tricar
        | 4 -> CarType.FourWheeler
        | 6 -> CarType.Doullies 
        | x when x % 2 = 1 -> CarType.WeirdContraption 
        | _ -> CarType.CrazyMonster // Ford F-650, Cat hauler, Mack semi

    let mutable passengerCount = 0

    //We use abstract for overriding and changing an attribute or property 
    //of the base class for the inherited class:
    abstract PassengerCount: int with get, set


    new() = Truck(1995, "Blue", "Ford F-150", 4) // A secondary constructor
    member x.Move() = printfn "The %d %s %s was sold" year color model 
    // The "x" is a self reference available in any class member, like "this" in C# or "self" in Ruby
    member x.CarType = carType

    //We change member to default after using abstract
    default x.PassengerCount with get() = passengerCount and set v = passengerCount <- v

//Here I derive a new class from my existing Car class 
//to demonstrate class inheritance:
type greenDumpTruck() =
    inherit Truck(2000, "Green", "Ford F-750", 2)

    //We use the override keyword for the implementation of
    //our PassengerCount property:
    override x.PassengerCount
        with set v =
            if v > 2 then failwith "Only 2 passengers fit"
                else base.PassengerCount <- v


let truck = Truck()//Instantiation of an instance of the class Car
truck.Move()
truck.PassengerCount <- 2
truck.PassengerCount <- 8


let redTruck = Truck(1999, "Red", "Ford F-650 Superduty", 6)
redTruck.Move()

printfn "The red truck has type %A" redTruck.CarType
printfn "Truck has %d passenger capacity" truck.PassengerCount

truck.PassengerCount <- 6

printfn "Truck has %d passenger capacity" truck.PassengerCount

//Converting types and casting them into other types
//The truck type is being upcast to an 'obj' type
// obj is higher up in the hierarchy of classes than 'truck'
let truckObject = truck :> obj
let truckObjectBack2truck = truck :?> Truck





// Using pattern-mmatching with guard conditions tested if the
// pattern itself succeeds.
//Example: computing the sign of a number:
let sign x =
    match x with
    | _ when x < 0 -> -1
    | _ when x > 0 -> 1
    |_ -> 0
  

    

[<EntryPoint>]
let main argv =
    printfn "****************************" 
    printfn "F is for F# and Ford!"
    0 // return an integer exit code





