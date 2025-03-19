open System

type SolveResult =
    None | Linear of float | Quadratic of float*float

let solve a b c =
    let D = b*b - 4.0*a*c
    if a=0.0 then
        if b=0. then None
        else Linear(-c/b)
    else
        if D<0. then None
        else Quadratic(((-b+sqrt(D))/(2.0*a), (-b-sqrt(D))/(2.0*a)))

let circleSurface r = Math.PI * r * r
let multiplySurfaceH s h = s * h
let cylinderVolumeSuperPosition = circleSurface >> multiplySurfaceH
let cylinderVolumeCurry r h = (circleSurface r) * h


let rec digitSumUp num = 
    if num = 0 then 0
    else (num % 10) + (digitSumUp(num / 10))

[<EntryPoint>]
let main (argv :string[]) = 
    //task 1
    Console.WriteLine("task1: Hello world")
    Console.WriteLine("task2: Solving quadratic quation")
    let res = solve 1.0 2.0 -3.0
    match res with
     | None -> Console.WriteLine("No solution")
     | Linear(x) -> printfn "Linear quation x = %f" x
     | Quadratic(x1,x2) -> printfn "Quadratic equation: x1=%f, x=%f" x1 x2

    Console.WriteLine("task3: Cylinder Volume via SuperPosition")

    Console.WriteLine("Enter radius:")
    let r = Console.ReadLine() |> float
    Console.WriteLine("Enter height:")
    let h = Console.ReadLine() |> float
    let volume = cylinderVolumeSuperPosition r h
    printfn "Cylinder Volume via SuperPosition: %f" volume

    Console.WriteLine("task3: Cylinder Volume via Curry")

    Console.WriteLine("Enter radius:")
    let r = Console.ReadLine() |> float
    Console.WriteLine("Enter height:")
    let h = Console.ReadLine() |> float
    let volume = cylinderVolumeCurry r h
    printfn "Cylinder Volume via Curry: %f" volume

    Console.WriteLine("task4: Digit Sum (Recursion UP)")
    Console.WriteLine("Enter number:")
    let num = Console.ReadLine() |> int
    let sum = digitSumUp num
    printfn "Digit sum of %d is: %d" num sum


    0
