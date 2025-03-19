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
    0
