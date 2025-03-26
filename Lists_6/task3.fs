open System

let rec foldWithPredicate(list: int list) (f: int -> int -> int) (p: int -> bool) (acc: int) =
    match list with
    | [] -> acc
    | head :: tail -> 
        let newAcc = if p head then f acc head else acc
        foldWithPredicate tail f p newAcc

[<EntryPoint>]
let main argv = 
    let myList = [1;2;3;4;5;6]
    let add = fun x y -> x + y
    let isEven = fun x -> x % 2 = 0
    let initialAcc = 0
    let result = foldWithPredicate myList add isEven initialAcc
    printfn "Result: %d" result
    0