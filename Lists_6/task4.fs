open System


let rec foldWithPredicate (list: int list) (f: int -> int -> int) (p: int -> bool) acc = 
    match list with
    | [] -> acc
    | head :: tail -> 
        let newAcc = if p head then f acc head else acc
        foldWithPredicate tail f p newAcc

let min_in_list list = 
    match list with
    | [] -> failwith "List is empty"
    | head :: _ -> 
        let minFunc = fun acc x -> if acc < x then acc else x
        let alwaysTrue = fun _ -> true
        foldWithPredicate list minFunc alwaysTrue head

let sum_of_even list = 
    match list with
    | [] -> failwith "List is empty"
    | list -> 
        let isEven = fun x -> if x % 2 = 0 then true else false
        let sum = fun x y -> x + y
        foldWithPredicate list sum isEven 0

let amount_of_odd list = 
    match list with
    | [] -> 0
    | list -> 
        let isOdd = fun x -> if x % 2 <> 0 then true else false
        let count = fun x _ -> x + 1
        foldWithPredicate list count isOdd 0
[<EntryPoint>]
let main argv =
    let myList = [16;65;2;4;6;7;1;-5;14;3;81]
    let min = min_in_list myList
    let sumOfEven = sum_of_even myList
    let amountOfOdd = amount_of_odd myList
    printfn "List: (16,65,2,4,6,7,1,-5,14,3,81)"
    printfn "Minimal element of the list: %d" min
    printfn "Sum of even numbers: %d" sumOfEven
    printfn "Amount of odd numbers: %d" amountOfOdd
    0
