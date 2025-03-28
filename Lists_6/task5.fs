open System

let rec updateCount (elem: int) (counts: (int * int) list) = 
    match counts with
    | [] -> [(elem, 1)]
    | (x, count) :: tail -> 
        if x = elem then (x, count + 1) :: tail
        else (x, count) :: updateCount elem tail

let rec countElements (list: int list) (acc: (int * int) list) = 
    match list with
    | [] -> acc
    | head :: tail -> 
        let newAcc = updateCount head acc
        countElements tail newAcc

let rec findMostFrequent (counts: (int * int) list) (maxElem: int) (maxCount: int) =
    match counts with
    | [] -> maxElem
    | (elem, count) :: tail ->
        if count > maxCount then findMostFrequent tail elem count
        else findMostFrequent tail maxElem maxCount

let mostFrequent (list: int list) = 
    match list with
    | [] -> failwith "Empty list"
    | head :: _ -> 
        let counts = countElements list []
        findMostFrequent counts head 1
    
[<EntryPoint>]
let main argv = 
    let myList = [1;2;3;2;4;2;5;3;2]
    let mostFrequentNumber = mostFrequent myList
    printfn "List: %A" myList
    printfn "The most frequent element: %d" mostFrequentNumber
    0