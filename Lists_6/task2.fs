open System

let rec printList list =
    match list with
    | [] -> Console.WriteLine("\n")
    | head :: tail -> 
        Console.WriteLine(head.ToString())
        printList(tail)

[<EntryPoint>]
let main argv = 
    Console.WriteLine("List: \n")
    let myList = [1;2;3;4;5]
    printList myList
    0