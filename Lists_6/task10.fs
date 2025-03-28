open System

let readStringList n =
    let rec read n acc =
        match n with
        | 0 -> List.rev acc  
        | _ ->
            printfn "Введите строку:"
            let str = Console.ReadLine()
            read (n - 1) (str :: acc)
    read n []


let sortByLength () =
    printfn "Введите количество строк:"
    let n = Console.ReadLine() |> int
    let strings = readStringList n
    let sorted = strings |> List.sortBy String.length
    sorted

[<EntryPoint>]
let main argv =
    let sortedList = sortByLength ()
    printfn "Отсортированный список по длине: %A" sortedList
    0