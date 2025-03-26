open System

let rec churcList n acc = 
    match n with
    | 0 -> acc
    | _ ->
        printfn "Enter element: "
        let x = Console.ReadLine() |> int
        churcList (n-1) (x :: acc)

let readList n = churcList n []

[<EntryPoint>]
let main (argv: string[]) =
    printfn "Enter the amount of elements (n): "
    let n = Console.ReadLine() |> int
    let result = readList n
    printfn "Your list: %A" result
    0