open System

let countSquares (list: int list) = 
    let squares = list |> List.map(fun x -> x * x ) |> List.distinct

    list |> List.filter (fun x -> List.contains x squares) |> List.length

[<EntryPoint>]
let main argv =
    let myList = [1; 2; 3; 4; 5; 9; 16] 
    let result = countSquares myList
    printfn "Список: %A" myList
    printfn "Количество элементов, являющихся квадратами: %d" result  // Ожидаем 4
    0