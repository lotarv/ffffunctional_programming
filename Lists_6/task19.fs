open System

let isPalindrome (s: string) = 
    let cleaned = s.ToLower() |> String.filter (fun c -> c <> ' ')
    cleaned = (cleaned |> Seq.rev |> String.Concat)

[<EntryPoint>]
let main argv = 
    printfn "Введите строку: "
    let input_string = Console.ReadLine()
    let result = isPalindrome input_string
    let result_message = if result 
                         then sprintf "Строка '%s' является палиндромом." input_string 
                         else sprintf "Строка '%s' НЕ является палиндромом." input_string
    printfn "%s" result_message
    0