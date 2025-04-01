open System

// Функция для определения, является ли символ гласной
let isVowel (c: char) =
    let vowels = ['a'; 'e'; 'i'; 'o'; 'u']
    List.contains (Char.ToLower c) vowels


let vowelConsonantDifference (s: string) =
    let chars = s.ToLower().ToCharArray()
    let rec countPairs (arr: char[]) vc cv i =
        if i >= arr.Length - 1 then (vc, cv)
        else
            let isCurrVowel = isVowel arr.[i]
            let isNextVowel = isVowel arr.[i + 1]
            match isCurrVowel, isNextVowel with
            | true, false -> countPairs arr (vc + 1) cv (i + 1)  // Гласная-согласная
            | false, true -> countPairs arr vc (cv + 1) (i + 1)  // Согласная-гласная
            | _ -> countPairs arr vc cv (i + 1)                   
    let vc, cv = countPairs chars 0 0 0
    vc - cv

// Функция для чтения списка строк
let readStringList () =
    printfn "Введите количество строк:"
    let n = try Console.ReadLine() |> int with _ -> 0
    if n <= 0 then
        printfn "Ошибка: введите положительное число. Возвращается пустой список."
        []
    else
        [for _ in 1..n do
            printfn "Введите строку:"
            yield Console.ReadLine()]

// Основная функция сортировки
let sortByVowelConsonantDiff (strings: string list) =
    strings |> List.sortBy vowelConsonantDifference

[<EntryPoint>]
let main argv =
    let strings = readStringList ()
    let sorted = sortByVowelConsonantDiff strings
    printfn "Отсортированные строки по разнице 'гласная-согласная' минус 'согласная-гласная':"
    sorted |> List.iter (printfn "%s")
    0