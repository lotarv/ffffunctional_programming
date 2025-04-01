open System

// Функция для вычисления суммы цифр числа (по абсолютной величине)
let sumOfDigits (n: int) =
    let absN = abs n
    let rec sumDigits acc num =
        if num = 0 then acc
        else sumDigits (acc + (num % 10)) (num / 10)
    sumDigits 0 absN

// Функция для подсчёта количества делителей
let countDivisors (n: int) =
    let absN = abs n
    let rec count d limit =
        if d > limit then 0
        else if absN % d = 0 then
            let pair = if d * d = absN then 1 else 2  // Если d² = n, то только 1 делитель
            pair + count (d + 1) (absN / d)
        else
            count (d + 1) limit
    if absN = 0 then 0 else count 1 absN

// Основная функция
let buildTuples (listA: int list) (listB: int list) (listC: int list) =
    // Сортировка списка A по убыванию
    let sortedA = listA |> List.sortByDescending id
    
    // Сортировка списка B по возрастанию суммы цифр, затем по возрастанию абсолютной величины
    let sortedB = listB |> List.sortBy (fun x -> (sumOfDigits x, abs x))
    
    // Сортировка списка C по убыванию количества делителей, затем по убыванию абсолютной величины
    let sortedC = listC |> List.sortByDescending (fun x -> (countDivisors x, abs x))
    
    // Определяем минимальную длину
    let minLength = List.min [List.length sortedA; List.length sortedB; List.length sortedC]
    
    // Берём первые minLength элементов и комбинируем в кортежи
    List.zip3
        (sortedA |> List.take minLength)
        (sortedB |> List.take minLength)
        (sortedC |> List.take minLength)

[<EntryPoint>]
let main argv =
    let listA = [5; 2; 8; 1; 9]
    let listB = [12; 3; 45; 7; -13]
    let listC = [6; 15; 8; 4; 10]
    
    let result = buildTuples listA listB listC
    printfn "Список A: %A" listA
    printfn "Список B: %A" listB
    printfn "Список C: %A" listC
    printfn "Результат: %A" result
    0