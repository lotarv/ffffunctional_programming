open System

// Функция для вычисления степени простого p в n!
let primeExponentInFactorial (n: int) (p: int) =
    let rec sumDivisions n acc =
        if n = 0 then acc
        else sumDivisions (n / p) (acc + n / p)
    sumDivisions n 0

// Функция для вычисления степени p в триномиальном коэффициенте
let primeExponentInTrinomial (n: int) (i: int) (j: int) (k: int) (p: int) =
    let nFact = primeExponentInFactorial n p
    let iFact = primeExponentInFactorial i p
    let jFact = primeExponentInFactorial j p
    let kFact = primeExponentInFactorial k p
    nFact - iFact - jFact - kFact

// Подсчёт числа триномиальных коэффициентов, кратных 10^target
let countMultiples (n: int) (exp2: int) (exp5: int) =
    let mutable count = 0L
    for i = 0 to n do
        for j = 0 to n - i do
            let k = n - i - j
            // Вычисляем v_2 и v_5 для текущей тройки (i, j, k)
            let v2 = primeExponentInTrinomial n i j k 2
            let v5 = primeExponentInTrinomial n i j k 5
            // Проверяем, кратно ли 10^target, то есть v_2 >= exp2 и v_5 >= exp5
            if v2 >= exp2 && v5 >= exp5 then
                count <- count + 1L
    count

[<EntryPoint>]
let main argv =
    let n = 10
    let target = 2 // 10^target = 10^2
    let exp2 = 2 // Степень 2 в 10^2
    let exp5 = 2 // Степень 5 в 10^2

    // Общее число слагаемых
    let totalTerms = (int64 (n + 1)) * (int64 (n + 2)) / 2L

    // Считаем, сколько коэффициентов кратны 10^target
    let result = countMultiples n exp2 exp5

    printfn "Число коэффициентов, кратных 10^%d: %d" target result
    printfn "Общее число слагаемых: %d" totalTerms
    0