open System

// Факториал для Пуассона
let factorial (n: int) : int64 =
    let rec factTail n acc = 
        match n with
        | 0L -> acc
        | 1L -> acc
        | _ -> factTail (n-1L) acc * n
    factTail (int64 n) 1L

// Функция для вычисления степени простого p в n!
let primeExponentInFactorial (n: int) (p: int) =
    let rec sumDivisions n acc =
        if n = 0 then acc
        else sumDivisions (n / p) (acc + n / p)
    sumDivisions n 0

// Приближение числа переносов через Пуассона (с mutable)
// let poissonProbGreaterEqual (lambda: float) (k: int) : float =
//     let mutable prob = 0.0
//     for i in 0 .. k - 1 do
//         let term = exp(-lambda) * (pown lambda i) / float (factorial i)
//         prob <- prob + term
//     1.0 - prob

// Приближение числа переносов через Пуассона (без mutable)
let poissonProbGreaterEqual (lambda: float) (k: int) : float =
    let probLessThanK = 
        seq { 0 .. k - 1 }
        |> Seq.sumBy (fun i -> exp(-lambda) * (pown lambda i) / float (factorial i))
    1.0 - probLessThanK

// Основная функция
[<EntryPoint>]
let main argv =
    let layer = 200000u
    let prime1 = 2u
    let exponent1 = 12u
    let prime2 = 5u
    let exponent2 = 12u

    // Общее число слагаемых
    let totalTerms = (uint64 (layer + 1u)) * (uint64 (layer + 2u)) / 2UL

    // Оценка числа разрядов и среднего числа переносов
    let digits1 = log (float layer) / log (float prime1) // log_2(200000)
    let digits2 = log (float layer) / log (float prime2) // log_5(200000)

    // Среднее число переносов (приближение)
    let lambda1 = digits1 * 0.5 // Для p = 2
    let lambda2 = digits2 * 0.25 // Для p = 5 (примерная вероятность переноса)

    // Вероятности v_p >= exp
    let prob1 = poissonProbGreaterEqual lambda1 (int exponent1)
    let prob2 = poissonProbGreaterEqual lambda2 (int exponent2)

    // Число кратных
    let count1 = uint64 (prob1 * float totalTerms)
    let count2 = uint64 (prob2 * float totalTerms)

    // Итоговый результат
    let result = uint64 (prob1 * prob2 * float totalTerms)

    printfn "Число коэффициентов, кратных 10^12: %d" result
    printfn "Общее число слагаемых: %d" totalTerms
    printfn "Кратных 2^%d: %d (prob = %f)" exponent1 count1 prob1
    printfn "Кратных 5^%d: %d (prob = %f)" exponent2 count2 prob2
    0