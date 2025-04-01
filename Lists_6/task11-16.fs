open System

// 1.14 Church
let rec countInRangeChurch (list: int list) (a: int) (b: int) (acc: int) =
    match list with
    | [] -> acc
    | head :: tail ->
        let newAcc = if head >= a && head <= b then acc + 1 else acc
        countInRangeChurch tail a b newAcc

// 1.14 List
let countInRangeList (list: int list) (a: int) (b: int) =
    list |> List.filter (fun x -> x >= a && x <= b) |> List.length

// 1.24 Church
let rec twoMaxChurch (list: int list) (max1: int) (max2: int) =
    match list with
    | [] -> (max1, max2)
    | head :: tail ->
        if head > max1 then twoMaxChurch tail head max1
        else if head > max2 then twoMaxChurch tail max1 head
        else twoMaxChurch tail max1 max2

let findTwoMaxChurch (list: int list) =
    match list with
    | [] -> failwith "Список пуст"
    | [x] -> (x, x)
    | x :: y :: tail -> twoMaxChurch tail (max x y) (min x y)

// 1.24 List
let findTwoMaxList (list: int list) =
    match list with
    | [] -> failwith "Список пуст"
    | _ ->
        let sorted = list |> List.sortDescending
        (sorted.[0], sorted.[1])

// 1.34 Church
let rec elementsInRangeChurch (list: int list) (a: int) (b: int) (acc: int list) =
    match list with
    | [] -> List.rev acc
    | head :: tail ->
        let newAcc = if head >= a && head <= b then head :: acc else acc
        elementsInRangeChurch tail a b newAcc

// 1.34 List
let elementsInRangeList (list: int list) (a: int) (b: int) =
    list |> List.filter (fun x -> x >= a && x <= b)

// 1.44 Church
let rec isAlternatingChurch (list: float list) (prevIsInt: bool option) (acc: bool) =
    match list with
    | [] -> acc
    | head :: tail ->
        let isInt = head = floor head
        match prevIsInt with
        | None -> isAlternatingChurch tail (Some isInt) true
        | Some prev ->
            let newAcc = acc && (isInt <> prev)
            isAlternatingChurch tail (Some isInt) newAcc

let checkAlternatingChurch (list: float list) =
    isAlternatingChurch list None true

// 1.44 List
let checkAlternatingList (list: float list) =
    match list with
    | [] | [_] -> true
    | _ ->
        list
        |> List.map (fun x -> x = floor x)
        |> List.pairwise
        |> List.forall (fun (a, b) -> a <> b)

// 1.54 Church
let rec buildCountChurch (list: int list) (acc: (int * int) list) =
    match list with
    | [] -> acc
    | head :: tail ->
        let newAcc =
            match List.tryFind (fun (x, _) -> x = head) acc with
            | Some (x, count) -> (x, count + 1) :: List.filter (fun (y, _) -> y <> x) acc
            | None -> (head, 1) :: acc
        buildCountChurch tail newAcc

let moreThanThreeChurch (list: int list) =
    let counts = buildCountChurch list []
    counts |> List.filter (fun (_, count) -> count > 3) |> List.map fst

// 1.54 List
let moreThanThreeList (list: int list) =
    list
    |> List.groupBy id
    |> List.filter (fun (_, group) -> List.length group > 3)
    |> List.map fst

[<EntryPoint>]
let main argv =
    let list1 = [1; 3; 5; 7; 2; 4; 6; 8]
    let list2 = [1; 2; 2; 3; 3; 3; 4; 4; 4; 4]
    let listFloat = [1.0; 2.5; 3.0; 4.5; 5.0]

    // 1.14
    let rangeCountChurch = countInRangeChurch list1 3 6 0
    let rangeCountList = countInRangeList list1 3 6
    printfn "1.14. Элементов в диапазоне 3..6:"
    printfn "  Church: %d" rangeCountChurch  // Ожидаем 4 (3, 5, 4, 6)
    printfn "  List: %d" rangeCountList      // Ожидаем 4

    // 1.24
    let twoMaxC = findTwoMaxChurch list1
    let twoMaxL = findTwoMaxList list1
    printfn "1.24. Два максимальных элемента:"
    printfn "  Church: %A" twoMaxC  // Ожидаем (8, 7)
    printfn "  List: %A" twoMaxL    // Ожидаем (8, 7)

    // 1.34
    let rangeElementsChurch = elementsInRangeChurch list1 3 6 []
    let rangeElementsList = elementsInRangeList list1 3 6
    printfn "1.34. Элементы в диапазоне 3..6:"
    printfn "  Church: %A" rangeElementsChurch  // Ожидаем [3; 5; 4; 6]
    printfn "  List: %A" rangeElementsList      // Ожидаем [3; 5; 4; 6]

    // 1.44
    let altChurch = checkAlternatingChurch listFloat
    let altList = checkAlternatingList listFloat
    printfn "1.44. Чередование целых и вещественных:"
    printfn "  Church: %b" altChurch  // Ожидаем true (1.0, 2.5, 3.0, 4.5, 5.0)
    printfn "  List: %b" altList      // Ожидаем true

    // 1.54
    let moreThan3Church = moreThanThreeChurch list2
    let moreThan3List = moreThanThreeList list2
    printfn "1.54. Элементы, встречающиеся более 3 раз:"
    printfn "  Church: %A" moreThan3Church  // Ожидаем [4]
    printfn "  List: %A" moreThan3List      // Ожидаем [4]

    0