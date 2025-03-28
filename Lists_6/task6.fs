open System

// Определение типа двоичного дерева
type BinaryTree =
    | Empty  // Пустое дерево
    | Node of string * BinaryTree * BinaryTree  // Узел с значением (string) и двумя поддеревьями (левым и правым)

// Функция вставки элемента в дерево
let rec insert (value: string) (tree: BinaryTree) =
    match tree with
    | Empty -> Node(value, Empty, Empty)  // Если дерево пустое, создаём новый узел
    | Node(nodeValue, left, right) ->
        if value < nodeValue then
            Node(nodeValue, insert value left, right)  // Вставляем в левое поддерево, если значение меньше
        else if value > nodeValue then
            Node(nodeValue, left, insert value right)  // Вставляем в правое поддерево, если значение больше
        else
            tree  // Если значение уже есть, оставляем дерево без изменений

// Функция для прямого обхода дерева (pre-order: корень, левое, правое)
let rec preOrder (tree: BinaryTree) =
    match tree with
    | Empty -> []
    | Node(value, left, right) ->
        value :: (preOrder left @ preOrder right)  // Собираем список значений

// Функция для создания дерева из списка
let buildTree (values: string list) =
    let rec build values tree =
        match values with
        | [] -> tree
        | head :: tail -> build tail (insert head tree)
    build values Empty

[<EntryPoint>]
let main argv =
    // Пример использования
    let values = ["banana"; "apple"; "cherry"; "date"; "apple"]  // "apple" дублируется, будет проигнорирован
    let tree = buildTree values
    let orderedList = preOrder tree
    printfn "Дерево в порядке прямого обхода: %A" orderedList
    0