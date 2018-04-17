type binTree<'a> = 
| Lf 
| Br of 'a * binTree<'a> * binTree<'a>

let myTree = Br(5,(Br(4,Lf,Lf)),(Br(6, Lf, Lf)))

// Prints a BST in ascending order
let rec printTree = function
| Br(data,left,right) -> 
    printTree left
    printf "%d " data
    printTree right
| Lf -> ()
    
// Inserts value into a BST
let rec insert value = function
| Br(data,left,right) ->
    if value < data
    then Br(data,insert value left,right)
    else Br(data,left,insert value right)
| Lf -> Br(value,Lf,Lf)

// Finds the max value in a given BST
let findMax binTree = 
    let rec findInTree acc = function
    | Br(data,left,right) -> 
        findInTree data right
    | Lf -> acc
    findInTree 0 binTree

// Searches for a value and deletes the value
let rec searchTree value = function
| Br(data,left,right) ->
    match left,right with
    | Lf, Lf ->
        Lf
    | left, Lf ->
        if value < data
        then Br(data,searchTree value left,right)
        elif value > data
        then Br(data,left,searchTree value right)
        else Br(data,searchTree value left,right)
    | Lf, right ->
        printf "Left Branch and right Leaf"
        Br(data,searchTree value left,right)
    | left, right ->
        printf "Left Branch and right Leaf"
        Br(data,searchTree value left,right)
| Lf -> Lf

let rec buildtree = function
| [] -> Lf
| x::xs -> insert xs (buildtree xs)