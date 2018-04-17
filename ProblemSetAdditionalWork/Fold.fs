module fold 

    //Fold list

    let reverseList list = List.fold (fun acc elem -> elem::acc) [] list
    printfn "reverseList result: %A" (reverseList [1 .. 10])

    let listSum list = List.fold (fun acc elem -> acc + elem) 6 list
    printfn "Sum of the elements of list %A is %d." [ 1 .. 3 ] (listSum [ 1 .. 3 ])

    //Difference Between fold and foldBack list

    let folding list = List.fold (fun acc x -> x :: acc) [] list
    // val it : int list = [5; 4; 3; 2; 1]
 
    printfn "list of fold: %A" (folding[1; 2; 3; 4; 5])
