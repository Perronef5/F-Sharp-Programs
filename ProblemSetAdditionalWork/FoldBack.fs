module foldback

    //FoldBack list

    let sumListBack list = List.foldBack (fun elem acc -> acc + elem) list 5
    printfn "sumListBack result: %d" (sumListBack [1; 2; 3])

    let copyList list = List.foldBack (fun elem acc -> elem::acc) list []
    printfn "copyList result: %A" (copyList [1 .. 10])
   
    let sumAList list =
        try
            List.reduce (fun acc elem -> acc + elem) list
        with
           | :? System.ArgumentException as exc -> 0

    let resultSum = sumAList [2; 4; 10]
    printfn "%d " resultSum

    let foldingBack list = List.foldBack (fun x acc -> x :: acc) [] list
    // val it : int list = [1; 2; 3; 4; 5]

    printfn "list of foldBack: %A" (foldingBack[1; 2; 3; 4; 5])


