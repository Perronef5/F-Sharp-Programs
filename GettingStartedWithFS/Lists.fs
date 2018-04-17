// ** Working With Lists -- * //
let list_practice() =
    // Define a list literal
    let list1 = [1; 2; 3; 4]
 
    // Print list
    list1 |> List.iter (printfn "Num : %i")
 
    // Print list
    printfn "%A" list1
 
    // Use cons operator 
    let list2 = 5::6::7::[]
    printfn "%A" list2
 
    // Use ranges
    let list3 = [1..5]
    let list4 = ['a'..'g']
    printfn "%A" list4
 
    // Get length
    printfn "Length : %i" list1.Length
 
    // Check if empty
    printfn "Empty : %b" list2.IsEmpty
 
    // Get item at index
    printfn "Index 2 : %c" (list4.Item(2))
 
    // Get the 1st item
    printfn "Head : %c" (list4.Head)
 
    // Get the tail
    printfn "Tail : %A" (list4.Tail)
 
    // Filter out only evens
    let list9 = list3 |> List.filter (fun x -> x % 2 = 0)
    printfn "Evens : %A" list9
 
    // Multiply all values times themselves
    let list10 = list9 |> List.map (fun x -> (x * x))
    printfn "Squares : %A" list10
 
    // Sort a list
    printfn "Sorted : %A" (List.sort [5; 4; 3])
 
    // Sum a list with fold
    printfn "Sum : %i" (List.fold (fun sum elem -> sum + elem) 0 [1;2;3])
