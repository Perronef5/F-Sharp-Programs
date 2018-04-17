open System

let loop_stuff() = 

    // Guessing Game
    //let magic_num = "7"
    //let mutable guess = ""

    //while not (magic_num.Equals(guess)) do
    //    printf "Guess the Number : "
    //    guess <- Console.ReadLine()

    //printfn "You Guessed the Number"

    for i = 1 to 10 do 
        printfn "%i" i

    for i = 10 downto 1 do
        printfn "%i" i

    for i in [1..10] do 
        printfn "%i" i

    [1..10] |> List.iter (printfn "Num : %i")

    let sum = List.reduce (+) [1..10]
    printfn "Sum : %i" sum 

loop_stuff()

Console.ReadKey() |> ignore
