// Learn more about F# at http://fsharp.org

open System

// Simple Math Operators
let do_math() = 
    printfn "10 + 4 = %i" (10 + 4)
    printfn "10 - 4 = %i" (10 - 4)
    printfn "10 * 4 = %i" (10 * 4)
    printfn "10 / 4 = %i" (10 / 4)
    printfn "10 %% 4 = %i" (10 % 4)
    printfn "10 ** 2 = %.1f" (10.0 / 4.0)

    // Getting Types and Casting
    let number = 2
    printfn "Type : %A" (number.GetType())
    printfn "A Float : %.2f" (float number)
    printfn "A Integer : %i" (int 3.14)

    // Useful Math Functions
    printfn "abs -1 : %i" (abs -1)
    printfn "ceil 5.5 : %f" (ceil 5.5)
    printfn "floor 5.5 : %f" (floor 5.5)
    printfn "log 2.71828 : %f" (log 2.71828)
    printfn "log10 1000 : %f" (log10 1000.0)
    printfn "sqrt 25 : %f" (sqrt 36.0)

    // (Trig Functions) -- Also tan, acos, asin, atan, cosh, sinh, tanh
    printfn "sin 0 : %f" (sin 0.0)
    printfn "cos 1 : %f" (cos 0.0)



do_math()

Console.ReadKey() |> ignore



