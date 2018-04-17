module TailRecursion

let map f l =
    let rec loop acc = function
        | [] -> List.rev acc
        | x::xs -> loop (f x::acc) xs
    loop [] l


let map2 f l =
    let rec loop cont = function
        | [] -> cont []
        | x::xs -> loop ( fun acc -> cont (f x::acc) ) xs
    loop id l
