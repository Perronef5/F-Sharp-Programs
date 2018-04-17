module ArithParser

type TERMINAL = ADD | SUB | MULT | DIV | OPEN | CLOSE | EOF | ID
type parse_tree = 
| Lf of TERMINAL
| Br of parse_tree * parse_tree * parse_tree

let eat token = function
| [] -> failwith "error, empty token"
| x::xs -> if(x.Equals token) then xs else failwith "error, wrong token"

let rec E = function
| [] -> failwith "error"
| xs -> 
    let leftTuple = xs |> T
    let t::toks = snd leftTuple
    match t with
    ADD | SUB -> 
        let rightTuple = toks |> T
        (Br(fst leftTuple, Lf t, fst rightTuple), snd rightTuple)
    | _ -> leftTuple

and T = function 
| [] -> failwith "error"
| xs ->
   let leftTuple = xs |> F
   let t::toks = snd leftTuple
   match t with
   | MULT | DIV -> 
        let rightTuple =  toks |> F
        (Br(fst leftTuple, Lf t, fst rightTuple), snd rightTuple)
   | _ -> leftTuple

and F = function
| [] -> failwith "error"
| x::xs -> 
    match x with 
    | OPEN -> 
        //x::xs |> eat OPEN |> E |> eat CLOSE
        let toks = x::xs |> eat OPEN
        let opTuple = toks |> E
        let closeToks = snd opTuple |> eat CLOSE
        (Br(Lf OPEN, fst opTuple , Lf CLOSE), closeToks)
    | ID -> 
        //x::xs |> eat ID
        (Lf(ID), xs)
    | _ -> failwith "error"
 

let test_program program = 
    let result  = program |> E
    match snd result with
    | [] -> failwith "Early EOF or missing EOF"
    | x::xs -> if x = EOF then printf "Valid program\n" else failwith "Invalid Program"
 
