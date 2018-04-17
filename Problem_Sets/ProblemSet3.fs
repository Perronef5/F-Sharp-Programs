module ProblemSet3

module P1

    // Part A
    type LinkedList<'a> = 
        | Empty
        | Cons of head:'a * tail:LinkedList<'a>


    //type binTree<'a> = 
        //| Lf 
        //| Br of 'a * binTree<'a> * binTree<'a>


        //( 4,3,5,7,2,1)
      
    //let tree = binTree(Br(4, Br(3,Br(2, Br(1, Lf, Lf), Lf),Lf), Br(5, Lf, Br(7, Lf, Lf))))


module P2

    // A) ZERO and ONE

    type TERMINAL = ZERO | ONE

    // B) string (ZERO, ZERO, ONE, ONE, ONE, ONE, ZERO, ZERO)

module P5

    // Tail Recursive Interleave Function
    let interleave (as,bs) = 
        let rec loop (xs,ys) zs = 
            match xs,ys with 
            | ([],[]) -> zs
            | (x::xs,y::ys) -> loop ((xs,ys),x::y::zs)
        loop (as,bs) []

module p6

  // Problem 6
  
  // Part A
  let seqInfinite = Seq.initInfinite(fun index -> 
        let n = float(index + 1)
        1.0/2.0**n*(if ((index+1)%2 = 0) then -1.0
                    else 1.0))

    // Part B
    let seq1 = seqInfinite;;
    for x in (seqInfinite) do printfn "%f.2" x;;

    // Part c
    // Stream definition
    type 'a Stream = Cons of 'a * (unit -> 'a Stream)
    let rec myStream n = 
        if n % 2 = 0
        then Cons((2.0 ** float n) * float(-1) , fun() -> myStream(n+1))
        else Cons(2.0 ** float n , fun() -> myStream(n+1))

    // Return frist n values from the stream
    let rec take n (Cons(x,xsf)) = 
        if n = 0
        then []
        else x:: take (n-1) (xsf())

    // Drops first n values from the stream
    let rec drop n (Cons(x,xsf)) = 
        if n = 0 
        then Cons(x,xsf)
        else drop (n-1) (xsf())

    let myStreamList = take 11 (drop 4 (myStream 1))
    // Print sequence 
    printfn "\nInfinte stream list:"
    for x in myStreamList do
        printfn "1.0/%f" x
    printfn "\n"


module P8

    type TERMINAL = IF|THEN|ELSE|BEGIN|END|PRINT|SEMICOLON|ID|EOF
    type parse_tree = 
        | Lf of TERMINAL
        | IF_Tree of parse_tree * parse_tree * parse_tree * parse_tree * parse_tree * parse_tree
        | Begin_Tree of parse_tree * parse_tree * parse_tree
        | Print_Tree of parse_tree * parse_tree
        | End_Tree of parse_tree
        | Semi_Tree of parse_tree * parse_tree * parse_tree
        | ID_Tree of parse_tree

    let eat token = function
    | [] -> failwith "error, empty token!"
    | x::xs -> if (x.Equals token) then xs else failwith "Wrong token!"

    let rec E = function
    | [] -> failwith "Empty Token!"
    | x::xs ->
        match x with
        // Return Tree and remaining tokens
        | ID -> (ID_Tree(Lf ID), xs)
        | _ -> failwith "Error, Incorrect Token"

    let rec L = function
    | [] -> failwith "Error, empty token!"
    | x::xs ->
        match x with
        // Return Tree and remaining tokens
        | END -> (End_Tree(Lf END), xs)
        | SEMICOLON -> //xs |> S |> L
            let (s_tree, s_tokens) = xs |> S
            let (l_tree, remain) = s_tokens |> L
            (Semi_Tree(Lf(SEMICOLON),s_tree,l_tree), remain)
        | _ -> failwith "Error, Invalid Token!"

    and S = function
    | [] -> failwith "Error, empty token!"
    | x::xs ->
        match x with 
        // Return tree and remaining tokens
        | IF -> // xs |> E |> eat THEN |> S |> eat ELSE |> S
            let (e_tree, e_tokens) = xs |> E
            let remain1 = e_tokens |> eat THEN
            let (s1_tree, s1_tokens) = remain1 |> S
            let remain2 = s1_tokens |> eat ELSE
            let (s2_tree, remain) = remain2 |> S
            //let eTuple = xs |> E
            //let eToks = snd eTuple |> eat THEN
            //let s1Tuple = eToks |> S
            //let s1Toks = snd s1Tuple |> eat ELSE
            //let s2Tuple = s1Toks |> S
            (IF_Tree(Lf IF, e_tree, Lf THEN, s1_tree, Lf ELSE, s2_tree), remain)
        | BEGIN -> // xs |> S |> L
            let (s_tree, s_tokens) = xs |> S
            let (l_tree, remain) = s_tokens |> L
            (Begin_Tree(Lf(BEGIN), s_tree, l_tree), remain)
        | PRINT -> // xs |> E
            let (e_tree, remain) = xs |> E
            (Print_Tree(Lf(PRINT), e_tree), remain)
        | _ -> failwith "Error, Invalid Token!"
         



    let test_program program = 
        let result = program |> S
        match snd result with
        | [] -> failwith "Early Termination or missing EOF"
        | x::xs -> if x = EOF then printf "Valid program" else failwith "Invalid Program!"

module P9

    let powerOf2 exp =
        let rec loop acc counter = 
           if counter > 0I then
              loop (acc * 2I) (counter - 1I)
           else
              acc
        loop 1I exp
    // Testing
    //powerOf2 0I
    //powerOf2 1I
    //powerOf2 2I
    //powerOf2 4I
    //powerOf2 8I
    //powerOf2 10I
    //powerOf2 15I
    //powerOf2 16I

module P11

//(*
//1) f: 'a
//2) f: 'a -> 'b
//3) f: float -> 'b
//4) f: float -> float
//*)

module P12

    let fibonacci n = 
       let prev1 = ref 0
       let prev2 = ref 1
       let countr = ref 1
       let result = ref 1

       while countr < n do
         result := !prev1 + !prev2

         prev1 := !prev2
         prev2 := !result


         countr := !countr + 1

       !result

    // Tests:
    let input = ref 2
    fibonacci input

    fibonacci (ref 4)

module P13

    type Student = {GPA: unit -> float; Credit: unit -> int; AddCredit: int -> unit; AddGradePoints: float -> unit}

    let stu = 
       let cred = ref 0
       let grpntave = ref 0.0
       {GPA = fun () -> !grpntave
        Credit = fun () -> !cred 
        AddCredit = fun n -> cred := !cred + n
        AddGradePoints = fun n -> grpntave := !grpntave + n}

    // Tests:
    stu
    stu.GPA ()
    stu.AddCredit 12
    stu.AddCredit 5
    stu.Credit ()
    stu.AddGradePoints 2.1
    stu.GPA ()
    stu.AddGradePoints 0.50
    stu.GPA ()

module P14

    let mkstack init = 
       let stk = ref init
       ((fun x -> stk := x :: (!stk)),        // push
        (fun () -> stk := List.tail (!stk)),  // pop
        (fun () -> List.head (!stk)))         // top

    let (push, pop, top) = mkstack [1;2;3]


    let factorial n = 
    let currNum = ref 1
    let (push, pop, top) = mkstack ([]: int list)
    let result2 = ref 1

    while !currNum <= n do
    push  !currNum
    currNum := !currNum + 1

    while !currNum > 1 do  
    result2 := !result2 *  top ()
    pop ()
      
    !result2

    factorial  5
