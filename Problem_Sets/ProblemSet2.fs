// Problem 1
module P1
    type TERMINAL = IF|THEN|ELSE|BEGIN|END|PRINT|SEMICOLON|ID|EOF

    let eat token =  function
        | [] -> failwith "No token to eat."
        | x::xs ->  if x = token then xs
                    else failwith "Expected a different token"

    let rec S = function
        | [] -> failwith "Early terination at S()"
        | x::xs -> match x with
                    | IF -> xs |> E |> eat THEN |> S |> eat ELSE |> S
                    | BEGIN -> xs |> S |> L
                    | PRINT -> xs |> E
                    | _ -> failwith "Invalid S() call"

    and L = function
        | [] -> failwith "Early terination at L()"
        | x::xs -> match x with 
                    | END -> xs
                    | SEMICOLON -> xs |> S |> L
                    | _ -> failwith "Invalid L() call"

    and E = function
        | [] -> failwith "Early terination at E()"
        | xs -> eat ID xs
    
    let test_program program =
      let result = program |> S
      match result with 
      | [] -> failwith "Early termination or missing EOF"
      | x::xs -> if x = EOF then "Compilation successful"
                 else "Compilation failed"
    
    let test() =
        printf "Testing [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;END;ELSE;PRINT;ID;EOF]... "
        test_program [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;END;ELSE;PRINT;ID;EOF]
        printfn "Success"

        printf "Testing [IF;ID;THEN;IF;ID;THEN;PRINT;ID;ELSE;PRINT;ID;ELSE;BEGIN;PRINT;ID;END;EOF]... "
        test_program [IF;ID;THEN;IF;ID;THEN;PRINT;ID;ELSE;PRINT;ID;ELSE;BEGIN;PRINT;ID;END;EOF]
        printfn "Success"

        printf "Testing [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;SEMICOLON;END;ELSE;PRINT;ID;EOF]... "
        test_program [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;SEMICOLON;END;ELSE;PRINT;ID;EOF]
        printfn "Success"
        
module P3
//Question 3

        let curry a = (fun x -> fun y -> a(x,y))

        let uncurried a = (fun (x,y) -> a x y)
        
module P4
//Question 4 inner curried tail recursion function

        //let rec inner xs ys =
        // match xs, ys with
        // | [], [] -> 0
        // | [], ys -> failwith "Vector not the same size"
        // | xs, [] -> failwith "Vectors not the same size"
        // | x::xs, y::ys -> x*y + inner xs ys

        let rec inner xs =
            let aux ys =
                match xs, ys with 
                | ([], ys) -> failwith "Error"
                | (xs, []) -> failwith "Error"
                | (x::xs, y::ys) -> (x*y) + inner xs ys 
            aux
        let result = inner [3], [2]
        printf "%A" result 
        
module P8
//Question 8
//Rules of syntax of language
        type Exp = 
         Num of int
         | Neg of Exp
         | Sum of Exp * Exp
         | Diff of Exp * Exp
         | Prod of Exp * Exp
         | Quot of Exp * Exp

        //Function to evaluate expressions
        let rec evaluate = function
         | Num n -> Some n
         | Neg e -> match evaluate e with
                    | Some x -> Some (-x)
                    | _ -> None
         | Sum (e1,e2) -> match (evaluate e1, evaluate e2) with
                          | Some x, Some y -> Some(x + y)
                          | _ -> None
         | Diff (e1, e2) -> match (evaluate e1, evaluate e2) with
                            | Some x, Some y -> Some(x - y)
                            | _ -> None
         | Prod (e1, e2) -> match (evaluate e1, evaluate e2) with
                            | Some x, Some y -> Some(x * y) 
                            | _ -> None
         | Quot (e1, e2) -> match (evaluate e1, evaluate e2) with
                            | Some x, Some 0 -> None
                            | Some x, Some y -> Some(x / y)
                            | _ -> None
                            
        let result1 = evaluate (Prod(Num 3, Diff(Num 5, Num 1)));;

/*
A parse tree is an entity which represents the structure of the derivation of a terminal string from some non-terminal 
(not necessarily the start symbol). The definition is as in the book. Key features to define are the root ∈ V and yield ∈ Σ* of each tree.

For each σ ∈ Σ, there is a tree with root σ and no children; its yield is σ

For each rule A → ε, there is a tree with root A and one child ε; its yield is ε

If t1, t2, ..., tn are parse trees with roots r1, r2, ..., rn and respective yields y1, y2, ..., yn, and A → r1r2...rn is a production, 
then there is a parse tree with root A whose children are t1, t2, ..., tn. Its root is A and its yield is y1y2...yn

Observe that parse trees are constructed from bottom up, not top down. The actual construction of "adding children" 
should be made more precise, but we intuitively know what's going on.

As an example, here are all the parse (sub) trees used to build the parse tree for the arithmetic expression 
4 + 3 * 2 using the expression grammar

E → E + T | E - T | T 
T → T * F | F  
F → a | ( E )

where a represents an operand of some type, be it a number or variable. The trees are grouped by height.


Syntactic Expressions for Trees

It's useful to have a non-graphical, syntactic way to express trees as lists. 
Think of the recursive construction of a tree with its subtrees like this:

(root subtree1 ... subtreen)

We want to create a grammar for trees (potential parse trees) for a given grammar G=(V,ΣR,S). The tree grammar uses these symbols:

terminals: V ∪ Σ ∪ { (, ) }, ∪ { 'ε' }, where 'ε' means the symbol ε treated literally
non-terminals: T, S, L, N, R

The rules for creating trees are these:

T → S | L            a Tree is a terminal Symbol or a List
L → ( N  R )         the first element of a List is a Non-terminal (the root)
R → T R | T | 'ε'    following N, one or more Trees or ε (literal), R serves to unRoll the Trees
N → A ∈ V – Σ       the Non-terminals from V are terminals for this tree grammar.
S → σ ∈ Σ

Not every tree constructed in this way can be a parse tree for the grammar, 
but this construction gives us all possible parse trees. 
Let's look at how it would work out for the expression grammar above using the sample expression. 
These are the list expressions of the above trees:


4    +    2    *    3
(F  4)    (F  2)    (F  3)
(T  (F  4))    (T  (F  2))
(E  (T  (F 4)))
(T  (T  (F 2))  *  (F 3)) 
(E  (E  (T  (F 4)))  +  (T (T (F 2))  *  (F 3)))
*/
