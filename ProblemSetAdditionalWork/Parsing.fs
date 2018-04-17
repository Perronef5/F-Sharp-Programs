let A_Parser str =
    if String.IsNullOrEmpty(str) then
        (false,"")
    else if str.[0] = 'A' then
        let remaining = str.[1..]
        (true,remaining)
    else
        (false,str)


val A_Parser :
    string -> (bool * string)


let inputABC = "ABC"
A_Parser inputABC

let inputZBC = "ZBC"
A_Parser inputZBC  

// new comment :D
