module AoC2020.Day18

open AoC2020.Utils
open FParsec

let token p = p .>> spaces
let number n = token pint64 n
let c s = token (skipChar s)

let rec calc s =
    s
    |> chainl1 value (choice [ c '+' >>% (+); c '*' >>% (*) ])

and value s =
    s
    |> choice [ c '(' >>. calc .>> c ')'
                number ]

let rec calc' s =
    s |> chainl1 lowerPrecedence (c '*' >>% (*))

and lowerPrecedence s = s |> chainl1 value' (c '+' >>% (+))

and value' s =
    s
    |> choice [ c '(' >>. calc' .>> c ')'
                number ]

let p calc'' = spaces >>. calc'' .>> eof

let p1 = p calc
let p2 = p calc'

let calculate parser input =
    run parser input
    |> function
    | Success (n, (), _) -> n
    | _ -> failwith "Error in parser!"

let day18 fn () =
    readInput fn |> Seq.map (calculate p1) |> Seq.sum

let day18part2 fn () =
    readInput fn |> Seq.map (calculate p2) |> Seq.sum
