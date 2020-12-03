module AoC2020.Day3


module Seq =
    let repeatForever s =
        let c = Seq.cache s
        seq {
            while true do
                yield! c
       }

let takeEveryThird n s =
    s|> Seq.skip (3 * n) |> Seq.head
   
let isTree c =
    match c with
    | '#' -> 1
    | '.' -> 0
