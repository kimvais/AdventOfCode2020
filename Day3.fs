module AoC2020.Day3

open AoC2020.Utils

module Seq =
    let repeatForever s =
        let c = Seq.cache s
        seq {
            while true do
                yield! c
        }

    let filteri f s =
        s
        |> Seq.mapi (fun i v -> (i, v))
        |> Seq.filter (fun v -> f (fst v) (snd v))
        |> Seq.map (fun v -> snd v)

let takeEveryNth n i s = s |> Seq.skip (n * i) |> Seq.head

let isTree c =
    match c with
    | '#' -> 1
    | '.' -> 0

let toboccan y x =
    readInput "3"
    |> Seq.filteri (fun i _ -> i % x = 0)
    |> Seq.map Seq.repeatForever
    |> Seq.mapi (takeEveryNth y)
    |> Seq.map isTree
    |> Seq.sum

let day3 () =
    toboccan 3 1 |> printfn "%A"
    0

let day3part2 () =
    (* Right 1, down 1.
       Right 3, down 1. (This is the slope you already checked.)
       Right 5, down 1.
       Right 7, down 1.
       Right 1, down 2 .*)

    [ (1, 1)
      (3, 1)
      (5, 1)
      (7, 1)
      (1, 2) ]
    |> Seq.map (fun (x, y) -> (toboccan x y |> bigint))
    |> Seq.reduce (*)
    |> printfn "%A"
    0
