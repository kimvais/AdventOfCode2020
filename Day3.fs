module AoC2020.Day3

open AoC2020.Utils


let takeEveryNth n i s = s |> Seq.skip (n * i) |> Seq.head

let isTree c =
    match c with
    | '#' -> 1
    | '.' -> 0

let toboccan inputFn y x =
    readInput inputFn
    |> Seq.filteri (fun i _ -> i % x = 0)
    |> Seq.map Seq.repeatForever
    |> Seq.mapi (takeEveryNth y)
    |> Seq.map isTree
    |> Seq.sum

let day3 fn () = toboccan fn 3 1

let day3part2 fn () =
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
    |> Seq.map (fun (x, y) -> (toboccan fn x y |> bigint))
    |> Seq.reduce (*)
    |> int
