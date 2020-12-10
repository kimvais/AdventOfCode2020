module AoC2020.Day10

open AoC2020.Utils

let day10 fn () =
    let input = readInput fn |> Seq.map int |> Seq.sort

    let ratings =
        [ seq { 0 }
          input
          seq { Seq.max input + 3 } ]
        |> Seq.concat

    ratings
    |> Seq.pairwise
    |> Seq.map (fun (a, b) -> b - a)
    |> Seq.groupBy id
    |> Seq.map (snd >> Seq.length)
    |> Seq.reduce (*)
    |> int64

let day10part2 fn () = 0L
