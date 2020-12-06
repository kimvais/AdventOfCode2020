module AoC2020.Day1

open AoC2020.Utils

let day1 () =
    let entries =
        readInput "1" |> Seq.map int32 |> Seq.cache

    Seq.allPairs entries entries
    |> Seq.filter (fun (a, b) -> (a <> b) && (a + b = 2020))
    |> Seq.head
    |> (fun (a, b) -> a * b)

let combine3 data =
    let input = data |> Array.ofSeq
    let l = (Seq.length input - 1)
    seq {
        for i in 0 .. l do
            for j in 0 .. l do
                for k in 0 .. l do
                    let a = input.[i]
                    let b = input.[j]
                    let c = input.[k]
                    if Seq.distinct [ a; b; c ] |> Seq.length = 3
                    then yield [ a; b; c ]
    }

let day1part2 () =
    let triplets =
        readInput "1" |> Seq.map int32 |> combine3

    triplets
    |> Seq.filter (fun s -> Seq.sum s = 2020)
    |> Seq.head
    |> Seq.reduce (*)
