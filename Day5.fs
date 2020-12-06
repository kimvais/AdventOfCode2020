module AoC2020.Day5

open System.Numerics
open AoC2020.Utils

let convertToBinary c =
    match c with
    | 'F'
    | 'L' -> 0
    | 'B'
    | 'R' -> 1

let getDigitValue (i: int) (c: int) = (bigint c) * 2I ** i |> int

let makeNumber: (string -> int) =
    Seq.map convertToBinary
    >> Seq.rev
    >> Seq.mapi getDigitValue
    >> Seq.sum

let getRowAndCol n =
    // Not needed for solution, but adding here for completeness sake and in case
    // we come back to this problem in the following days ...
    let row = n >>> 3
    let col = n &&& 0b111
    (row, col)

let day5 fn () =
    readInput fn
    |> Seq.map makeNumber
    |> Seq.max
    |> int64

let day5part2 fn () =
    readInput fn
    |> Seq.map makeNumber
    |> Seq.sort
    |> Seq.windowed 2
    |> Seq.filter (fun [| a; b |] -> b = a + 2)
    |> Seq.head
    |> (fun [| a; _ |] -> a + 1)
    |> int64
