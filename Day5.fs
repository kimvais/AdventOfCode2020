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

let day5 () =
    readInput "5"
    |> Seq.map makeNumber
    |> Seq.max
    |> printfn "%A"
    0

let day5part2 () =
    readInput "5"
    |> Seq.map makeNumber
    |> Seq.sort
    |> Seq.windowed 2
    |> Seq.filter (fun [| a; b |] -> b = a + 2)
    |> Seq.head
    |> (fun [| a; _ |] -> a + 1)
    |> printfn "%d"
    0
