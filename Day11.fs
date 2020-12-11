module AoC2020.Day11

open System
open AoC2020.Utils

let printSeats seats =
    for row in seats do
        printfn "%s" (row |> Array.map string |> String.concat "")
let findNeighbours (seats: char [] []) x y =
        let numRows = Array.length seats
        let numCols = Array.length seats.[0]
        let areValid x y =
            x >= 0 && y >= 0 && x < numRows && y < numCols
        [ x - 1, y - 1
          x - 1, y
          x - 1, y
          x, y - 1
          x, y + 1
          x + 1, y - 1
          x + 1, y 
          x + 1, y + 1 ]

        |> Seq.filter (fun (x, y) -> areValid x y )
        |> Seq.map (fun (x,y) -> seats.[x].[y])

let countNeighbours seats x y =
    findNeighbours seats x y
    |> Seq.map (function
        | 'L'
        | '.' -> 0
        | '#' -> 1)
    |> Seq.sum

let reseat (seats: char [][]) =
    let getNewState x y =
        match seats.[x].[y] with
        | '.' -> '.'
        | _ ->
            match countNeighbours seats x y with
            | n when n < 4 -> '#'
            | _ -> 'L'
    seq {
        for row in { 0 .. (Seq.length seats - 1)} do
            yield seq {
                for col in {0 .. (Seq.length seats.[0] - 1)} do
                    yield getNewState row col
            } |> Array.ofSeq
    }
    |> Array.ofSeq
    
let day11 fn () =
    let allSeats =
        readInput fn
        |> Seq.map (Array.ofSeq)
        |> Array.ofSeq

    printSeats allSeats

    let unfolder state =
        printfn ""
        let newState = reseat state
        if newState = state then
            None
        else
            newState |> printSeats
            Some (newState, newState)
    let seatingProgress = Seq.unfold unfolder allSeats
    seatingProgress |> Seq.take 5 |> Seq.iter (fun _ -> ())
    seatingProgress|> Seq.head |> Array.concat |> Array.filter (fun c -> c = '#') |> Array.length |> int64