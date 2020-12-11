module AoC2020.Day11

open System
open AoC2020.Utils

let printSeats seats =
    for row in seats do
        printfn "%s" (row |> Array.map string |> String.concat "")
let findNeighbours offset (seats: char array) pos =
        let indices =
            [ pos - offset - 1
              pos - offset
              pos - offset + 1
              pos - 1
              pos + 1
              pos + offset - 1
              pos + offset
              pos + offset + 1 ]

        indices
        |> Seq.filter (fun i -> (i >= 0) && (i < Seq.length seats - 1))
        |> Seq.map (fun i -> seats.[i])

let countNeighbours offset seats pos =
    findNeighbours offset (seats: char array) pos
    |> Seq.map (function
        | 'L'
        | '.' -> 0
        | '#' -> 1)
    |> Seq.sum

let reseat offset (seats: char array) =
    let getNewState i =
        match seats.[i] with
        | '.' -> '.'
        | _ ->
            match countNeighbours offset seats i with
            | n when n < 4 -> '#'
            | _ -> 'L'

    { 0 .. (Seq.length seats - 1)}
    |> Seq.map getNewState
    |> Array.ofSeq
    
let day11 fn () =
    let floorPlan =
        readInput fn
        |> Seq.map (Array.ofSeq)
        |> Array.ofSeq

    printSeats floorPlan
    let offset = floorPlan |> Array.head |> Array.length
    let allSeats = floorPlan |> Array.concat

    let unfolder state =
        printfn ""
        let newState = reseat offset state
        if newState = state then
            None
        else
            newState |> Seq.chunkBySize offset |> printSeats
            Some (newState, newState)
    Seq.unfold unfolder allSeats |> Seq.last |> Array.filter (fun c -> c = '#') |> Array.length |> int64