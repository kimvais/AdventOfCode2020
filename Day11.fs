module AoC2020.Day11

open System
open AoC2020.Utils

let printSeats seats =
    printfn ""
    for row in seats do
        printfn "%s" (row |> Array.map string |> String.concat "")

let findNeighbours (seats: char [] []) x y =
    let numRows = Array.length seats
    let numCols = Array.length seats.[0]

    let areValid x y =
        x >= 0 && y >= 0 && x < numRows && y < numCols

    [ x - 1, y - 1
      x - 1, y
      x - 1, y + 1
      x, y - 1
      x, y + 1
      x + 1, y - 1
      x + 1, y
      x + 1, y + 1 ]

    |> Seq.filter (fun (x, y) -> areValid x y)
    |> Seq.map (fun (x, y) -> seats.[x].[y])


let getVectors (maxx: int) (maxy: int) (x: int) (y: int) =
    seq {
        Seq.initInfinite (fun i -> (x - i, y)) // North
        Seq.initInfinite (fun i -> (x - i, y + i)) // Northeast
        Seq.initInfinite (fun i -> (x, y + i)) // East
        Seq.initInfinite (fun i -> (x + i, y + i)) // Southeast
        Seq.initInfinite (fun i -> (x + i, y)) // South
        Seq.initInfinite (fun i -> (x + i, y - i)) //Southwest
        Seq.initInfinite (fun i -> (x - i, y)) // West
        Seq.initInfinite (fun i -> (x - i, y - i)) // Northwest
    }
    |> Seq.map (Seq.takeWhile (fun (a, b) -> a >= 0 && a < maxx && b >= 0 && b < maxy))
    
let findVisible (seats: char [] []) x y =
    let vecs = getVectors (Seq.length seats) (Seq.length seats.[0]) x y
    vecs |> Seq.choose (Seq.tryFind (fun (x,y) -> seats.[x].[y] <> '.')) |> Seq.map (fun (x,y) -> seats.[x].[y])

let countNeighbours findFn seats x y =
    findFn seats x y
    |> Seq.map (function
        | 'L'
        | '.' -> 0
        | '#' -> 1)
    |> Seq.sum

let getNewState findFn maxOccupied (seats: char [] []) x y =
    match seats.[x].[y] with
    | '.' -> '.'
    | c ->
        match countNeighbours findFn seats x y with
        | n when n = 0 -> '#'
        | n when n >= maxOccupied -> 'L'
        | _ -> c

let reseat findFn maxOccupied (seats: char [] []) =
    printSeats seats
    seq {
        for row in 0 .. (Seq.length seats - 1) do
            yield seq {
                      for col in 0 .. (Seq.length seats.[0] - 1) do
                          yield getNewState findFn maxOccupied seats row col
                  }
                  |> Array.ofSeq
    }
    |> Array.ofSeq

let loadSeatMap fn =
     readInput fn
        |> Seq.map (Array.ofSeq)
        |> Array.ofSeq

let day11 fn part () =
    let allSeats = loadSeatMap fn
    let reseat =
        match part with
        | "1" -> reseat findNeighbours 4
        | "2" -> reseat findVisible 5

       
    let unfolder state =
        let newState = reseat state
        if newState = state then None else Some(newState, newState)

    let seatingProgress = Seq.unfold unfolder allSeats
    seatingProgress
    |> Seq.last
    |> Array.concat
    |> Array.filter (fun c -> c = '#')
    |> Array.length
    |> int64
