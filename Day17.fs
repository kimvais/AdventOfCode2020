module AoC2020.Day17

open AoC2020.Utils

let rounds = 6
let startingGridSize = 8

type Cell = int * int * int

let getInitialState =
    function
    | '.' -> false
    | '#' -> true

let offsets = [ 0; -1; 1 ]

let neighbourCoordOffsets =
    seq {
        for x in offsets do
            for y in offsets do
                for z in offsets do
                    yield x, y, z
    }
    |> Seq.tail // Skip 0,0,0

let getNeighbours (x, y, z) =
    neighbourCoordOffsets
    |> Seq.map (fun (x', y', z') -> x + x', y + y', z + z')
    |> Set.ofSeq

let populate soup: Cell Set =
    seq {
        for x, row in Seq.indexed soup do
            for y, cell in Seq.indexed row do
                if getInitialState cell then
                    yield x, y, 0
    }
    |> Set.ofSeq

let getState cells c =
    // printfn "%A" c
    let numNeighbours cells cell =
        Set.intersect cells (getNeighbours cell)
        |> Set.count

    let isAlive cells cell = Set.contains cell cells
    match (isAlive cells c, numNeighbours cells c) with
    | (true, 2) -> true
    | (_, 3) -> true
    | _ -> false

let rec play cells round =
    printfn "Round %d: %d cells alive" round (Set.count cells)
    printfn "%A" cells
    match round with
    | 6 -> cells
    | _ ->
        let neighbours =
            cells
            |> Set.map (fun c -> (getNeighbours c))
            |> Set.unionMany

        let toEvaluate = Set.union cells neighbours

        let cells' =
            toEvaluate |> Set.filter (getState cells)

        play cells' (round + 1)

let day17 fn () =
    let soup = readInput fn
    let cells = populate soup
    let final = play cells 0
    Set.count final |> int64
