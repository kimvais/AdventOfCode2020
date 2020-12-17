module AoC2020.Day17

open AoC2020.Utils

// Fixme: Unnecessary copypasta for 3d and 4d solutions

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

let neighbourCoordOffsets4d =
    seq {
        for x in offsets do
            for y in offsets do
                for z in offsets do
                    for w in offsets do
                        yield x, y, z, w
    }
    |> Seq.tail // Skip 0,0,0,0

let getNeighbours (x, y, z) =
    neighbourCoordOffsets
    |> Seq.map (fun (x', y', z') -> x + x', y + y', z + z')
    |> Set.ofSeq

let getNeighbours4d (x, y, z, w) =
    neighbourCoordOffsets4d
    |> Seq.map (fun (x', y', z', w') -> x + x', y + y', z + z', w + w')
    |> Set.ofSeq

let populate soup =
    seq {
        for x, row in Seq.indexed soup do
            for y, cell in Seq.indexed row do
                if getInitialState cell then yield x, y, 0
    }

let populate4d soup =
    populate soup
    |> Seq.map (fun (x, y, z) -> (x, y, z, 0))

let getState cells c =
    let numNeighbours cells cell =
        Set.intersect cells (getNeighbours cell)
        |> Set.count

    let isAlive cells cell = Set.contains cell cells
    match (isAlive cells c, numNeighbours cells c) with
    | (true, 2) -> true
    | (_, 3) -> true
    | _ -> false

let getState4d cells c =
    let numNeighbours cells cell =
        Set.intersect cells (getNeighbours4d cell)
        |> Set.count

    let isAlive cells cell = Set.contains cell cells
    match (isAlive cells c, numNeighbours cells c) with
    | (true, 2) -> true
    | (_, 3) -> true
    | _ -> false

let rec play cells round =
    printfn "Round %d: %d cells alive" round (Set.count cells)
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

let rec play4d cells round =
    printfn "Round %d: %d cells alive" round (Set.count cells)
    match round with
    | 6 -> cells
    | _ ->
        let neighbours =
            cells
            |> Set.map (fun c -> (getNeighbours4d c))
            |> Set.unionMany

        let toEvaluate = Set.union cells neighbours

        let cells' =
            toEvaluate |> Set.filter (getState4d cells)

        play4d cells' (round + 1)

let day17 fn () =
    let soup = readInput fn
    let cells = populate soup |> Set.ofSeq
    let final = play cells 0
    Set.count final |> int64

let day17part2 fn () =
    let soup = readInput fn
    let cells = populate4d soup |> Set.ofSeq
    let final = play4d cells 0
    Set.count final |> int64
