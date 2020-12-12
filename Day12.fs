module AoC2020.Day12

open AoC2020.Utils

type Direction =
    | North
    | East
    | South
    | West

type Movement = | Forward

type Turn =
    | Left
    | Right


type Ship = { X: int; Y: int; Facing: Direction }
type Vector = { X: int; Y: int }

type VectoredShip = { X: int; Y: int; Waypoint: Vector }

let turnRight =
    function
    | North -> East
    | East -> South
    | South -> West
    | West -> North

let turnLeft =
    function
    | North -> West
    | East -> North
    | South -> East
    | West -> South

let turnShip dir degrees ship =
    let times = degrees / 90

    let turnFn =
        match dir with
        | Left -> turnLeft
        | Right -> turnRight

    let folder f _ = turnFn f

    let newFacing =
        [ 0 .. times - 1 ] |> Seq.fold folder ship.Facing

    { ship with Facing = newFacing }


let move dir distance (ship: Ship) =
    match dir with
    | North -> { ship with X = ship.X - distance }
    | East -> { ship with Y = ship.Y + distance }
    | South -> { ship with X = ship.X + distance }
    | West -> { ship with Y = ship.Y - distance }

let moveShip (direction: Direction option) distance ship =
    let dir =
        match direction with
        | Some d -> d
        | None -> ship.Facing

    move dir distance ship

let moveVector dir distance (ship: VectoredShip) =
    let vec = ship.Waypoint

    let newVec =
        match dir with
        | North -> { vec with X = vec.X - distance }
        | East -> { vec with Y = vec.Y + distance }
        | South -> { vec with X = vec.X + distance }
        | West -> { vec with Y = vec.Y - distance }

    { ship with Waypoint = newVec }

let rotateVector dir arg (ship: VectoredShip) =
    let times = arg / 90

    let rotFn =
        match dir with
        | Right -> fun x y -> (y, -x)
        | Left -> fun x y -> (-y, x)

    let folder (x, y) _ = rotFn x y

    let x, y =
        [ 0 .. times - 1 ]
        |> Seq.fold folder (ship.Waypoint.X, ship.Waypoint.Y)

    { ship with
          Waypoint = { X = x; Y = y } }

let moveShip2 arg ship =
    let x = ship.X
    let y = ship.Y
    let deltax = ship.Waypoint.X * arg
    let deltay = ship.Waypoint.Y * arg
    // printfn "Moving from %d,%d %d times on %d/%d" x y arg deltax deltay
    { ship with
          X = x + deltax
          Y = y + deltay }

let parseInstruction (s: string) =
    let arg = int s.[1..(String.length s - 1)]
    match s.[0] with
    | 'N' -> moveShip (Some North) arg
    | 'E' -> moveShip (Some East) arg
    | 'S' -> moveShip (Some South) arg
    | 'W' -> moveShip (Some West) arg
    | 'L' -> turnShip Left arg
    | 'R' -> turnShip Right arg
    | 'F' -> moveShip None arg

let parseInstructions2 (s: string) =
    let arg = int s.[1..(String.length s - 1)]
    match s.[0] with
    | 'N' -> moveVector North arg
    | 'E' -> moveVector East arg
    | 'S' -> moveVector South arg
    | 'W' -> moveVector West arg
    | 'L' -> rotateVector Left arg
    | 'R' -> rotateVector Right arg
    | 'F' -> moveShip2 arg


let day12 fn () =
    let ship = { X = 0; Y = 0; Facing = East }
    let folder s fn = fn s
    let instructions = readInput fn |> Seq.map parseInstruction
    instructions
    |> Seq.fold folder ship
    |> (fun s -> abs s.X + abs s.Y)
    |> int64

let day12part2 fn () =
    let ship =
        { X = 0
          Y = 0
          Waypoint = { X = -1; Y = 10 } }
    let folder s fn =
        // printfn "%A" s
        fn s
    let instructions = readInput fn |> Seq.map parseInstructions2
    instructions
    |> Seq.fold folder ship
    |> (fun s -> abs s.X + abs s.Y)
    |> int64
