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

    ship.X
