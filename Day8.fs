module AoC2020.Day8

open System.Text.RegularExpressions
open AoC2020.Utils

type State =
    { Pos: int
      Acc: int
      Visited: int Set
      Halted: bool }

let jmp n state () =
    let newPos = state.Pos + n
    match Set.contains newPos state.Visited with
    | true -> { state with Halted = true }
    | false ->
        { state with
              Pos = newPos
              Visited = state.Visited.Add newPos }

let acc n state = jmp 1 { state with Acc = state.Acc + n }

let nop n state = jmp 1 state

let parseCommand s =
    let re =
        Regex("^(?<cmd>acc|jmp|nop) (?<arg>[+-]\d+)")

    let m = re.Match(s)
    let cmd = m.Groups.["cmd"].Value
    let arg = int m.Groups.["arg"].Value
    match cmd with
    | "acc" -> acc arg
    | "jmp" -> jmp arg
    | "nop" -> nop arg

let execute (program: string array) initialState =
    let mutable state = initialState
    while not state.Halted do
        let op = parseCommand program.[state.Pos]
        state <- op state ()
    state

let day8 fn () =
    let initialState =
        { Pos = 0
          Acc = 0
          Visited = Set.empty
          Halted = false }

    let program = readInput fn |> Array.ofSeq
    let state = execute program initialState
    printfn "%A" state
    state.Acc |> int64


let day8part2 fn () = 0L
