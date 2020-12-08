module AoC2020.Day8

open System.Text.RegularExpressions
open AoC2020.Utils

type ExecutionState =
    | Running
    | Halted
    | Terminated

type State =
    { Pos: int
      Acc: int
      Visited: int Set
      State: ExecutionState }

let jmp n state () =
    let newPos = state.Pos + n
    match Set.contains newPos state.Visited with
    | true -> { state with State = Halted }
    | false ->
        { state with
              Pos = newPos
              Visited = state.Visited.Add newPos }

let acc n state = jmp 1 { state with Acc = state.Acc + n }

let nop state = jmp 1 state

let getCmd s =
    let re =
        Regex("^(?<cmd>acc|jmp|nop) (?<arg>[+-]\d+)")

    let m = re.Match(s)
    let cmd = m.Groups.["cmd"].Value
    let arg = int m.Groups.["arg"].Value
    cmd, arg

let parseCommand s =
    match getCmd s with
    | ("acc", arg) -> acc arg
    | ("jmp", arg) -> jmp arg
    | ("nop", _) -> nop

let initialState =
    { Pos = 0
      Acc = 0
      Visited = Set.singleton 0
      State = Running }

let execute (program: string array) =
    let mutable state = initialState
    while state.State = Running do
        try
            let op = parseCommand program.[state.Pos]
            state <- op state ()
        with IndexOutOfRangeException -> state <- { state with State = Terminated }
    state

let replaceJump (program: string array) pos =
    let p = Array.copy program
    p.[pos] <- "nop +0"
    p

let findJumpCommands program =
    program
    |> Seq.mapi (fun i e -> i, e)
    |> Seq.filter (fun (_, s) -> fst (getCmd s) = "jmp")
    |> Seq.map fst

let mutate originalProgram =
    let program = originalProgram |> Array.ofSeq
    findJumpCommands originalProgram
    |> Seq.map (fun pos -> replaceJump program pos)

let day8 fn () =
    let program = readInput fn |> Array.ofSeq
    let state = execute program
    state.Acc |> int64

let day8part2 fn () =
    let programs = readInput fn |> mutate

    let state =
        programs
        |> Seq.map execute
        |> Seq.filter (fun s -> s.State = Terminated)
        |> Seq.head

    state.Acc |> int64
