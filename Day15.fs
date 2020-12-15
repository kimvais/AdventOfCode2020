module AoC2020.Day15

open AoC2020.Utils

type State = { Numbers: int list
               Previous: int }

let generateNumber state =
    let previous =
        match state.Numbers with
        | head :: tail ->
            match tail |> List.tryFindIndex ((=) head) with
            | Some i -> i + 1
            | None -> 0

    { state with
          Previous = previous
          Numbers = [ previous ] @ state.Numbers }

let day15 (input: string) n () =
    let numbers =
        input.Split(",")
        |> List.ofSeq
        |> List.map int
        |> List.rev

    let count = n - List.length numbers
    let mutable state = { Numbers = numbers; Previous = List.head numbers }
    for i in [ 1 .. count ] do
        if i % 1000 = 0 then printfn "%d" i
        state <- generateNumber state

    state.Previous |> int64
