module AoC2020.Day15

open AoC2020.Utils

let generateNumber state =
    let previous =
        match state with
        | head :: tail ->
            match tail |> List.tryFindIndex ((=) head) with
            | Some i -> i + 1
            | None -> 0

    [ previous ] @ state

let day15 (input: string) n () =
    let numbers =
        input.Split(",")
        |> List.ofSeq
        |> List.map int
        |> List.rev

    let count = n - List.length numbers
    let mutable ls = numbers
    for i in [1..count] do
        if i % 1000 = 0 then printfn "%d" i
        ls <- generateNumber ls
    
    ls |> List.head |> int64
