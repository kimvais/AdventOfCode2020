module AoC2020.Day15

let rec play round previous roundsToPlay seen =
    if round % 1_000_000 = 0 then
        printfn "Round: %d" round
    match round = roundsToPlay with
    | true -> previous
    | false ->
        let newNumber =
            match seen |> Map.tryFind previous with
            | None -> 0
            | Some n -> (round - 1) - n

        play (round + 1) newNumber roundsToPlay (seen |> Map.add previous (round - 1))


let day15 (input: string) n () =
    let numbers =
        input.Split(",")
        |> List.ofSeq
        |> List.map int

    let seen =
        numbers
        |> List.mapi (fun i e -> (e, i + 1))
        |> Map.ofList

    play (List.length numbers + 1) (List.last numbers) (n + 1) seen
    |> int64
