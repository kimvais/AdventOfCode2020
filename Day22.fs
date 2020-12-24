module AoC2020.Day22

open AoC2020.Utils

let rec play d1 d2 =
    match d1, d2 with
    | d, [] -> d
    | [], d -> d
    | _ ->
        match List.head d1, List.head d2 with
        | (a, b) when a > b -> play (List.tail d1 @ [ a; b ]) (List.tail d2)
        | (a, b) -> play (List.tail d1) (List.tail d2 @ [ b; a ])

let day22 fn () =
    let decks =
        readInputDelimByEmptyLine fn
        |> Seq.map (splitByLinefeed >> Seq.tail >> Seq.map int)

    let p1 = Seq.head decks |> List.ofSeq
    let p2 = Seq.last decks |> List.ofSeq

    play p1 p2
    |> List.rev
    |> List.mapi (fun i n -> (i + 1) * n)
    |> List.sumBy int64
