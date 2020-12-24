module AoC2020.Day24

open AoC2020.Utils

(*
12     WW NW     -1,-1  -1,0
304    SW -- NE   0,-1   0,0  0,+1
 56       SE EE         +1,0  +1,+1
*)

type Direction =
    | W
    | NW
    | NE
    | E
    | SE
    | SW

let getOffset =
    function
    | W -> -1, -1
    | NW -> -1, 0
    | NE -> 0, 1
    | E -> 1, 1
    | SE -> 1, 0
    | SW -> 0, -1


let rec parseDirections dirs =
    match dirs with
    | [] -> None
    | c :: ls when c = 'w' -> Some(W, ls)
    | c :: ls when c = 'e' -> Some(E, ls)
    | c :: ls when c = 's' ->
        match List.head ls with
        | 'w' -> Some(SW, List.tail ls)
        | 'e' -> Some(SE, List.tail ls)
    | c :: ls when c = 'n' ->
        match List.head ls with
        | 'w' -> Some(NW, List.tail ls)
        | 'e' -> Some(NE, List.tail ls)

let day24 fn () =
    let input = readInput fn

    let directions =
        input
        |> Seq.map (List.ofSeq >> List.unfold parseDirections)

    directions
    |> Seq.map
        ((List.map getOffset)
         >> (List.fold (fun (x1, y1) (x2, y2) -> (x1 + x2, y1 + y2)) (0, 0)))
    |> Seq.countBy id
    |> Seq.filter (fun (_, n) -> n % 2 = 1)
    |> Seq.length
    |> int64
