module AoC2020.Day16

open System.Text.RegularExpressions
open AoC2020.Utils

let fieldRe =
    Regex("^(?<fieldName>[a-z ]+): (\d+)-(\d+) or (\d+)-(\d+)$")

let parseField field =
    let m = fieldRe.Match(field)
    [ m.Groups.[1].Value
      m.Groups.[2].Value
      m.Groups.[3].Value
      m.Groups.[4].Value ]
    |> List.map int

let isInvalid ranges v =
    match Seq.forall (fun (lo, hi) -> v < lo || v > hi) ranges with
    | true -> Some v
    | false -> None

let validRanges =
    Seq.map parseField
    >> Seq.concat
    >> Seq.chunkBySize 2
    >> Seq.map (fun [| a; b |] -> a, b)

let day16solver fn =
    let [ fieldData; myTicket; ticketData ] =
        (readInputDelimByEmptyLine fn |> List.ofSeq)

    let fields = splitByLinefeed fieldData

    let tickets =
        splitByLinefeed ticketData
        |> Seq.tail
        |> Seq.map (fun s -> s.Split(",") |> Seq.map int)

    tickets, validRanges fields

let day16 fn () =
    let tickets, ranges = day16solver fn
    tickets
    |> Seq.choose (Seq.tryPick (isInvalid ranges))
    |> Seq.sum
    |> int64
