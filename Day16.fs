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

let day16 fn () =
    let [ fieldData; myTicket; ticketData ] =
        (readInputDelimByEmptyLine fn |> List.ofSeq)

    let fields = splitByLinefeed fieldData

    let tickets =
        splitByLinefeed ticketData
        |> Seq.tail
        |> Seq.map (fun s -> s.Split(",") |> Seq.map int)

    let validRanges =
        fields
        |> Seq.map parseField
        |> Seq.concat
        |> Seq.chunkBySize 2
        |> Seq.map (fun [| a; b |] -> a, b)

    let isInvalid v =
        match Seq.forall (fun (lo, hi) -> v < lo || v > hi) validRanges with
        | true -> Some v
        | false -> None

    tickets
    |> Seq.choose (Seq.tryPick isInvalid)
    |> Seq.sum
    |> int64
