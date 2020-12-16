module AoC2020.Day16

open System.Text.RegularExpressions
open AoC2020.Utils

type Field =
    { Name: string
      LowRange: int * int
      HighRange: int * int
      Candidates: int Set }

let fieldRe =
    Regex("^(?<fieldName>[a-z ]+): (\d+)-(\d+) or (\d+)-(\d+)$")

let parseField field =
    let m = fieldRe.Match(field)
    [ m.Groups.[1].Value
      m.Groups.[2].Value
      m.Groups.[3].Value
      m.Groups.[4].Value ]
    |> List.map int

let parseField2 (numOfFields: int) field =
    let m = fieldRe.Match(field)
    let g = m.Groups
    { Name = g.[5].Value
      LowRange = (int g.[1].Value, int g.[2].Value)
      HighRange = (int g.[3].Value, int g.[4].Value)
      Candidates = Seq.init numOfFields id |> Set.ofSeq }

let isInvalid ranges v =
    match Seq.forall (fun (lo, hi) -> v < lo || v > hi) ranges with
    | true -> Some v
    | false -> None

let validRanges =
    Seq.map parseField
    >> Seq.concat
    >> Seq.chunkBySize 2
    >> Seq.map (fun [| a; b |] -> a, b)

let parseTicket (s:string) =
    s.Split(",") |> Seq.map int
    
let day16loader fn =
    let [ fieldData; myTicket; ticketData ] =
        (readInputDelimByEmptyLine fn |> List.ofSeq)

    let fields = splitByLinefeed fieldData
    let my = splitByLinefeed myTicket |> Seq.last |> parseTicket |> Array.ofSeq

    let tickets =
        splitByLinefeed ticketData
        |> Seq.tail
        |> Seq.map parseTicket

    tickets, fields, my

let day16 fn () =
    let tickets, fields, _ = day16loader fn
    let ranges = validRanges fields
    tickets
    |> Seq.choose (Seq.tryPick (isInvalid ranges))
    |> Seq.sum
    |> int64

let checkValidity value field =
    (value
     >= fst field.LowRange
     && value <= snd field.LowRange)
    || (value
        >= fst field.HighRange
        && value <= snd field.HighRange)

let eliminate field (ticket: int array) =
    let validFields =
        field.Candidates
        |> Set.filter (fun i -> checkValidity ticket.[i] field)

    { field with Candidates = validFields }

let day16part2 fn () =
    let tickets, fields, myTicket = day16loader fn
    
    let ranges = validRanges fields
    
    let fieldDefs =
        fields
        |> Seq.map (parseField2 (Seq.length fields))
        |> Array.ofSeq
    
    let validTickets =
        Seq.filter (fun t ->
            match Seq.tryPick (isInvalid ranges) t with
            | None -> true
            | Some _ -> false) tickets
    
    for values in validTickets |> Seq.map Array.ofSeq do
        fieldDefs
        |> Array.iteri (fun i v -> (fieldDefs.[i] <- eliminate v values))
    
    let mutable recognized = Set.empty<int>
    
    let departureFields =
        seq {
            for field in fieldDefs
                         |> Array.sortBy (fun f -> Set.count f.Candidates) do
                let matched =
                    Set.difference field.Candidates recognized
                    |> Seq.exactlyOne

                recognized <- field.Candidates
                printfn "%s: %d" field.Name matched
                yield (field.Name, matched)
        }
        |> Map.ofSeq
        |> Map.filter (fun k _ -> k.[..9] = "departure ")
        |> Map.toSeq
        |> Seq.map snd
    
    departureFields |> Seq.map (fun i -> int64 myTicket.[i]) |> Seq.reduce (*)
