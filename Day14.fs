module AoC2020.Day14

open System
open System.Text.RegularExpressions
open AoC2020.Utils

type Bit =
    | Set = 1L
    | Unset = 0L
    | Keep = -1L

let translate =
    function
    | '0' -> Bit.Unset
    | '1' -> Bit.Set
    | 'X' -> Bit.Keep
    | c -> failwith (sprintf "Invalid char %c" c)

let asBitString (n: int64) =
    (sprintf "%36s" (Convert.ToString(n, 2))).Replace(' ', '0')

let maskRe = Regex("^mask = (?<mask>[01X]{36})$")

let memRe =
    Regex("^mem\[(?<address>\d+)\] = (?<value>\d+)$")

let getMask (m: Match) =
    m.Groups.["mask"].Value
    |> Seq.map translate
    |> Array.ofSeq

let applyBit =
    function
    | (Bit.Set, _) -> Bit.Set
    | (Bit.Unset, _) -> Bit.Unset
    | (Bit.Keep, n) -> n

let applyBit2 =
    function
    | (Bit.Set, _) -> Bit.Set
    | (Bit.Unset, n) -> n
    | (Bit.Keep, _) -> Bit.Keep
      
let applyMask (mask: array<Bit>) (value: array<Bit>) =
    Array.zip mask value |> Array.map applyBit

let applyMask2 (mask: array<Bit>) (value: array<Bit>) =
    Array.zip mask value |> Array.map applyBit2
    
let getAddressAndValue line =
    let m = memRe.Match(line)
    let addr = m.Groups.["address"].Value
    let value = m.Groups.["value"].Value
    (int64 addr,
     (value
      |> (int64
          >> asBitString
          >> Seq.map translate
          >> Array.ofSeq)))

let getValue bits =
    bits
    |> Array.rev
    |> Array.mapi (fun e b -> bigint (int b) * (2I ** e) |> int64)
    |> Array.sum

let printMemory mem =
    printfn "\nMemory:"
    mem
    |> Map.iter (fun k v -> printfn "%d: %d" k (getValue v))

let printMask m =
    let v = getValue m
    m
    |> Array.map (function
        | Bit.Set -> "1"
        | Bit.Unset -> "0"
        | Bit.Keep -> "X")
    |> String.concat ""
    |> printfn "%d %s" v

let runProgram fn op =
    let instructions = readInput fn
    let mutable mask = Array.create 36 Bit.Keep
    let mutable memory: Map<int64, Bit []> = Map.empty

    for line in instructions do
        match maskRe.IsMatch(line) with
        | true -> mask <- getMask (maskRe.Match(line))
        | false -> memory <- op memory mask (getAddressAndValue line)

    memory
    |> Map.map (fun _ v -> getValue v)
    |> Map.toSeq
    |> Seq.sumBy snd

let rec getCombinations (acc: Bit list) tail: seq<Bit list> =
    seq {
        match tail with
        | [] -> acc
        | ls ->
            let cur = List.head ls
            let trailer = List.tail ls
            match cur with
            | Bit.Set
            | Bit.Unset -> yield! (getCombinations (acc @ [ cur ]) trailer)
            | Bit.Keep ->
                yield! (getCombinations (acc @ [ Bit.Unset ]) trailer)
                yield! (getCombinations (acc @ [ Bit.Set ]) trailer)
    }


let doOp (memory: Map<int64, Bit []>) mask (addr, v) =
    let newValue = applyMask mask v
    memory |> Map.add addr newValue

let doOp2 memory (mask: Bit array) (addr: int64, value) =
    let addr' =
        asBitString addr
        |> Seq.map translate
        |> Array.ofSeq
    let addrs =
        getCombinations [] (List.ofArray (applyMask2 mask addr'))
        |> Seq.map Array.ofList

    // printfn "%A %d" mask (Seq.length addrs)
    let mutable mem = memory
    for addr'' in addrs do
        mem <- mem |> Map.add (getValue addr'') value
        // printMemory mem
    mem

let day14 fn () = runProgram fn doOp
let day14part2 fn () = runProgram fn doOp2
