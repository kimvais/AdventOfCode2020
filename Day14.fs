module AoC2020.Day14

open System
open System.Text.RegularExpressions
open AoC2020.Utils

type Bit =
    | Set = 1
    | Unset = 0
    | Keep = -1

let translate =
    function
    | '0' -> Bit.Unset
    | '1' -> Bit.Set
    | 'X' -> Bit.Keep
    | c -> failwith (sprintf "Invalid char %c" c)

let asBitString (n: int) =
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

let applyMask (mask: array<Bit>) (value: array<Bit>) =
    Array.zip mask value |> Array.map applyBit

let getAddressAndValue line =
    let m = memRe.Match(line)
    let addr = m.Groups.["address"].Value
    let value = m.Groups.["value"].Value
    (int addr, (value |> (int >> asBitString >> Seq.map translate >> Array.ofSeq)))

let getValue bits =
    bits |> Array.rev |> Array.mapi (fun e b -> bigint (int b) * (2I ** e) |> int64) |> Array.sum
    
let day14 fn () =
    let instructions = readInput fn
    let mutable mask = Array.create 36 Bit.Keep
    let mutable memory: Map<int,Bit []> = Map.empty
    let doOp (addr, v) =
        let newValue = applyMask mask v
        memory <- memory |> Map.add addr newValue
    for line in instructions do
        match maskRe.IsMatch(line) with
        | true -> mask <- getMask (maskRe.Match(line))
        | false -> doOp (getAddressAndValue line)
    memory |> Map.map (fun _ v -> getValue v) |> Map.toSeq |> Seq.sumBy snd
