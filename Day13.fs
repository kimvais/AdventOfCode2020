module AoC2020.Day13

open AoC2020.Utils
open AoC2020.Math


let getNextDeparture time busId =
    match busId with
    | b when b > 0L -> b, b - (time % b)
    | _ -> -1L, -1L

let getWaitTime time busId =
    match busId with
    | -1L -> -1L
    | b ->
        match (time % b) with
        | 0L -> 0L
        | n when n > 0L -> b - n

let day13 fn () =
    let input = readInput fn
    let dep = int64 (Seq.head input)
    let busList = Seq.last input

    let buses =
        busList.Split(',')
        |> Array.filter ((<>) "x")
        |> Array.map int64

    let nextDepartures =
        Array.map (getNextDeparture dep) >> Seq.sortBy snd

    let nextBus, waitTime = Seq.head (nextDepartures buses)
    int64 nextBus * int64 waitTime
  
// This takes ridiculously long time to solve the actual part 2, but handles all the test cases...
let solvePart2BruteForce (busList: string) startAt =

    let buses =
        busList.Split(',')
        |> Array.indexed
        |> Array.filter (fun (_, c) -> c <> "x")
        |> Array.map (fun (i, n) -> int64 i, int64 n)
        
    let validateWaitTime (i, b) time =
        (match (time % b) with
         | 0L -> 0L
         | n when n > 0L -> b - n) = i

    let validityFunctions = buses |> Array.map validateWaitTime

    let offset, candidate = buses |> Array.maxBy snd
    let unfolder candidate s =
        let n = s + candidate
        Some (n,n)
    let timesToTry = Seq.unfold (unfolder candidate) (candidate - offset)

    timesToTry |> Seq.find (fun time -> validityFunctions |> Seq.forall (fun fn -> fn time)) 

let solvePart2 (busList:string) =
    let buses =
        busList.Split(',')
        |> Array.indexed
        |> Array.filter (fun (_, c) -> c <> "x")
        |> Array.map (fun (i, n) -> int64 i, int64 n)
        |> Array.sortBy snd 
    let remainders = buses |> Array.map (fun (i, x) -> if i = 0L then 0L else x - i)
    let divisors = buses |> Array.map snd 
    match CD remainders divisors with
    | Some n -> n
    | None -> 0L
    
let day13part2 fn () =
    let input = readInput fn
    let busList = Seq.last input
    solvePart2 busList
