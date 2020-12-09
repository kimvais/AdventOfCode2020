module AoC2020.Day9

open AoC2020.Utils


let getWindow preample numbers =
    let stream =
        numbers |> Seq.windowed preample |> Seq.cache

    Seq.zip stream stream
    |> Seq.map (fun (a, b) ->
        Seq.allPairs a b
        |> Seq.filter (fun (a, b) -> a <> b)
        |> Seq.map (fun (a, b) -> a + b))
    
let findNumber input preampleSize = 
    let toBeTested = input |> Seq.skip preampleSize 
    let preamples = getWindow preampleSize input
    let stream = Seq.zip toBeTested preamples
    stream
    |> Seq.find (fun (n, preample) -> preample |> Seq.exists ((=) n) |> not)
    |> fst
    |> int64
    
let day9 fn preampleSize () =
    let input = readInput fn |> Seq.map int64 |> Seq.cache
    findNumber input preampleSize
    

let day9part2 fn preampleSize () =
    let input = readInput fn |> Seq.map int64 |> Seq.cache
    let n = findNumber input preampleSize
    let windows = Seq.initInfinite (fun i -> input |> Seq.map int64 |> Seq.windowed (i + 2)) |> Seq.concat
    let found = windows |> Seq.find (Seq.sum >> (=) n) |> Seq.cache
    let min = Seq.min found
    let max = Seq.max found
    min + max
    
