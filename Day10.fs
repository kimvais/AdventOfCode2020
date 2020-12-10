module AoC2020.Day10

open AoC2020.Utils

let getRatings fn =
    let input =
        readInput fn |> Seq.map int64 |> Seq.sort

    [ seq { 0L }
      input
      seq { Seq.max input + 3L } ]
    |> Seq.concat

let day10 fn () =
    getRatings fn
    |> Seq.pairwise
    |> Seq.map (fun (a, b) -> b - a)
    |> Seq.groupBy id
    |> Seq.map (snd >> Seq.length >> int64)
    |> Seq.reduce (*)

let findValidNextAdapters (input: int64 seq) joltage =
    let rs = Seq.cache input

    let tail =
        rs |> Seq.skipWhile (fun r -> r <= joltage)

    let valids =
        tail
        |> Seq.takeWhile (fun r -> r <= (joltage + 3L))

    (joltage, valids)

let flipPaths (s: seq<int64 * seq<int64>>) =
    let helper (i, adaps) =
        seq {
            for a in adaps do
                yield (a, i)
        }

    s |> Seq.map helper |> Seq.concat

let day10part2 (fn: string) () =
    let ratings = getRatings fn

    let paths =
        ratings |> Seq.map (findValidNextAdapters ratings)

    let paths' =
        flipPaths paths
        |> Seq.sort
        |> Seq.groupBy fst
        |> Seq.map (fun (a, b) -> (a, Seq.map snd b))


    let mutable pathCounts = Map.add 0L 1L Map.empty

    for path in paths' do
        let i, sources = path
        pathCounts <-
            pathCounts
            |> Map.add
                i
                   (sources
                    |> Seq.map (fun x -> pathCounts.[x])
                    |> Seq.sum)
    pathCounts.[Seq.last ratings]
