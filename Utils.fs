module AoC2020.Utils

open System.IO

let readLines filePath = File.ReadLines(filePath)

let readInput (s: string) =
    readLines (__SOURCE_DIRECTORY__ + (sprintf "/input/%s.txt" s))

let getProblem (a: seq<string>): string = a |> Seq.head

