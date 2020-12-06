module AoC2020.Utils

open System.IO
open System.Text.RegularExpressions

let readLines filePath = File.ReadLines(filePath)

let readInput (s: string) =
    readLines (__SOURCE_DIRECTORY__ + (sprintf "/input/%s.txt" s))

let getProblem (a: seq<string>): string = a |> Seq.head

module Seq =
    let repeatForever s =
        let c = Seq.cache s
        seq {
            while true do
                yield! c
        }

    let filteri f s =
        s
        |> Seq.mapi (fun i v -> (i, v))
        |> Seq.filter (fun v -> f (fst v) (snd v))
        |> Seq.map (fun v -> snd v)

let splitByLinefeed (s: string) = s.Split '\n'

let splitByTwoLinefeeds s = Regex.Split(s, "\n\n")

let readInputDelimByEmptyLine inputfile =
    readInput inputfile
    |> String.concat "\n"
    |> splitByTwoLinefeeds
