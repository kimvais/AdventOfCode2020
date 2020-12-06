module AoC2020.Day6

open System
open AoC2020.Utils

let isChar (c: char) = Char.IsLower(c)

let countAnswersInGroups =
    Seq.map (Seq.filter isChar >> Seq.distinct >> Seq.length)

let countCommonAnswersInGroup: (seq<string> -> int) =
    Seq.map Set.ofSeq
    >> Seq.reduce (fun a b -> Set.intersect a b)
    >> Set.count

let countAnswers = countAnswersInGroups >> Seq.sum

let day6 () =
    readInputDelimByEmptyLine "6"
    |> countAnswers

let day6part2 () =
    readInputDelimByEmptyLine "6"
    |> Seq.map splitByLinefeed
    |> Seq.map countCommonAnswersInGroup
    |> Seq.sum
