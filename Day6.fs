module AoC2020.Day6

open System
open AoC2020.Utils

let isChar (c: char) = Char.IsLower(c)

let countAnswersInGroups =
    Seq.map (Seq.filter isChar >> Seq.distinct >> Seq.length)

let countCommonAnswersInGroup: (seq<string> -> int) =
    Seq.map Set.ofSeq
    >> Seq.reduce Set.intersect
    >> Set.count

let countAnswers = countAnswersInGroups >> Seq.sum >> int64

let day6 fn () =
    readInputDelimByEmptyLine fn |> countAnswers

let day6part2 fn () =
    readInputDelimByEmptyLine fn
    |> Seq.map splitByLinefeed
    |> Seq.map countCommonAnswersInGroup
    |> Seq.sum
    |> int64
