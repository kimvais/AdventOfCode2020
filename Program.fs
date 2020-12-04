﻿module AoC2020.Main

open AoC2020.Utils
open AoC2020.Day1
open AoC2020.Day2
open AoC2020.Day3
open AoC2020.Day4
open AoC2020.Day5

[<EntryPoint>]
let main argv =
    let day = argv |> getProblem
    match day with
    | "1" -> day1 ()
    | "1b" -> day1part2 ()
    | "2" -> day2 ()
    | "2b" -> day2part2 ()
    | "3" -> day3 ()
    | "3b" -> day3part2 ()
    | "4" -> day4 ()
    | "4b" -> day4part2 ()
    | "5" -> day5 ()
    | "5b" -> day5part2 ()
    | _ -> 1
