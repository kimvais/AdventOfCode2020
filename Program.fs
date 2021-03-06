﻿module AoC2020.Main

open AoC2020.Utils
open AoC2020.Day1
open AoC2020.Day2
open AoC2020.Day3
open AoC2020.Day4
open AoC2020.Day5
open AoC2020.Day6
open AoC2020.Day7
open AoC2020.Day8
open AoC2020.Day9
open AoC2020.Day10
open AoC2020.Day11
open AoC2020.Day12
open AoC2020.Day13
open AoC2020.Day14
open AoC2020.Day15
open AoC2020.Day16
open AoC2020.Day17
open AoC2020.Day18
open AoC2020.Day19
open AoC2020.Day20
open AoC2020.Day21
open AoC2020.Day22
open AoC2020.Day24
open AoC2020.Day25

[<EntryPoint>]
let main argv =
    let day = argv |> getProblem
    match day with
    | "1" -> day1 "1" ()
    | "1b" -> day1part2 "1" ()
    | "2" -> day2 "2" ()
    | "2b" -> day2part2 "2" ()
    | "3" -> day3 "3" ()
    | "3b" -> day3part2 "3" ()
    | "4" -> day4 "4" ()
    | "4b" -> day4part2 "4" ()
    | "5" -> day5 "5" ()
    | "5b" -> day5part2 "5" ()
    | "6" -> day6 "6" ()
    | "6b" -> day6part2 "6" ()
    | "7" -> day7 "7" ()
    | "7b" -> day7part2 "7" ()
    | "8" -> day8 "8" ()
    | "8b" -> day8part2 "8" ()
    | "9" -> day9 "9" 25 ()
    | "9b" -> day9part2 "9" 25 ()
    | "10" -> day10 "10" ()
    | "10b" -> day10part2 "10" ()
    | "11" -> day11 "11" "1" ()
    | "11b" -> day11 "11" "2" ()
    | "12" -> day12 "12" ()
    | "12b" -> day12part2 "12" ()
    | "13" -> day13 "13" ()
    | "13b" -> day13part2 "13" ()
    | "14" -> day14 "14" ()
    | "14b" -> day14part2 "14" ()
    | "15" -> day15 "0,13,16,17,1,10,6" 2020 ()
    | "15b" -> day15 "0,13,16,17,1,10,6" 30000000 ()
    | "16" -> day16 "16" ()
    | "16b" -> day16part2 "16" ()
    | "17" -> day17 "17" ()
    | "17b" -> day17part2 "17" ()
    | "18" -> day18 "18" ()
    | "18b" -> day18part2 "18" ()
    | "19" -> day19 "19" ()
    | "20" -> day20 "20" ()
    | "21" -> day21 "21" ()
    | "22" -> day22 "22" ()
    | "24" -> day24 "24" ()
    | "24b" -> day24part2 "24" 100 ()
    | "test" -> day25 5764801I 17807724I ()
    | "25" -> day25 3418282I 8719412I ()
    | _ -> -1L
    |> printfn "%d"
    0
