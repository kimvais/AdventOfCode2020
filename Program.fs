module AoC2020.Main

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
    | "test" -> day14part2 "test14b" ()
    | _ -> -1L
    |> printfn "%d"
    0
