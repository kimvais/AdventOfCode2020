module AoC2020.Day2

open System.Text.RegularExpressions
open AoC2020.Utils

type Password =
    { min: int
      max: int
      c: char
      s: string }

let parsePassword pwd =
    // 2-13 k: wkbwczdmrgkklvxpppfx
    let r =
        Regex("(?<minChars>\d+)-(?<maxChars>\d+) (?<chr>\w): (?<password>\w+)")

    let groups = (r.Match pwd).Groups

    let password =
        { min = int groups.["minChars"].Value
          max = int groups.["maxChars"].Value
          c = char groups.["chr"].Value
          s = groups.["password"].Value }

    password

let checkValidity1 pwd =
    let charCount =
        pwd.s |> Seq.filter ((=) pwd.c) |> Seq.length

    (pwd.min <= charCount) && (pwd.max >= charCount)

let checkValidity2 pwd =
    [ pwd.s.[pwd.min - 1]
      pwd.s.[pwd.max - 1] ]
    |> Seq.filter ((=) pwd.c)
    |> Seq.length = 1

let countValidPasswords fn checkFn =
    readInput fn
    |> Seq.map (parsePassword >> checkFn)
    |> Seq.filter id
    |> Seq.length

let day2 fn () = countValidPasswords fn checkValidity1

let day2part2 fn () = countValidPasswords fn checkValidity2
