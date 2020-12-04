module AoC2020.Day4

open System.Text.RegularExpressions
open AoC2020.Utils

let convertToKV (a: Match) =
    let k = a.Groups.["key"].Value
    let v = a.Groups.["value"].Value
    (k, v)

let splitByTwoLinefeeds s = Regex.Split(s, "\n\n")

let validate (passport: Map<string, string>) =
    (*
    byr (Birth Year)
    iyr (Issue Year)
    eyr (Expiration Year)
    hgt (Height)
    hcl (Hair Color)
    ecl (Eye Color)
    pid (Passport ID)
    cid (Country ID)
    *)
    [ "byr"
      "iyr"
      "eyr"
      "hgt"
      "hcl"
      "ecl"
      "pid" ]
    |> Seq.forall (fun k -> Map.containsKey k passport)

let isBetweenInclusive low high v =
    let re = Regex("^(\d+)$")
    let m = re.Match(v)
    m.Groups.[1].Value
    |> int
    |> (fun i -> i >= low && i <= high)

// byr (Birth Year) - four digits; at least 1920 and at most 2002.
let validateByr = isBetweenInclusive 1920 2002

// iyr (Issue Year) - four digits; at least 2010 and at most 2020.
let validateIyr = isBetweenInclusive 2010 2020

//eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
let validateEyr = isBetweenInclusive 2020 2030

(* hgt (Height) - a number followed by either cm or in:
    If cm, the number must be at least 150 and at most 193.
    If in, the number must be at least 59 and at most 76.
*)
let validateHgt hgt =
    let re = Regex("^(\d+)(in|cm)$")
    let m = re.Match(hgt)
    match m.Groups.[2].Value with
    | "cm" -> isBetweenInclusive 150 193 m.Groups.[1].Value
    | "in" -> isBetweenInclusive 59 76 m.Groups.[1].Value
    | _ -> false

// hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
let validateHcl hcl =
    let re = Regex("^#[0-9a-f]{6}$")
    re.IsMatch(hcl)

// ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
let validateEcl ecl =
    let re = Regex("^(amb|blu|brn|gry|grn|hzl|oth)$")
    re.IsMatch(ecl)

// pid (Passport ID) - a nine-digit number, including leading zeroes.
let validatePid pid =
    let re = Regex("^\d{9}$")
    re.IsMatch(pid)

let validateField fieldName v =
    let result =
        match fieldName with
        | "byr" -> validateByr v
        | "iyr" -> validateIyr v
        | "eyr" -> validateEyr v
        | "hgt" -> validateHgt v
        | "hcl" -> validateHcl v
        | "ecl" -> validateEcl v
        | "pid" -> validatePid v
        | "cid" -> true
        | _ -> false

    // printfn "%s: %s = %b" fieldName v result
    result

let validate2 (passport: Map<string, string>) =
    match validate passport with
    | false -> false
    | true -> passport |> Map.forall validateField

let getPassports inputfile =

    let input =
        readInput inputfile |> String.concat "\n"

    let passports = input |> splitByTwoLinefeeds
    printfn "Total passports: %d" (Seq.length passports)
    passports

let filterPassports validator ps =
    let re =
        Regex("(?<key>[a-z]{3}):(?<value>[^ \n]+)[\n ]*")

    ps
    |> Seq.map re.Matches
    |> Seq.map (Seq.map convertToKV >> Map.ofSeq)
    |> Seq.filter validator

let countValidPassports fn validator =
    getPassports fn
    |> filterPassports validator
    |> Seq.length

let day4 () =
    countValidPassports "4" validate
    |> printfn "Valid passports: %d"
    0

let day4part2 () =
    countValidPassports "4" validate2
    |> printfn "Valid passports: %d"
    0
