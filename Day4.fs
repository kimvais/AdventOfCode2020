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
    printfn "%A" passport
    [ "byr"
      "iyr"
      "eyr"
      "hgt"
      "hcl"
      "ecl"
      "pid" ]
    |> Seq.forall (fun k -> Map.containsKey k passport)

let isBetweenInclusive low high v =
    let re = Regex("(\d+)")
    let m = re.Match(v)
    m.Groups.[1].Value
    |> int
    |> (fun i -> i >= low && i <= high)

let validateByr = isBetweenInclusive 1920 2002
let validateIyr = isBetweenInclusive 2010 2020
let validateEyr = isBetweenInclusive 2020 2030

let validateHgt hgt =
    let re = Regex("(\d+)(in|cm)")
    let m = re.Match(hgt)
    match m.Groups.[2].Value with
    | "in" -> isBetweenInclusive 59 76 m.Groups.[1].Value
    | "cm" -> isBetweenInclusive 150 193 m.Groups.[1].Value
    | _ -> false

let validateHcl hcl =
    let re = Regex("#[0-9a-z]{6}")
    re.IsMatch(hcl)

let validateEcl ecl =
    let re = Regex("(amb|blu|brn|gry|grn|hzl|oth)")
    re.IsMatch(ecl)

let validatePid pid =
    let re = Regex("\d{9}")
    re.IsMatch(pid)

let validate2 (passport: Map<string, string>) =
    (*
    byr (Birth Year) - four digits; at least 1920 and at most 2002.
    iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    hgt (Height) - a number followed by either cm or in:
    If cm, the number must be at least 150 and at most 193.
    If in, the number must be at least 59 and at most 76.
    hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    pid (Passport ID) - a nine-digit number, including leading zeroes.
    cid (Country ID) - ignored, missing or not.
    *)

    let validateCid _ = true

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
            | "cid" -> validateCid v
            | _ -> false

        printfn "%s: %s = %b" fieldName v result
        result

    passport |> Map.forall validateField

let validatePassports validator inputfile =
    let re =
        Regex("(?<key>[a-z]{3}):(?<value>[^ \n]+)[\n ]*")

    let input =
        readInput inputfile |> String.concat "\n"

    input
    |> splitByTwoLinefeeds
    |> Seq.map re.Matches
    |> Seq.map (Seq.map convertToKV >> Map.ofSeq)
    |> Seq.filter validator
    |> Seq.length

let day4 () =
    validatePassports validate "4" |> printfn "%d"
    0

let day4part2 () =
    validatePassports validate2 "4" |> printfn "%d"
    0
