module AoC2020.Day7

open AoC2020.Utils
open System.Text.RegularExpressions

let getContents s =
    let contentsRe =
        Regex("(?<count>\d+) (?<bag>\w+ \w+) bags?[.,]?")

    let matches = contentsRe.Matches(s)
    seq {
        for m in matches do
            let bag = m.Groups.["bag"].Value
            let count = m.Groups.["count"].Value
            (bag, count)
    }

let getContainer s =
    let containerRe = Regex("^(?<bag>\w+ \w+) bags contain")
    containerRe.Match(s).Groups.["bag"].Value

let getRule s =
    let container = getContainer s
    let contents = getContents s
    seq {
        for (bag, c) in contents do
            match (bag, container) with
            | ("no other", _) -> ()
            | (_, "shiny gold") -> ()
            | _ -> yield (bag, (container, c))
    }

let getRules s =
    let mutable rules: Map<string, Set<string * string>> = Map.empty
    for (canBeIn, container) in s do
        let inside =
            match rules.TryFind canBeIn with
            | Some bags -> bags
            | None -> Set.empty

        rules <- Map.add canBeIn (inside.Add container) rules
    rules

let rec findAllBags (rules: Map<string, Set<string * string>>) (bag: string) =
    seq {
        let bags =
            match rules.TryFind(bag) with
            | None -> Set.empty
            | Some b -> b

        yield! bags
        for (bag, c) in bags do
            yield! findAllBags rules bag
    }

let day7 fn () =
    let rawRules =
        readInput fn |> Seq.map getRule |> Seq.concat
    // printfn "Raw rules: %A" rawRules
    let rules = rawRules |> getRules
    // printfn "Rules: %A" rules
    let bags = findAllBags rules "shiny gold"
    // printfn "Bags: %A" bags
    bags |> Seq.distinctBy fst |> Seq.length |> int64


let day7part2 fn () = 0L
