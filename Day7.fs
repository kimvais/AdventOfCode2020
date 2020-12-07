module AoC2020.Day7

open AoC2020.Utils
open System.Text.RegularExpressions

let ruleRe = Regex("(?<bag>\w+ \w+) bag[s]?")

let getRule s =
    let matches = ruleRe.Matches(s)

    let bags =
        matches
        |> Seq.map (fun m -> m.Groups.["bag"].Value)

    let container = bags |> Seq.head
    let contents = bags |> Seq.tail
    seq {
        for bag in contents do
            match (bag, container) with
            | ("no other", _) -> ()
            | (_, "shiny gold") -> ()
            | _ -> yield (bag, container)
    }

let getRules s =
    let mutable rules: Map<string, string Set> = Map.empty
    for (canBeIn, container) in s do
        let inside =
            match rules.TryFind canBeIn with
            | Some bags -> bags
            | None -> Set.empty

        rules <- Map.add canBeIn (inside.Add container) rules
    rules

let rec findAllBags (rules: Map<string, string Set>) (bag: string) =
    seq {
        let bags =
            match rules.TryFind(bag) with
            | None -> Set.empty
            | Some b -> b

        yield! bags
        for bag in bags do
            yield! findAllBags rules bag
    }

let day7 fn () =
    let rawRules =
        readInput fn |> Seq.map getRule |> Seq.concat

    let rules = rawRules |> getRules

    findAllBags rules "shiny gold" |> Seq.distinct |> Seq.length |> int64
    

let day7part2 fn () = 0L
