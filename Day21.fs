module AoC2020.Day21

open System.Text.RegularExpressions
open AoC2020.Utils

type allergen =
    { Name: string
      Candidates: string Set }

let parseLine s =
    let m = Regex("(.+) \(contains (.+)\)").Match(s)
    let ingredients = Set.ofArray (m.Groups.[1].Value.Split(' '))
    let allergens = Set.ofArray (m.Groups.[2].Value.Split(", "))
    ingredients, allergens

let rec eliminate known unknown = ()


let day21 fn () =
    let data = readInput fn |> Seq.map parseLine
    let ingredients = Set.unionMany (Seq.map fst data)
    let allergens = Set.unionMany (Seq.map snd data)
    // printfn "Allergens: %A" allergens
    // printfn "Ingredients: %A" ingredients

    let possibleAllergens =
        allergens
        |> Seq.map (fun name ->
            { Name = name
              Candidates =
                  Set.intersectMany
                      (data
                       |> Seq.filter (fun (is', as') -> Set.contains name as')
                       |> Seq.map fst) }) |> Seq.sortBy (fun f -> Set.count f.Candidates)

    for a in possibleAllergens do
        printfn "%A" a
    let mutable recognized = Set.empty<string>

    let nonAllergens =
        seq {
            for allergen in possibleAllergens do
                printfn "Name: %s, candidates: %A" allergen.Name allergen.Candidates

                let matched =
                    Set.difference allergen.Candidates recognized
                    |> Seq.exactlyOne
                printfn "Matched: %A" matched
                // TODO: rebuild the possibleAllergen list here.
                recognized <- Set.add matched recognized
                yield (allergen.Name, matched)
        }
        |> Seq.map snd
        |> Set.ofSeq
        |> Set.difference ingredients

    data
    |> Seq.map (fun (i, a) -> Set.intersect nonAllergens i |> Set.count)
    |> Seq.sum
    |> int64
