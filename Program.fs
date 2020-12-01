open System.IO
let readLines filePath = File.ReadLines(filePath)

let readInput (s: string) =
    readLines (__SOURCE_DIRECTORY__ + (sprintf "/input/%s.txt" s))
let getProblem (a: seq<string>): string = a |> Seq.head


let day1 () =
    let entries = readInput "1" |> Seq.map int32 |> Seq.cache
    Seq.allPairs entries entries |> Seq.filter (fun (a,b) -> (a <> b) && (a + b = 2020)) |> Seq.head |> (fun (a,b) -> a * b)
    
    
[<EntryPoint>]
let main argv =
    let day = argv |> getProblem
    match day with
    | "1" -> day1 ()
    | _ -> -1