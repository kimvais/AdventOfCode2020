module AoC2020.Day20

open AoC2020.Utils
open System.Text.RegularExpressions

(*
  Ideas / questions
  1. Tile's edges can be expressed as 4 10-bit integers
  2. When all edges are grouped, the corner tiles are the ones with 2 edges that don't match any other tile
  3. Does orientation matter if we just read all edges both ways?
     - rotating changes the ordering of edges
     - flipping changes which way edges are read (but you have to read rotated edges the wrong way anyway?)
*)

type Edge = char []

type Tile = { Id: int64; Edges: Edge Set }

let getEdges (lines: string seq) =
    seq {
        for edge in seq {
                        Seq.head lines |> Array.ofSeq
                        lines |> Seq.map (Seq.head) |> Array.ofSeq
                        lines |> Seq.map (Seq.last) |> Array.ofSeq
                        Seq.last lines |> Array.ofSeq
                    } do
            yield edge
            yield edge |> Array.rev
    }
    |> Set.ofSeq

let parseTile tileS =
    let s = (splitByLinefeed tileS)
    let idLine = Seq.head s
    let tileLines = Seq.tail s

    let tileId =
        int64 (Regex("^Tile (?<num>\d+):$").Match(idLine).Groups.["num"].Value)

    let edges = getEdges tileLines
    { Id = tileId; Edges = edges }

let day20 fn () =
    let tiles =
        readInputDelimByEmptyLine fn |> Seq.map parseTile
    let allEdges = tiles |> Seq.map (fun t -> t.Edges) |> Set.unionMany
    let connectingEdges = allEdges |> Seq.filter (fun e -> tiles |> Seq.filter (fun t -> t.Edges |> Set.contains e) |> Seq.length = 2) |> Set.ofSeq
    tiles |> Seq.filter (fun t -> Set.intersect connectingEdges t.Edges |> Seq.length = 4) |> Seq.map (fun t -> t.Id) |> Seq.reduce (*)
