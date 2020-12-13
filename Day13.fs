module AoC2020.Day13

open AoC2020.Utils

let day13 fn () =
    let input = readInput fn
    let dep = int (Seq.head input)
    let busList = Seq.last input

    let buses =
        busList.Split(',')
        |> Array.filter ((<>) "x")
        |> Array.map int

    let getNextDeparture time busId =
        busId, busId - (time % busId)
        
    let nextDepartures =
        buses
        |> Array.map (getNextDeparture dep)
        |> Seq.sortBy snd

    let nextBus, waitTime = Seq.head nextDepartures
    int64 nextBus * int64 waitTime
