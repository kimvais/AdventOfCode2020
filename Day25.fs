module AoC2020.Day25

open System.Numerics
open AoC2020.Utils

let mod' = 20201227I
let n = 7I

let modexp n e = BigInteger.ModPow(n, e, mod')

let loop n = Seq.initInfinite (bigint >> modexp n)

let findLoopSize e =
    loop n |> Seq.findIndex ((=) e) |> bigint

let day25 card door () =
    let cardLoops = findLoopSize card
    let doorLoops = findLoopSize door
    let card' = modexp 7I cardLoops
    modexp card' doorLoops |> int64
