module AoC2020.Math

//Calculate the Modular Inverse: Nigel Galloway: April 3rd., 2018
let MI n g =
    let rec fN n i g e l a =
        match e with
        | 0L -> g
        | _ ->
            let o = n / e
            fN e l a (n - o * e) (i - o * l) (g - o * a)

    (n + (fN n 1L 0L g 0L 1L)) % n

let rec gcd a b = if b = 0L then abs a else gcd b (a % b)

let CD n g =
    match Seq.fold (fun n g -> if (gcd n g) = 1L then n * g else 0L) 1L g with
    | 0L -> None
    | fN -> Some((Seq.fold2 (fun n i g -> n + i * (fN / g) * (MI g ((fN / g) % g))) 0L n g) % fN)
