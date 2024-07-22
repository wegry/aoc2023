module DayXX

open System.IO

let parse (input: string seq) = input |> Array.ofSeq

let input = File.ReadLines("puzzle_input/day_XX") |> parse

let part_1_ (input) = "stub"

let part_2_ (input) = "stub"

let part_1 () = input |> part_1_ |> printfn "%A"

let part_2 () = input |> part_2_ |> printfn "%A"

let parts = (System.Int32.MaxValue, part_1, part_2)
