module Day09

open System.IO

type Sequence = int list

let parse (input: string seq) =
    input
    |> Seq.map (fun line -> line.Split(' ') |> Seq.filter ((<>) " ") |> Seq.map int |> List.ofSeq)
    |> Array.ofSeq

module Part1 =
    let rec computeDifferences (s: Sequence) : Sequence list =
        if (s |> List.forall ((=) 0)) then
            [ s ]
        else
            (let differences =
                s
                |> List.windowed 2
                |> List.map (function
                    | [ x; y ] -> y - x
                    | y -> failwithf "shouldn't happen %A" y)

             [ s ] @ computeDifferences differences)


let input = File.ReadLines("puzzle_input/day_09") |> parse

let part_1_ (input) =
    input
    |> Array.map (fun sequence ->
        Part1.computeDifferences sequence
        |> List.rev
        |> List.map (fun difference -> difference.[^0])
        |> List.sum)
    |> Array.sum


let part_2_ (input) =
    input
    |> Array.map (fun sequence ->
        Part1.computeDifferences sequence
        |> List.rev
        |> List.map (fun difference -> difference.[0])
        |> List.fold (fun acc curr -> [ curr - acc.[0] ] @ acc) [ 0 ]
        |> List.head)
    |> Array.sum

let part_1 () = input |> part_1_ |> printfn "%A"

let part_2 () = input |> part_2_ |> printfn "%A"

let parts = (9, part_1, part_2)
