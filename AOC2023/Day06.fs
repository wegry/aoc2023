module Day06

open System.IO
open System.Text.RegularExpressions
open FSharp.Data.UnitSystems

module Part1 =
    type Record =
        // Maybe m v. mm is significant
        { distance: uint64<SI.UnitSymbols.m>
          time: uint64<SI.UnitSymbols.s> }

    let beatTheRecord (record: Record) =
        let maxTime = record.time

        seq { 1UL .. (uint64 record.time) }
        |> Seq.filter (fun time ->
            // remaining time and velocity
            let remainingTime = maxTime - (uint64 time * 1UL<SI.UnitSymbols.s>)
            let velocity = time * 1UL<SI.UnitSymbols.m / SI.UnitSymbols.s>

            record.distance < (remainingTime * velocity))
        |> Seq.toArray


    let parse (input: string array) =
        let (time, distance) =
            match
                (input
                 |> Array.map (fun line ->

                     Regex.Replace(line, "(Time|Distance):", "").Split(" ")
                     |> Array.filter (fun x -> x <> "")
                     |> Array.map uint64))
            with
            | [| time; distance |] -> (time, distance)
            | x -> failwithf "Invalid input: %A" x

        let result: Record array =
            Array.zip time distance
            |> Array.map (fun (time, distance) ->
                { time = time * 1UL<SI.UnitSymbols.s>
                  distance = distance * 1UL<SI.UnitSymbols.m> })

        result

module Part2 =
    let parse (input: string array) =
        let (time, distance) =
            match
                (input
                 |> Array.map (fun line ->

                     Regex.Replace(line, "(Time|Distance):", "").Replace(" ", "") |> uint64))
            with
            | [| time; distance |] -> (time, distance)
            | x -> failwithf "Invalid input: %A" x

        let result: Part1.Record =
            { time = time * 1UL<SI.UnitSymbols.s>
              distance = distance * 1UL<SI.UnitSymbols.m> }

        result


let input = File.ReadAllLines("puzzle_input/day_06")

let part_1_ (input) =
    input |> Array.map (Part1.beatTheRecord >> Array.length) |> Array.fold (*) 1

let part_2_ (input) = part_1_ [| input |]


let part_1 () =
    input |> Part1.parse |> part_1_ |> printfn "%A"

let part_2 () =
    input |> Part2.parse |> part_2_ |> printfn "%A"

let parts = (6, part_1, part_2)
