module Day05

open System.IO
open FSharp.Core
open FSharp.Collections.ParallelSeq

module Shared =
    type Category =
        | Fertilizer
        | Humidity
        | Light
        | Location
        | Seed
        | Soil
        | Temperature
        | Water

    type Category with

        static member Of(input: string) =
            match input.Trim() with
            | "seed" -> Seed
            | "soil" -> Soil
            | "fertilizer" -> Fertilizer
            | "water" -> Water
            | "light" -> Light
            | "temperature" -> Temperature
            | "humidity" -> Humidity
            | "location" -> Location
            | x -> failwithf "Invalid input: '%s'" x

    type Range =
        { destination_start: uint64
          source_start: uint64
          length: uint64 }

    type Range with

        static member Parse(input: string) =
            let as_uint64 = input.Trim().Split " " |> Array.map (fun x -> x.Trim() |> uint64)

            let (dest_start, source_start, source_length) =
                match as_uint64 with
                | [| x; y; z |] -> x, y, z
                | _ -> failwith "Invalid input"

            { destination_start = uint64 dest_start
              source_start = uint64 source_start
              length = uint64 source_length }

        member this.Contains(index: uint64) =
            this.source_start <= index && this.source_start + this.length > index

        member this.source_end() = this.source_start + this.length - 1UL

    type Part2Range = { start: uint64; length: uint64 }

    type Part2Range with
        member this.``end``() = this.start + this.length - 1UL

    type Overlap =
        | Outside of Part2Range
        | Inside of Part2Range

    type Mapping =
        { source: Category
          destination: Category
          ranges: Range[] }

    type Mapping with

        static member Parse(raw: string) =
            let input = raw.Trim()

            let (source_and_dest, rest) =
                match input.Split "map:\n" with
                | [| x; y |] -> x, y
                | x -> failwithf "Invalid input '%A'" x

            let (source, dest) =
                match source_and_dest.Split "-to-" with
                | [| x; y |] -> x, y
                | _ -> failwith "Invalid input"

            let ranges = rest.Split "\n" |> Array.map Range.Parse |> Array.sortBy _.source_start

            { source = Category.Of source
              destination = Category.Of dest
              ranges = ranges }

        member this.FindRange(index: uint64) =
            this.ranges
            |> Array.tryFind (fun range -> range.Contains index)
            |> Option.map (fun range -> range.destination_start + index - range.source_start)
            // Default to the input
            |> Option.defaultValue index


    type LookupType = { category: Category; index: uint64 }


    type Almanac =
        { seeds: uint64 seq
          mappings: Map<Category, Mapping> }

    type Almanac with

        static member Parse(input: string seq) =
            let (rawSeeds, rest) =
                match input with
                | x when Seq.length x > 2 -> Seq.head x, Seq.skip 1 x
                | _ -> failwith "Invalid input"

            let seeds =
                rawSeeds
                |> (fun x -> (x.Split ": ").[1].Split ' ')
                |> Array.map uint64
                |> Set.ofArray


            let mappings =
                String.concat "\n" rest
                |> (fun x -> x.Split "\n\n")
                |> Array.map (Mapping.Parse >> (fun x -> x.source, x))
                |> Map.ofArray


            { seeds = seeds; mappings = mappings }

        member this.Lookup(index: uint64) =
            let mutable curr: LookupType = { category = Seed; index = index }

            while curr.category <> Location do
                (let next: LookupType =
                    (this.mappings
                     |> Map.find curr.category
                     |> (fun x ->
                         { LookupType.category = x.destination
                           index = x.FindRange(curr.index) }))

                 curr <- next)

            curr



module Part2 =
    open Shared

    let parse (input: string seq) =
        let raw_seeds = Seq.head input

        let part_1_almanac = input |> Almanac.Parse

        let seeds =
            raw_seeds
            |> (fun x -> (x.Split ": ").[1].Split ' ')
            |> Seq.map uint64
            |> Seq.chunkBySize 2
            |> Seq.collect (function
                | [| x; y |] -> { x .. (x + y) }
                | x -> failwithf "Invalid input: %A" x)
            |> Seq.toArray

        let part_2_ranges = { part_1_almanac with seeds = seeds }

        part_2_ranges


let parse (input: string seq) = input |> Shared.Almanac.Parse

let unparsed = File.ReadLines("puzzle_input/day_05")

let input = unparsed |> parse

let part_1_ (input: Shared.Almanac) =
    let result =
        input.seeds |> Seq.map (fun x -> input.Lookup x) |> Seq.minBy (fun x -> x.index)


    result.index


let part_2_ (raw: string seq) =
    let input = Part2.parse raw

    let result =
        input.seeds |> PSeq.distinct |> Seq.map input.Lookup |> Seq.minBy _.index

    result.index


let part_1 () = input |> part_1_ |> printfn "%A"

let part_2 () = unparsed |> part_2_ |> printfn "%A"

let parts = (5, part_1, part_2)
