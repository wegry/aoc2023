module Day03

open System.IO
open FSharp.Core

module Part1 =
    let parse (input: string seq) =
        let as_arr = input |> Seq.map (fun x -> x.ToCharArray()) |> Array.ofSeq

        let grid =
            Array2D.init (Array.length as_arr) (as_arr.[0].Length) (fun i j -> as_arr.[i].[j])

        grid


    type SchematicNumber =
        { start: int
          ending: int
          row: int
          value: string }

    type SchematicNumber with

        member this.PositionsArround() =
            seq {
                for row in [ this.row - 1 .. this.row + 1 ] do
                    for col in [ this.start - 1 .. this.ending + 1 ] do
                        if row = this.row && (col >= this.start && col <= this.ending) then
                            ()
                        else
                            yield (row, col)
            }


        member this.HasSurroundingSymbol(symbols: Set<int * int>) =
            this.PositionsArround()
            |> Seq.exists (fun (row, column) -> symbols |> Set.contains (row, column))

    let numbers_and_symbols (grid: char[,]) =
        let mutable all_numbers = Set.empty
        let mutable symbols_positions = Set.empty
        let mutable in_number: SchematicNumber option = None
        let mutable prev_i = None

        let maybe_add_number () =
            match in_number with
            | Some num ->
                in_number <- None
                all_numbers <- Set.add num all_numbers
            | None -> ()

        grid
        |> Array2D.iteri (fun i j curr ->
            match prev_i with
            | Some prev when prev <> i ->
                prev_i <- Some i
                maybe_add_number ()
            | Some _ -> ()
            | None -> prev_i <- Some i

            match curr with
            | '.' -> maybe_add_number ()
            | maybeNum when maybeNum >= '0' && maybeNum <= '9' ->
                match in_number with
                | Some prev ->

                    in_number <-
                        Some(
                            { prev with
                                ending = j
                                value = prev.value + string curr }
                        )
                | None ->
                    in_number <-
                        Some(
                            { start = j
                              ending = j
                              row = i
                              value = string curr }
                        )
            | _ ->
                maybe_add_number ()
                symbols_positions <- symbols_positions |> Set.add (i, j)
                ())

        maybe_add_number ()

        {| numbers = all_numbers
           symbols = symbols_positions |}

    let find_numbers_next_to_symbols (grid: char[,]) =
        let processed = numbers_and_symbols grid
        let all_numbers = processed.numbers
        let symbols_positions = processed.symbols

        let schematic_numbers_by_symbols =
            all_numbers |> Seq.filter (fun x -> (x.HasSurroundingSymbol symbols_positions))

        schematic_numbers_by_symbols
        |> Seq.map (fun x -> x.value |> uint)
        |> Seq.reduce (+)

module Part2 =
    let numbers_and_symbols (grid: char[,]) =
        let mutable all_numbers = Set.empty
        let mutable symbols_positions = Set.empty
        let mutable in_number: Part1.SchematicNumber option = None
        let mutable prev_i = None

        let maybe_add_number () =
            match in_number with
            | Some num ->
                in_number <- None
                all_numbers <- Set.add num all_numbers
            | None -> ()

        grid
        |> Array2D.iteri (fun i j curr ->
            match prev_i with
            | Some prev when prev <> i ->
                prev_i <- Some i
                maybe_add_number ()
            | Some _ -> ()
            | None -> prev_i <- Some i

            match curr with
            | maybeNum when maybeNum >= '0' && maybeNum <= '9' ->
                match in_number with
                | Some prev ->

                    in_number <-
                        Some(
                            { prev with
                                ending = j
                                value = prev.value + string curr }
                        )
                | None ->
                    in_number <-
                        Some(
                            { start = j
                              ending = j
                              row = i
                              value = string curr }
                        )
            | '*' ->
                maybe_add_number ()
                symbols_positions <- symbols_positions |> Set.add (i, j)
            | _ -> maybe_add_number ())

        maybe_add_number ()

        {| numbers = all_numbers
           symbols = symbols_positions |}

    let find_gears (grid: char[,]) : uint64 =
        let processed = numbers_and_symbols grid
        let all_numbers = processed.numbers
        let symbols_positions = processed.symbols

        let numbers_by_row = all_numbers |> Seq.groupBy (fun x -> x.row) |> Map.ofSeq

        symbols_positions
        |> Seq.fold
            (fun acc (gearCandidateRow, gearCandidateCol) ->
                let adjacent_indicies =
                    seq {
                        for row in [ gearCandidateRow - 1 .. gearCandidateRow + 1 ] do
                            for col in [ gearCandidateCol - 1 .. gearCandidateCol + 1 ] do
                                yield (row, col)
                    }

                let adjacent_numbers =
                    adjacent_indicies
                    |> Seq.choose (fun (row, column) ->
                        numbers_by_row
                        |> Map.tryFind row
                        |> Option.map (fun x ->
                            x |> Seq.filter (fun x -> [ x.start .. x.ending ] |> Seq.contains column)))
                    |> Seq.collect id
                    |> Set.ofSeq

                let gear_sum =
                    if (adjacent_numbers |> Set.count) = 2 then
                        adjacent_numbers
                        |> Set.map (fun x -> x.value |> uint64)
                        |> Seq.reduce (fun x y -> x * y)
                    else
                        0UL

                acc + gear_sum)

            0UL

let parse (input: string seq) = input

let input = File.ReadLines("puzzle_input/day_03") |> parse

let part_1_ (input) =
    input |> Part1.parse |> Part1.find_numbers_next_to_symbols

let part_2_ (input) =
    input |> Part1.parse |> Part2.find_gears

let part_1 () = input |> part_1_ |> printfn "%A"

// 78483111 too low
let part_2 () = input |> part_2_ |> printfn "%A"

let parts = (3, part_1, part_2)

module Tests =
    let testInput =
        ("
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")
            .Trim()
            .Split
            '\n'
        |> parse

    let parse_part2 (input: string) = input.Trim().Split() |> parse

    let ``PositionsAround`` () =
        let number: Part1.SchematicNumber =
            { start = 0
              ending = 2
              row = 2
              value = "123" }

        let result = number.PositionsArround()

        assert (result |> Seq.length = 12)


    let ``HasSurroundingSymbol should be true`` () =
        let symbols = Set.ofList [ (1, 0) ]

        let number: Part1.SchematicNumber =
            { start = 0
              ending = 2
              row = 2
              value = "123" }

        let result = number.HasSurroundingSymbol symbols

        assert (result = true)

    let ``HasSurroundingSymbol where symbol mistakenly in number position`` () =
        let symbols = Set.ofList [ (2, 0) ]

        let number: Part1.SchematicNumber =
            { start = 0
              ending = 2
              row = 2
              value = "123" }

        let result = number.HasSurroundingSymbol symbols

        assert (result = false)

    let ``HasSurroundingSymbol should be false`` () =
        let symbols = Set.ofList [ (10, 0) ]

        let number: Part1.SchematicNumber =
            { start = 0
              ending = 2
              row = 2
              value = "123" }

        let result = number.HasSurroundingSymbol symbols

        assert (result = false)

    let ``part 1 sample data`` () =
        testInput
        |> part_1_
        |> (fun result ->
#if DEBUG
            printfn "Part 1 sample data result: %A" result
#endif
            assert (result = 4361u))

    let ``part 2 sample data`` () =
        testInput
        |> part_2_
        |> (fun result ->
#if DEBUG
            printfn "Part 2 sample data result: %A" result
#endif
            assert (result = 467835UL))

    let part_2_configurations =
        """
3*4

3.
.*
4.

*3
4.

3*
4."""
            .Trim()
            .Split("\n\n")

    let ``part 2 configurations`` () =

        (part_2_configurations
         |> Seq.map (
             (fun x ->
                 printfn
                     "Test configuration for
%A"
                     x

                 x)
             >> parse_part2
         )
         |> Seq.map part_2_
         |> Seq.iter (fun x ->
             printfn "%A" x
             assert (x = 12UL)))

#if DEBUG
Tests.PositionsAround()
Tests.``HasSurroundingSymbol should be true`` ()
Tests.``HasSurroundingSymbol should be false`` ()
Tests.``HasSurroundingSymbol where symbol mistakenly in number position`` ()
Tests.``part 1 sample data`` ()
Tests.``part 2 sample data`` ()
Tests.``part 2 configurations`` ()
#endif
