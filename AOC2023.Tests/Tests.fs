open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type Day01 () =
    let testInput =
        ("1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")
            .Split
            '\n'
        |> Array.map Day01.parse

    let testInputPart2 =
        ("two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")
            .Split
            '\n'
        |> Array.map Day01.parse_part2


    [<TestMethod>]
    member _.``Calibration works`` () =
        let result = testInput |> Array.reduce (+)

        Assert.AreEqual(result, 142u)

    [<TestMethod>]
    member _.``Inlining numbers works with calibration`` () =
        let result = testInputPart2 |> Array.reduce (+)

        Assert.AreEqual(result, 281u)

[<TestClass>]
type Day02 () =
    let testInput =
        ("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")
            .Split
            '\n'
        |> Day02.parse

    [<TestMethod>]
    member _.``part 1 sample data`` () =
        testInput
        |> Day02.part_1_
        |> (fun result ->
#if DEBUG
            printfn "Part 1 sample data result: %A" result
#endif
            Assert.AreEqual(result , 8u))

    [<TestMethod>]
    member _.``part 2 sample data`` () =
        testInput
        |> Day02.part_2_
        |> (fun result ->
#if DEBUG
            printfn "Part 2 sample data result: %A" result
#endif
            Assert.AreEqual(result, 2286u))

[<TestClass>]
type Day03 () =
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
            .ReplaceLineEndings("\n")
            .Split
            '\n'
        |> Day03.parse

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
            .ReplaceLineEndings("\n")
            .Split("\n\n")


    let parse_part2 (input: string) = input.Trim().Split() |> Day03.parse

    [<TestMethod>]
    member _.``PositionsAround`` () =
        let number: Day03.Part1.SchematicNumber =
            { start = 0
              ending = 2
              row = 2
              value = "123" }

        let result = number.PositionsArround()

        Assert.AreEqual (result |> Seq.length , 12)


    [<TestMethod>]
    member _.``HasSurroundingSymbol should be true`` () =
        let symbols = Set.ofList [ (1, 0) ]

        let number: Day03.Part1.SchematicNumber =
            { start = 0
              ending = 2
              row = 2
              value = "123" }

        let result = number.HasSurroundingSymbol symbols

        Assert.IsTrue result

    [<TestMethod>]
    member _.``HasSurroundingSymbol where symbol mistakenly in number position`` () =
        let symbols = Set.ofList [ (2, 0) ]

        let number: Day03.Part1.SchematicNumber =
            { start = 0
              ending = 2
              row = 2
              value = "123" }

        let result = number.HasSurroundingSymbol symbols

        Assert.IsFalse (result )

    [<TestMethod>]
     member _.``HasSurroundingSymbol should be false`` () =
        let symbols = Set.ofList [ (10, 0) ]

        let number: Day03.Part1.SchematicNumber =
            { start = 0
              ending = 2
              row = 2
              value = "123" }

        let result = number.HasSurroundingSymbol symbols

        Assert.IsFalse result

    [<TestMethod>]
    member _.``part 1 sample data`` () =
        testInput
        |> Day03.part_1_
        |> (fun result ->
#if DEBUG
            printfn "Part 1 sample data result: %A" result
#endif
            Assert.AreEqual (result , 4361u))

    [<TestMethod>]
    member _.``part 2 sample data`` () =
        testInput
        |> Day03.part_2_
        |> (fun result ->
#if DEBUG
            printfn "Part 2 sample data result: %A" result
#endif
            Assert.AreEqual (result , 467835UL))

    [<TestMethod>]
    member _.``part 2 configurations`` () =

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
         |> Seq.map Day03.part_2_
         |> Seq.iter (fun x ->
             printfn "%A" x
             assert (x = 12UL)))


[<TestClass>]
type Day04 () =
    let testInput =
        ("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")
            .Split
            '\n'
        |> Day04.parse

    [<TestMethod>]
    member _.``part 1 sample data`` () =
        testInput
        |> Day04.part_1_
        |> (fun result ->
#if DEBUG
            printfn "Part 1 sample data result: %A" result
#endif
            Assert.AreEqual(result , 13u))

    [<TestMethod>]
    member _.``part 2 sample data`` () =
        testInput
        |> Day04.part_2_
        |> (fun result ->
#if DEBUG
            printfn "Part 2 sample data result: %A" result
#endif
            Assert.AreEqual (result , 30))


[<TestClass>]
type Day05 () =
    let testInput =
        ("seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")
            .ReplaceLineEndings( "\n")
            .Split '\n'

    let part_1_test_input = testInput |> Day05.parse

    [<TestMethod>]
    member _.``part 1 sample data`` () =
        part_1_test_input
        |> Day05.part_1_
        |> (fun result ->
#if DEBUG
            printfn "Part 1 sample data result: %A" result
#endif
            Assert.AreEqual (result , 35UL))

    [<TestMethod>]
    member _.``part 2 sample data`` () =
        printfn "Part 2"
        testInput
        |> Day05.part_2_
        |> (fun result ->
#if DEBUG
            printfn "Part 2 sample data result: %A" result
#endif
            Assert.AreEqual (result , 46UL))


