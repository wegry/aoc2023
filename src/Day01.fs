module Day01

open System
open System.IO

let parse (x: string) =
    let just_numbers = x |> Seq.filter Char.IsDigit |> Seq.map string |> Seq.toArray


    let first_and_last = just_numbers.[0] + (just_numbers |> Array.last)

    first_and_last |> uint

let parse_part2 (input: string) =
    let spelled_out_as_digit =
        let lookups =
            [ ("zero", '0')
              ("one", '1')
              ("two", '2')
              ("three", '3')
              ("four", '4')
              ("five", '5')
              ("six", '6')
              ("seven", '7')
              ("eight", '8')
              ("nine", '9') ]
            |> Map.ofList

        input
        |> Seq.mapi (fun i x ->
            if Char.IsDigit(x) then
                Some x
            else
                lookups
                |> Map.tryPick (fun (word: string) (n: char) ->
                    let current_rest = input[i..] |> string

                    if current_rest.StartsWith(word) then Some n else None))

    //  (fun (acc: string) (word, digit) -> acc.Replace(word, digit)) x

    let just_numbers =
        spelled_out_as_digit
        |> Seq.choose id
        |> Seq.filter Char.IsDigit
        |> Seq.map string
        |> Seq.toArray

    let first_and_last = just_numbers.[0] + (just_numbers |> Array.last)

    first_and_last |> uint

let input = File.ReadLines("puzzle_input/day_01/part1") |> Array.ofSeq

let part_1 () =
    input |> Array.map parse |> Array.reduce (+) |> printfn "%A"

let part_2 () =
    input |> Array.map parse_part2 |> Array.reduce (+) |> printfn "%A"

let parts = (1, part_1, part_2)

module Tests =
    let testInput =
        ("1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")
            .Split
            '\n'
        |> Array.map parse

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
        |> Array.map parse_part2

    let ``Calibration works`` () =
        let result = testInput |> Array.reduce (+)

        assert (result = 142u)

    let ``Inlining numbers works with calibration`` () =
        printfn "%A" testInputPart2

        let result = testInputPart2 |> Array.reduce (+)

        assert (result = 281u)

Tests.``Calibration works`` ()
Tests.``Inlining numbers works with calibration`` ()
