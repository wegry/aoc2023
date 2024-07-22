module Day06

open Day06

open Expecto
open FSharp.Data.UnitSystems.SI

let testInput =
    ("Time:      7  15   30
Distance:  9  40  200")
        .Split
        '\n'

let part_1_test_input = testInput |> Part1.parse

[<Tests>]
let tests =
    testList
        "Day 06"
        [ test "part 1 parse" {

              Expect.equal part_1_test_input.[0].time 7UL<UnitSymbols.s> ""
          }
          test "part 1 beatTheRecord" {
              let result = Part1.beatTheRecord part_1_test_input.[0]
              let expected = [| 2; 3; 4; 5 |] |> Array.map uint64

              Expect.equal result expected ""
          }

          test "part 1 solve" {
              let result = part_1_ part_1_test_input

              Expect.equal result 288 ""
          }

          test "part 2 solve" {
              let result = testInput |> Part2.parse |> part_2_

              Expect.equal result 71503 ""
          } ]
