module Day09

open Day09

open Expecto

let testInput =
    ("0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45")
        .Split
        '\n'

let part_1_test_input = testInput |> parse

[<Tests>]
let tests =
    testList
        "Day 09"
        [ test "part 1 parse" {
              let result = part_1_ part_1_test_input
              Expect.equal result 114 ""
          }

          test "part 1 one line" {
              let input = (seq { yield "0 3 6 9 12 15" }) |> parse
              let result = part_1_ input
              Expect.equal result 18 ""
          }

          test "part 2 one line" {
              let result = part_2_ part_1_test_input
              Expect.equal result 2 ""
          } ]
