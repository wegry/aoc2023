module Day08

open Day08

open Expecto

let testInput =
    ("RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")
        .Split
        '\n'

let part_1_test_input = testInput |> parse

[<Tests>]
let tests =
    testList
        "Day 08"
        [ test "part 1 parse" {
              let result = part_1_ part_1_test_input
              Expect.equal result 2 ""
          }
          test "part 1 reptition" {
              let input =
                  ("LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)")


              let result = input.Split '\n' |> parse |> part_1_

              Expect.equal result 6 ""
          }

          test "part 1 instructions longer than list terminates" {
              let input =
                  ("LLRLLRLLRL

BBB = (AAA, ZZZ)
AAA = (BBB, BBB)
ZZZ = (AAA, BBB)")


              let result = input.Split '\n' |> parse |> part_1_

              Expect.equal result 6 ""

          }

          test "part 2 navigate" {
              let input =
                  ("LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")

              let result = input.Split '\n' |> parse |> part_2_
              Expect.equal result 6UL ""
          } ]
