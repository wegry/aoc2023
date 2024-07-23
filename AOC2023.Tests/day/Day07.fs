module Day07

open Day07

open Expecto

let testInput =
    ("32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483")
        .Split
        '\n'

let part_1_test_input = testInput |> Part1.parse

[<Tests>]
let tests =
    testList
        "Day 07"
        [ test "part 1 parse" {
              let result = part_1_ part_1_test_input
              Expect.equal result 6440UL ""
          }
          test "part 1 parse full house" {
              let result =
                  (Part1.parse (
                      seq {
                          "TTJJJ 765"
                          "TTQQQ 0"
                          "TT222 1"
                      }
                  ))
                  |> Array.sortDescending
                  |> Array.map _.``type``

              Expect.equal result [| Part1.FullHouse; Part1.FullHouse; Part1.FullHouse |] ""

          }
          test "part 2 parse" {
              let result =
                  (Part2.parse (
                      seq {
                          "TJAKQ 2"
                          "TTKJJ 765"
                          "TTJQQ 0"
                          "TTJJ2 1"
                          "JJJJJ 5"
                      }
                  ))
                  |> Array.sortDescending
                  |> Utils.tee
                  |> Array.map _.``type``

              Expect.equal
                  result
                  [| Part2.OnePair
                     Part2.FullHouse
                     Part2.FourOfAKind
                     Part2.FourOfAKind
                     Part2.FiveOfAKind |]
                  ""

          }
          test "part 2 result" {
              let expected = 5905UL
              let result = testInput |> Part2.parse |> part_2_

              Expect.equal result expected ""
          } ]
