module Day04

open System.IO

module Part1 =
    type Card =
        { number: int
          winningNumbers: Set<uint>
          yourNumbers: Set<uint> }

    type Card with

        static member Parse(line: string) =
            let prelude, raw_numbers =
                match (line.Split ':') with
                | [| prelude; raw_numbers |] -> prelude, raw_numbers
                | _ -> failwith "Invalid line input"

            let winning, yours =
                match (raw_numbers.Split '|') with
                | [| winning; yours |] -> winning, yours
                | _ -> failwith "Invalid winning input"

            let split_and_parse (input: string) =
                input.Split ' '
                |> Array.choose (fun x ->
                    match x.Trim() with
                    | "" -> None
                    | x -> Some x)
                |> Array.map uint

            let winningNumbers = split_and_parse winning |> Set.ofArray
            let yourNumbers = split_and_parse yours |> Set.ofArray

            { number = int (prelude.[5..].Trim())
              winningNumbers = winningNumbers
              yourNumbers = yourNumbers }

        member this.WinningNumbers() =
            (Set.intersect this.winningNumbers this.yourNumbers).Count

        member this.Score() =
            this.WinningNumbers() |> (fun x -> pown (uint 2) (x - 1))


let parse (input: string seq) =
    input |> Seq.map Part1.Card.Parse |> Array.ofSeq

let input = File.ReadLines("puzzle_input/day_04") |> parse

let part_1_ (input: Part1.Card[]) =
    input |> Array.map (fun x -> x.Score()) |> Array.reduce (+)

let part_2_ (input: Part1.Card[]) =
    let counts_by_card_number =
        input |> Array.map (fun x -> x.number, x.WinningNumbers()) |> Map

    let mutable copies_by_card_number = Map.empty

    counts_by_card_number
    |> Map.toSeq
    |> Seq.iter (fun (card_number, count) ->
        match count with
        | 0 -> ()
        | n ->
            let copies_of_current_card =
                copies_by_card_number
                |> Map.tryFind card_number
                |> Option.map ((+) 1)
                |> Option.defaultValue 1

            for i in (card_number + 1) .. (card_number + n) do
                copies_by_card_number <-
                    copies_by_card_number
                    |> Map.change i (function
                        | Some x -> Some(x + copies_of_current_card)
                        | None -> Some copies_of_current_card))

    input
    |> Array.map (fun x ->
        let instances_of =
            copies_by_card_number |> Map.tryFind x.number |> Option.defaultValue (0)

        x.number, instances_of + 1)
    |> Array.map (snd)
    |> Array.sum

let part_1 () = input |> part_1_ |> printfn "%A"

let part_2 () = input |> part_2_ |> printfn "%A"

let parts = (4, part_1, part_2)

