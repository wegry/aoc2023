module Day08

open System.IO
open System.Text.RegularExpressions

let lcm (x: uint64 array) =
    // https://stackoverflow.com/a/196472/1924257
    let _lcm a b =
        let mutable gcd, tmp = a, b

        while tmp <> 0UL do
            let nextGcd = tmp
            let nextTmp = gcd % tmp

            gcd <- nextGcd
            tmp <- nextTmp

        a * b / gcd


    x |> Array.reduce _lcm

module Shared =
    type Node =
        { name: string
          left: string
          right: string }

    and Instruction =
        | Left
        | Right

    type Node with
        member this.Item
            with get (x: Instruction) =
                match x with
                | Left -> this.left
                | Right -> this.right


    type Network =
        { leftRightInstructions: Instruction array
          map: Map<string, Node> }

    type Network with
        member this.Item
            with get (x: string, y: Instruction) = this.map.[this.map.[x].[y]]

        member this.RepeatInstructions() =
            seq {
                while true do
                    yield! this.leftRightInstructions
            }

        member this.Navigate(start: string) =
            let mutable curr = this.map.[start]

            let count =
                this.RepeatInstructions()
                |> Seq.findIndex (fun instruction ->
                    match curr.name with
                    | "ZZZ" -> true
                    | name ->
                        (curr <- this.[name, instruction]
                         false))


            (count, curr)

        member this.NavigateBatch() =
            let start =
                this.map
                |> Map.values
                |> Seq.choose (fun x -> if x.name.[^0] = 'A' then Some x.name else None)
                |> Array.ofSeq

            let mutable ends: Map<string, int option> =
                this.map
                |> Map.values
                |> Seq.choose (fun x -> if x.name.[^0] = 'Z' then Some(x.name, None) else None)
                |> Map.ofSeq


            this.RepeatInstructions()
            |> Seq.scan (fun curr instruction -> curr |> Array.map (fun name -> this.[name, instruction].name)) start
            |> Seq.mapi (fun i x ->
                let zEnds = x |> Array.filter (fun x -> x.[^0] = 'Z')

                zEnds
                |> Array.iter (fun ending ->
                    if ends.[ending].IsNone then
                        // AoC Reddit hint: cheat here and only look for the first iteration of each ending.
                        ends <- ends |> Map.add ending (Some i))

                (i, x))

            |> Seq.takeWhile (fun _ -> ends |> Map.exists (fun _ v -> v.IsNone))
            // Force evaluation of seq https://stackoverflow.com/q/14965700/1924257
            |> Seq.length
            |> ignore

            ends |> Map.values |> Seq.choose id |> Seq.map uint64 |> Array.ofSeq |> lcm


        static member parse(input: string seq) =
            let (instructions, rest) =
                match input |> List.ofSeq with
                | head :: _ :: rest -> (head, rest)
                | _ -> failwithf "invalid input"

            let leftRightInstructions =
                instructions.ToCharArray()
                |> Array.map (function
                    | 'L' -> Left
                    | 'R' -> Right
                    | x -> failwithf "Invalid instruction input: %A" x)

            let map =
                rest
                |> Seq.toArray
                |> Array.map (fun line ->
                    let m = Regex.Match(line, "(?<name>\w{3}) = \((?<left>\w{3}), (?<right>\w{3})\)")
                    let groups = m.Groups
                    let name = groups.["name"].Value
                    let left = groups.["left"].Value
                    let right = groups.["right"].Value

                    { name = name
                      left = left
                      right = right })

                |> Array.map (fun x -> x.name, x)
                |> Map.ofArray

            { leftRightInstructions = leftRightInstructions
              map = map }


let parse (input: string seq) = input |> Shared.Network.parse

let input = File.ReadLines("puzzle_input/day_08")

let part_1_ (input: Shared.Network) =
    let (count, _) = input.Navigate("AAA")

    count

let part_2_ (input: Shared.Network) =
    let count = input.NavigateBatch()

    count

let part_1 () =
    input |> parse |> part_1_ |> printfn "%A"

let part_2 () =
    input |> parse |> part_2_ |> printfn "%A"

let parts = (8, part_1, part_2)
