module Day02

open System.IO
open System.Text.RegularExpressions

module Part1 =
    type Selection = { count: uint; color: string }

    type Reveal(cubes: Selection[]) =
        member this.Cubes = cubes

        static member Parse(input: string) : Reveal =
            let cubes: Selection array =
                input.Split ','
                |> Array.map (fun x -> x.Trim())
                |> Array.map (fun selection ->
                    match selection.Split ' ' with
                    | [| count; color |] -> { count = count |> uint; color = color }
                    | x -> failwithf "Invalid reveal input: %A" x)

            Reveal(cubes)

    type Limit = Map<string, uint>

    let limit: Limit = [ ("red", 12u); ("green", 13u); ("blue", 14u) ] |> Map.ofList

    type Game(id: uint, reveals: Reveal[]) =
        member this.Reveals = reveals
        member this.ID = id

        member this.OverLimit(limit: Limit) : Reveal[] =
            this.Reveals
            |> Array.filter (fun reveal ->
                reveal.Cubes
                |> Array.exists (fun selection -> selection.count > limit.[selection.color]))



        static member Parse(input: string) : Game =
            let id, raw_reveals =
                match input.Split ':' with
                | [| id; raw_reveals |] -> (id, raw_reveals)
                | x -> failwithf "Invalid game input: %A" x

            let reveals =
                raw_reveals.Split ';' |> Array.map ((fun x -> x.Trim()) >> Reveal.Parse)


            Game(Regex.Replace(id, "Game ", "") |> uint, reveals)

    let compute (input: seq<Game>) =
        input
        |> Seq.filter (fun x ->
            match x.OverLimit(limit) with
            | [||] -> true
            | over ->
#if DEBUG
                printfn "Game %A is over limit: %A" x.ID over
#endif
                false)
        |> Seq.map (fun x -> x.ID)
        |> Seq.reduce (+)

module Part2 =
    let minimum_needed (input: Part1.Reveal[]) =
        input
        |> Array.fold
            (fun state reveal ->
                let mutable map = state

                for selection in reveal.Cubes do
                    if (map |> Map.find selection.color < selection.count) then
                        map <- Map.change selection.color (fun x -> Some selection.count) map

                map)
            ([ "red"; "green"; "blue" ] |> List.map (fun x -> (x, 0u)) |> Map.ofList)

    let compute (input: seq<Part1.Game>) =
        input
        |> Seq.map (fun x ->
            let min = minimum_needed x.Reveals
#if DEBUG
            printfn "Game %A minimum needed: %A" x.ID min
#endif
            min |> Map.values |> Seq.reduce (fun x y -> x * y)

        )
        |> Seq.reduce (+)

let parse (input) = input |> Seq.map Part1.Game.Parse

let input = File.ReadLines("puzzle_input/day_02") |> parse

let part_1_ (input) = input |> Part1.compute

let part_2_ (input) = input |> Part2.compute

let part_1 () = input |> part_1_ |> printfn "%A"

let part_2 () = input |> part_2_ |> printfn "%A"

let parts = (System.Int32.MaxValue, part_1, part_2)

module Tests =
    let testInput =
        ("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")
            .Split
            '\n'
        |> parse

    let ``part 1 sample data`` () =
        testInput
        |> part_1_
        |> (fun result ->
#if DEBUG
            printfn "Part 1 sample data result: %A" result
#endif
            assert (result = 8u))

    let ``part 2 sample data`` () =
        testInput
        |> part_2_
        |> (fun result ->
#if DEBUG
            printfn "Part 2 sample data result: %A" result
#endif
            assert (result = 2286u))

Tests.``part 1 sample data`` ()
Tests.``part 2 sample data`` ()
