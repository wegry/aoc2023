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

let input = lazy (File.ReadLines("puzzle_input/day_02") |> parse)

let part_1_ (input) = input |> Part1.compute

let part_2_ (input) = input |> Part2.compute

let part_1 () = input.Value |> part_1_ |> printfn "%A"

let part_2 () = input.Value |> part_2_ |> printfn "%A"

let parts = (2, part_1, part_2)
