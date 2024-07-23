module Day07

open System.IO

module Part1 =
    type Card =
        | A
        | K
        | Q
        | J
        | T
        | N9
        | N8
        | N7
        | N6
        | N5
        | N4
        | N3
        | N2

    type Card with
        static member parse =
            function
            | 'A' -> A
            | 'K' -> K
            | 'Q' -> Q
            | 'J' -> J
            | 'T' -> T
            | '9' -> N9
            | '8' -> N8
            | '7' -> N7
            | '6' -> N6
            | '5' -> N5
            | '4' -> N4
            | '3' -> N3
            | '2' -> N2
            | x -> failwithf "Invalid %A" x

    type Type =
        | FiveOfAKind
        | FourOfAKind
        | FullHouse
        | ThreeOfAKind
        | TwoPair
        | OnePair
        | HighCard


    type Hand =
        { ``type``: Type
          cards: Card array
          bid: uint }

    type Type with

        static member classify(cards: Card array) =
            let counts =
                cards
                |> Array.countBy id
                |> Array.groupBy (fun (k, c) -> c)
                |> Array.map (fun (c, grouping) -> c, grouping |> Array.map fst)
                |> Array.sortByDescending fst
                |> Map.ofArray

            counts
            |> Map.tryPick (fun count chars ->
                count
                |> function
                    | 4 -> Some(FourOfAKind)
                    | 5 -> Some(FiveOfAKind)
                    | 3
                    | 2 ->
                        (if counts.ContainsKey 2 && counts.ContainsKey 3 then
                             FullHouse
                         else
                             match count with
                             | 3 -> ThreeOfAKind
                             | 2 -> if chars.Length = 1 then OnePair else TwoPair
                             | _ -> failwith "not possible")
                        |> Some
                    | _ -> None)
            |> Option.defaultValue HighCard



    let parse (input: string seq) =
        input
        |> Seq.map (fun x ->
            let (cards, bid) =
                match x.Split(' ') with
                | [| cards; bid |] -> (cards.ToCharArray() |> Array.map Card.parse, bid |> uint)
                | x -> failwithf "Invalid input %A" x

            { cards = cards
              bid = bid
              ``type`` = Type.classify cards })
        |> Seq.toArray

module Part2 =
    type Card =
        | A
        | K
        | Q
        | T
        | N9
        | N8
        | N7
        | N6
        | N5
        | N4
        | N3
        | N2
        | J

    type Card with
        static member parse =
            function
            | 'A' -> A
            | 'K' -> K
            | 'Q' -> Q
            | 'J' -> J
            | 'T' -> T
            | '9' -> N9
            | '8' -> N8
            | '7' -> N7
            | '6' -> N6
            | '5' -> N5
            | '4' -> N4
            | '3' -> N3
            | '2' -> N2
            | x -> failwithf "Invalid %A" x

    type Type =
        | FiveOfAKind
        | FourOfAKind
        | FullHouse
        | ThreeOfAKind
        | TwoPair
        | OnePair
        | HighCard


    type Hand =
        { ``type``: Type
          cards: Card array
          bid: uint }

    type Type with

        static member classify(cards: Card array) =
            let (allJ, rest) = cards |> Array.partition ((=) J)
            let jCount = allJ.Length

            let counts =
                rest
                |> Array.countBy id
                |> Array.groupBy (fun (k, c) -> c)
                |> Array.map (fun (c, grouping) -> c, grouping |> Array.map fst)
                |> Array.sortByDescending fst

            let countMap = counts |> Map.ofArray


            counts
            |> Array.tryPick (fun (count, chars) ->

                count
                |> function
                    | x when x + jCount >= 5 -> Some(FiveOfAKind)
                    | x when x + jCount = 4 -> Some(FourOfAKind)
                    | 3 when countMap.ContainsKey 2 -> Some FullHouse
                    | 2 when countMap.[2].Length = 2 && jCount = 1 -> Some FullHouse
                    | x when x + jCount = 3 -> Some(ThreeOfAKind)
                    | 2 ->
                        (match chars.Length with
                         | 1 when jCount = 0 -> OnePair
                         | 1 when jCount = 1 -> TwoPair
                         | 2 when jCount = 0 -> TwoPair
                         | x -> failwithf "Invalid %A %A" x jCount)
                        |> Some
                    | 1 when jCount = 1 -> Some(OnePair)

                    | _ -> None)
            |> Option.defaultWith (fun () ->
                match jCount with
                | 5 -> FiveOfAKind
                | _ -> HighCard)



    let parse (input: string seq) =
        input
        |> Seq.map (fun x ->
            let (cards, bid) =
                match x.Split(' ') with
                | [| cards; bid |] -> (cards.ToCharArray() |> Array.map Card.parse, bid |> uint)
                | x -> failwithf "Invalid input %A" x

            { cards = cards
              bid = bid
              ``type`` = Type.classify cards })
        |> Seq.toArray

let input = File.ReadLines("puzzle_input/day_07")

let part_1_ (input: Part1.Hand array) =
    input
    |> Array.sortDescending
    |> Array.mapi (fun i x -> i, x)
    |> Array.sumBy (fun (i, x) -> uint64 (i + 1) * uint64 x.bid)

let part_2_ (input: Part2.Hand array) =
    input
    |> Array.sortDescending
    |> Array.mapi (fun i x -> i, x)
    |> Array.sumBy (fun (i, x) -> uint64 (i + 1) * uint64 x.bid)

let part_1 () =
    input |> Part1.parse |> part_1_ |> printfn "%A"

let part_2 () =
    input |> Part2.parse |> part_2_ |> printfn "%A"

let parts = (7, part_1, part_2)
