type Day = int * (unit -> unit) * (unit -> unit)

let Days: Day[] = [| Day03.parts; Day02.parts; Day01.parts |]

// https://stackoverflow.com/a/4646066/1924257
let duration f =
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let returnValue = f ()
    printfn "Elapsed Time: %A" timer.Elapsed
    returnValue

[<EntryPoint>]
let main args =
    // For more information see https://aka.ms/fsharp-console-apps
    Days
    // Allow filtering by day
    |> if Array.isEmpty args then
           id
       else
           Array.filter (fun (day, _, _) -> args.[0] = day.ToString())
    |> Array.iter (fun (day, part_1, part_2) ->
        printfn "\nDay %i" day
        printfn "Part 1"
        duration part_1
        printfn "\nPart 2"
        duration part_2)

    0
