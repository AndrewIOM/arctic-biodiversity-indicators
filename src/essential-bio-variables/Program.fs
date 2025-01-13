open BiodiversityCoder.Core
open RProvider.oxcAAR

let args = System.Environment.GetCommandLineArgs()

match args |> Array.tryItem 1 |> Option.defaultValue "" with
| "harmonise-individual-dates" ->

    printfn "Setting up OxCal..."
    R.quickSetupOxcal() |> ignore

    printfn "Querying graph database for dated timelines..."
    let graph = Storage.loadOrInitGraph "../../data/" |> Result.forceOk

    match DateHarmonisation.queryGraphForDates graph with
    | Error e -> printfn "Failed to read graph: %s." e
    | Ok timelines ->

        let timelines =
            match args |> Array.tryItem 2 with
            | Some i ->
                printfn "A timeline filter was specified. Filtering to timeline %s" i
                timelines |> List.filter(fun (t,_,_,_) -> t.AsString.Contains i)
            | None -> timelines

        printfn "Query identified %i timelines with individual dates." timelines.Length

        let updatedGraph =
            timelines
            |> List.fold(fun g (key,context,latestExtent,dates) ->
                match dates |> List.choose(fun (_,_,used) -> used) |> List.isEmpty with
                | true ->
                    printfn "Processing %s." key.AsString
                    DateHarmonisation.processTimeline (dates |> List.map(fun (a,b,c) -> (a,b))) context latestExtent g
                | false ->
                    printfn "Skipping %s as dates have already been calibrated." key.AsString
                    graph
                ) graph
        
        printfn "Harmonisation of individual dates complete."

| "harmonise-temporal-extents" ->

    printfn "Setting up OxCal..."
    R.quickSetupOxcal() |> ignore

    printfn "Querying graph database for dated timelines..."
    let graph = Storage.loadOrInitGraph "/Users/andrewmartin/Documents/GitHub Projects/holocene-arctic-biodiversity-map/data" |> Result.forceOk

    match DateHarmonisation.readAllTimelineAtoms graph with
    | Error _ ->
        printfn "Could not read all timelines for stage 2 analysis."
    | Ok timelines ->
        timelines 
        |> List.fold(fun g timelineAtom -> DateHarmonisation.calibrateExtent timelineAtom g) graph
        |> ignore


| "calculate-biodiversity-variables" ->

    printfn "Calculating biodiversity variables and outputting a seperate tsv file."

    printfn "Loading graph database..."
    let graph = Storage.loadOrInitGraph "/Users/andrewmartin/Documents/GitHub Projects/holocene-arctic-biodiversity-map/data" |> Result.forceOk
    
    let x = BiodiversityVariables.calculateBiodiversityVariables graph |> Result.forceOk

    ()

| _ ->
    printfn "Enter a valid command line argument: [harmonise-individual-dates / harmonise-temporal-extents / calculate-biodiversity-variables]."
    ()
