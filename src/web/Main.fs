module GraphExplorer.Client.Main

open System
open System.Net.Http
open Microsoft.AspNetCore.Components
open Elmish
open Bolero
open Bolero.Html

// What do we need most urgently? A website for the AHBDB itself (not the map!)

// Structure of data sources - node visualisation + explore.
// - For Greenland, how are datasets connected secondary -> primary?
// - "Used in further analyses by..." (backwards link)

// Dataset statuses
// - Show sources by digitised vs not digitised datasets. Default to digitised only?

// Essential biodiversity variables
// - View individual time-series: focus not on pollen diagrams; they can be in Neotoma eventually.
// - Compare and group time-series at a place (e.g. certain plants vs others based on defined lists 
        // or taxonomic tree? Herbs etc.)

// Spatial analysis:
// - Map plots of individual time-series, individual points (colour coded)
// - Switch to 

// EBVs (specific things)
// - Date of first occurrence maps

// Data export:
// - Allow data from any view to be exported to csv data frame.
// - Include txt file with list of references for use in publications?

module ModelParts =

    type SpatialUnit = {
        Label: string
        GeoJSON: string
    }

    type TimelineWithLocation = {
        TimelineId: string
        Coordinate: string
        LocationName: string
    }

    type DimensionView =
        | Temporal of timelines: string list
        | Spatial of timelines: string list * intersect:SpatialUnit

    type EssentialBiodiversityVariable =
        | TaxonDistribution
        | Morphology
        | Movement
        | TaxonomicAndPhylogeneticDiversity
        | TraitDiversity

    with
        member this.Name =
            match this with
            | TaxonDistribution -> "Taxon distribution"
            | Morphology -> "Morphology"
            | Movement -> "Movement"
            | TaxonomicAndPhylogeneticDiversity -> "Taxonomic and phylogenetic diversity"
            | TraitDiversity -> "Trait diversity"

        member this.Slug =
            match this with
            | TaxonDistribution -> "distribution"
            | Morphology -> "morphology"
            | Movement -> "movement"
            | TaxonomicAndPhylogeneticDiversity -> "taxonomic-diversity"
            | TraitDiversity -> "trait-diversity"


    type EBVCategory = {
        Label: string
        EBVs: EssentialBiodiversityVariable list
    }

type Dataset = string list // TODO

module DataAccess =

    open BiodiversityCoder.Core
    open FSharp.Data

    /// File-based service definition.
    type DataAccess =
        {
            LoadSourceNode: Graph.UniqueKey -> Sources.SourceNode option

            RawDataFor: string -> string list -> (decimal * decimal) seq

            GetProcessedEbvData: ModelParts.EssentialBiodiversityVariable -> ModelParts.DimensionView -> Async<Dataset[]>
        }

    module Graph =

        open Microsoft.FSharpLu.Json
        open System.Threading.Tasks

        let atomUrl atomId = sprintf "https://raw.githubusercontent.com/AndrewIOM/holocene-arctic-biodiversity-map/refs/heads/main/data/atom-%s.json" atomId

        let inline loadAtomFromGitHub< ^T> (httpClient:HttpClient) atomKey : Task<Result< ^T,string>> =
            task {
                let url = atomUrl atomKey
                let! json = httpClient.GetAsync(url)
                if json.IsSuccessStatusCode
                then
                    return
                        json.Content.ReadAsStream Threading.CancellationToken.None
                        |> Compact.deserializeStream
                        |> Ok
                else return Error <| sprintf "Could not read node from github repository: %s (%s)" (json.StatusCode.ToString()) url
            }

    module RawCalibrated =

        type RawCalibratedData = CsvProvider<"../../data-derived/raw-data-calibrated.csv">

        let load (httpClient:HttpClient) =
            task {
                let! csv = httpClient.GetStringAsync "/content/indicators/raw-data-calibrated.csv"
                return RawCalibratedData.Parse csv
            }

        let dataForTimeline morphotypes timeline (data:RawCalibratedData) : (decimal * decimal) seq =
            data.Rows
            |> Seq.filter(fun r -> r.Timeline_id = timeline)
            |> Seq.filter(fun r -> morphotypes |> Seq.contains r.Morphotype)
            |> Seq.map(fun r -> r.Age_ybp, r.``Data-value``)


/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home
    | [<EndPoint "/about">] About
    | [<EndPoint "/ebv/{name}">] EssentialBioVariable of name:string
    | [<EndPoint "/ebv/spatial/{name}">] EssentialBioVariableSpatial of name:string
    | [<EndPoint "/sources/{sourceId}">] Sources of sourceId:string option


/// The Elmish application's model.
type Model =
    {
        page: Page
        error: string option
        data: DataAccess.RawCalibrated.RawCalibratedData option
        dataSlice: Map<ModelParts.TimelineWithLocation, (decimal * decimal) seq>
        filterByTaxa: string list
        dimension: ModelParts.DimensionView
        taxonList: string list
        timelineList: ModelParts.TimelineWithLocation list
    }

let initModel =
    {
        page = Home
        error = None
        data = None
        dataSlice = Map.empty
        filterByTaxa = []
        dimension = ModelParts.Temporal []
        taxonList = []
        timelineList = []
    }


// Browse EBVs by time and by spatial aggregation.

open ModelParts

let ebvIndex = [
    { Label = "Species populations"
      EBVs = [
        TaxonDistribution
      ] }
    { Label = "Species traits"
      EBVs = [
        Morphology
        Movement
      ] }
    { Label = "Community composition"
      EBVs = [
        TaxonomicAndPhylogeneticDiversity
        TraitDiversity
      ] }
]

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | GetEBVData
    | GotEBVData of Dataset[]
    | Error of exn
    | ClearError

    | LoadRawData
    | LoadedRawData of DataAccess.RawCalibrated.RawCalibratedData
    | SliceRawData

    | AddTaxonToFilter of string
    | AddTimelineToFilter of string


let update httpClient message model =
    match message with
    | SetPage page ->
        { model with page = page }, Cmd.none

    | Error exn ->
        printfn "Error! Was %A" exn
        { model with error = Some exn.Message }, Cmd.none
    | ClearError ->
        { model with error = None }, Cmd.none
    
    | LoadRawData ->
        printfn "Loading..."
        model, Cmd.OfTask.either (fun _ -> DataAccess.RawCalibrated.load httpClient) () LoadedRawData Error
    | LoadedRawData raw ->
        printfn "Loaded!"
        let taxa =
            raw.Rows 
            |> Seq.mapi(fun i r ->
                printfn "%i" i
                r.Morphotype)
            |> Seq.distinct
            |> Seq.toList
            |> List.sort
        printfn "Cool"
        let timelines =
            raw.Rows 
            |> Seq.map(fun r -> { TimelineId = r.Timeline_id; LocationName = r.Site_name; Coordinate = r.Coord })
            |> Seq.distinct
            |> Seq.toList
        printfn "Here"
        { model with data = Some raw; taxonList = taxa; timelineList = timelines }, Cmd.none

    | SliceRawData ->
        match model.data with
        | None -> { model with error = Some "Raw datasets not yet loaded"}, Cmd.none
        | Some dataset ->
            match model.dimension with
            | Temporal timelines ->
                let sliced =
                    timelines
                    |> List.map (fun t -> 
                        model.timelineList |> Seq.find (fun ts -> ts.TimelineId = t), 
                        DataAccess.RawCalibrated.dataForTimeline model.filterByTaxa t dataset)
                    |> Map.ofList
                { model with dataSlice = sliced }, Cmd.none
            | Spatial _ -> model, Cmd.none

    | AddTaxonToFilter taxon ->
        { model with filterByTaxa = taxon :: model.filterByTaxa }, Cmd.ofMsg SliceRawData
    | AddTimelineToFilter timeline ->
        match model.dimension with
        | Temporal t -> { model with dimension = Temporal (timeline :: t) }, Cmd.ofMsg SliceRawData
        | Spatial (t,s) -> { model with dimension = Spatial ((timeline :: t),s) }, Cmd.ofMsg SliceRawData

/// Connects the routing system to the Elmish application.
let router = Router.infer SetPage (fun model -> model.page)

module View =

        let menu (model: Model) =
            forEach ebvIndex <|
                function i ->
                            concat {
                                p { i.Label }
                                forEach i.EBVs <| function ebv ->
                                                            li {
                                                                a {
                                                                    attr.``class`` (if model.page = (Page.EssentialBioVariable ebv.Slug) then "is-active" else "")
                                                                    attr.href (router.Link (Page.EssentialBioVariable ebv.Slug ))
                                                                    ebv.Name
                                                                }
                                                            }
                            }

        let main (menuHole: Node) (bodyHole: Node) (errorHole: Node) =
            div {
                attr.``class`` "columns"
                aside {
                    attr.``class`` "column sidebar is-narrow"
                    section {
                        attr.``class`` "section"
                        nav {
                            attr.``class`` "menu"
                            ul {
                                attr.``class`` "menu-list"
                                menuHole
                            }
                        }
                    }
                }
                div {
                    attr.``class`` "column"
                    section {
                        attr.``class`` "section"
                        bodyHole
                        div {
                            attr.id "notification-area"
                            div {
                                errorHole
                            }
                        }
                    }
                }
            }

        let errorNotification (message: Node) hideAction =
            div {
                attr.``class`` "notification is-warning"
                button {
                    attr.``class`` "delete"
                    on.click hideAction
                    message
                }
            }

module Plots =

    open Plotly.NET

    let slicedRawData (model:Map<TimelineWithLocation,seq<decimal * decimal>>) =
        cond model.IsEmpty <| function
        | true -> text "No data to display"
        | false ->
            model
            |> Seq.map(fun kv ->
                Chart.Line(kv.Value |> Seq.map snd, kv.Value |> Seq.map fst, Name = kv.Key.LocationName, ShowMarkers = true, Orientation = StyleParam.Orientation.Vertical)
                |> Chart.withYAxisStyle(MinMax = (12000, 0))
                |> Chart.withXAxisStyle(MinMax = (0, 50))
            )
            |> Chart.Grid(nRows = 1, nCols = (model |> Seq.length), Pattern = StyleParam.LayoutGridPattern.Coupled)
            |> Chart.withLegendStyle(
                Orientation = StyleParam.Orientation.Horizontal,
                X = 0.5,
                XAnchor = StyleParam.XAnchorPosition.Center
            )
            |> GenericChart.toChartHTML
            |> rawHtml

    let traitRadial =
        let radial = [ 1; 2; 3; 2; 5; 11 ]
        let theta = [ "Leaf area"; "Leaf nitrogen"; "Leaf mass"; "Plant height"; "Diaspore mass"; "Stem density" ]
        let pointPolar = Chart.PointPolar(r = radial, theta = theta)
        pointPolar |> GenericChart.toChartHTML |> rawHtml

let selectTaxaMulti model dispatch =
    div {
        ul {
            forEach model.filterByTaxa <| fun t ->
                li {
                    text t
                }
        }
        select {
            bind.change.string "" (fun s -> AddTaxonToFilter s |> dispatch)
            forEach model.taxonList <| fun t ->
                option {
                    attr.name t
                    attr.value t
                    text t
                }
        }
    }

let selectTimelineMulti dimension (timelineList: ModelParts.TimelineWithLocation list) dispatch =
    cond dimension <| function
    | Temporal timelines ->
        div {
            ul {
                forEach timelines <| fun t ->
                    li {
                        text (timelineList |> List.find(fun t2 -> t2.TimelineId = t)).LocationName
                        text (timelineList |> List.find(fun t2 -> t2.TimelineId = t)).Coordinate
                    }
            }
            select {
                bind.change.string "" (fun s -> AddTimelineToFilter s |> dispatch)
                forEach timelineList <| fun t ->
                    option {
                        attr.name t.TimelineId
                        attr.value t.TimelineId
                        text t.LocationName
                    }
            }
        }


let homePage (model: Model) dispatch =
    div {
        h1 { text "Slice raw data by taxon and place" }
        selectTaxaMulti model dispatch
        selectTimelineMulti model.dimension model.timelineList dispatch
        Plots.slicedRawData model.dataSlice

        h1 { text "Plant traits test plot" }
        Plots.traitRadial
    }


let view model dispatch =
    View.main
        (View.menu model)
        (
            cond model.page <| function
            | Home -> homePage model dispatch
        )
        (
            cond model.error <| function
            | None -> empty()
            | Some err ->
                View.errorNotification (text err) (fun _ -> dispatch ClearError)
        )

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    [<Inject>]
    member val HttpClient = Unchecked.defaultof<HttpClient> with get, set

    override this.Program =
        let update = update this.HttpClient
        Program.mkProgram (fun _ -> initModel, Cmd.ofMsg LoadRawData) update view
        |> Program.withRouter router
