module GraphExplorer.Client.Main

open System
open System.Net.Http
open Microsoft.AspNetCore.Components
open Elmish
open Bolero
open Bolero.Html

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

type IndexItem = { Tag: string; Name: string }

module Markdown =

    let read (client:HttpClient) tag =
        client.GetStringAsync("/content/" + tag + ".md")

    let asHtml (document:string) = 
        let html = Markdig.Markdown.ToHtml(document)
        let subheadings = 
            System.Text.RegularExpressions.Regex.Matches(html, "<h3>(.*)</h3>")
            |> Seq.map(fun s -> s.Groups.[1].Value)
        Seq.fold(fun (html: string) title ->
            html.Replace(sprintf "<h3>%s</h3>" title, 
                sprintf "<h3 id='%s'>%s</h3>" (System.Web.HttpUtility.UrlEncode title) title)) html subheadings,
        subheadings |> Seq.map(fun s -> {
            Name = s; Tag = System.Web.HttpUtility.UrlEncode s
        }) |> Seq.toList

    let getContentAsync client name =
        task {
            let! content = read client name
            return content |> asHtml
        } |> Async.AwaitTask


module ModelParts =

    type SpatialUnit = {
        Label: string
        GeoJSON: string
    }

    type TimelineWithLocation = {
        TimelineId: string
        LatitudeDD: float
        LongitudeDD: float
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
        | RawAbundanceData

    with
        member this.Name =
            match this with
            | TaxonDistribution -> "Taxon distribution"
            | Morphology -> "Morphology"
            | Movement -> "Movement"
            | TaxonomicAndPhylogeneticDiversity -> "Taxonomic and phylogenetic diversity"
            | TraitDiversity -> "Trait diversity"
            | RawAbundanceData -> "Raw datasets"

        member this.Slug =
            match this with
            | TaxonDistribution -> "distribution"
            | Morphology -> "morphology"
            | Movement -> "movement"
            | TaxonomicAndPhylogeneticDiversity -> "taxonomic-diversity"
            | TraitDiversity -> "trait-diversity"
            | RawAbundanceData -> "raw-abundance-data"

        static member FromSlug slug =
            match slug with
            | "distribution" -> Some TaxonDistribution
            | "morphology" -> Some Morphology
            | "movement" -> Some Movement
            | "taxonomic-diversity" -> Some TaxonomicAndPhylogeneticDiversity
            | "trait-diversity" -> Some TraitDiversity
            | "raw-abundance-data" -> Some RawAbundanceData
            | _ -> None

    type EBVCategory = {
        Label: string
        EBVs: EssentialBiodiversityVariable list
    }

module DataAccess =

    open FSharp.Data

    module TaxonIndex =

        type TaxonIndex = CsvProvider<"../../data-derived/taxon-index.tsv">

        // For each taxon, find those that fall
        // underneath it and add to a list
        type TaxonIndexItem = {
            IncludedLatinNames: string list
            LatinName: string
            Rank: string
        }

        let load (httpClient:HttpClient) =
            task {
                let! csv = httpClient.GetStringAsync "/content/indicators/taxon-index.tsv"
                let data = TaxonIndex.Parse csv
                let formatted =
                    data.Rows
                    |> Seq.map(fun row ->
                        let included =
                            data.Rows 
                            |> Seq.filter(fun row2 -> row2.Taxonomic_tree.Split(" > ") |> Seq.contains row.Taxon)
                            |> Seq.map(fun row2 -> row2.Taxon)
                            |> Seq.toList
                        {
                            IncludedLatinNames = included
                            LatinName = row.Taxon
                            Rank = row.Rank
                        }
                    )
                    |> Seq.toList
                return formatted
            }

    module TimelineIndex =

        type TimelineIndex = CsvProvider<"../../data-derived/timeline-index.tsv">

        let load (httpClient:HttpClient) =
            task {
                let! csv = httpClient.GetStringAsync "/content/indicators/timeline-index.tsv"
                return (TimelineIndex.Parse csv).Rows |> Seq.toList
            }

    type DatasetEBV = CsvProvider<"../essential-bio-variables/samples/biodiversity-variable.tsv">

    let loadRawData (httpClient:HttpClient) =
        task {
            let! csv = httpClient.GetStringAsync "/content/indicators/raw-data-calibrated.tsv"
            return (DatasetEBV.Parse csv).Rows |> Seq.toList
        }

    let loadEbvData ebv (httpClient:HttpClient) =
        task {
            let url =
                match ebv with
                | ModelParts.EssentialBiodiversityVariable.TaxonDistribution -> "/content/indicators/populations/taxon-presence.tsv"
                | ModelParts.EssentialBiodiversityVariable.TraitDiversity -> "/content/indicators/traits/plant-morphology.tsv"
            let! csv = httpClient.GetStringAsync url
            return (DatasetEBV.Parse csv).Rows |> Seq.toList
        }

    // RawDataFor: string -> string list -> (decimal * decimal) seq
    // GetProcessedEbvData: ModelParts.EssentialBiodiversityVariable -> ModelParts.DimensionView -> Async<Dataset[]>
    let dataForTimeline (taxa: TaxonIndex.TaxonIndexItem list) timeline (data:list<DatasetEBV.Row>) =
        data
        |> Seq.filter(fun r -> r.Timeline_id = timeline)
        |> Seq.filter(fun r ->
            taxa |> Seq.exists (fun t ->
                t.LatinName :: t.IncludedLatinNames |> List.contains r.Taxon))
        |> Seq.map(fun r -> r.Bin_early, r.Variable_value, r.Variable_confidence_qualitative)

    /// Direct read-only access to the holocene map graph database from github.
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



/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home
    | [<EndPoint "/ebv/{name}">] EssentialBioVariable of name:string
    | [<EndPoint "/ebv/spatial/{name}">] EssentialBioVariableSpatial of name:string
    // | [<EndPoint "/sources/{sourceId}">] Sources of sourceId:string option
    | [<EndPoint "/{*tags}">] MarkdownPage of tags: list<string>

/// The Elmish application's model.
type Model =
    {
        page: Page
        markdown: string option
        inpagelinks: IndexItem list
        error: string option
        
        taxonList: Map<string, DataAccess.TaxonIndex.TaxonIndexItem list>
        timelineList: ModelParts.TimelineWithLocation list

        data: DataAccess.DatasetEBV.Row list option
        dataSlice: Map<ModelParts.TimelineWithLocation, (int * float * option<string>) seq>
        filterByTaxa: DataAccess.TaxonIndex.TaxonIndexItem list
        selectedRankFilter: string
        dimension: ModelParts.DimensionView
    }

let initModel =
    {
        page = Home
        error = None
        data = None
        dataSlice = Map.empty
        filterByTaxa = []
        selectedRankFilter = "Genus"
        dimension = ModelParts.Temporal []
        taxonList = Map.empty
        timelineList = []
        inpagelinks = []
        markdown = None
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
        // Movement
      ] }
    // { Label = "Community composition"
    //   EBVs = [
    //     TaxonomicAndPhylogeneticDiversity
    //     TraitDiversity
    //   ] }
    { Label = "Underlying data"
      EBVs = [
        RawAbundanceData
      ] }
]

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | LoadMarkdownPage of string list
    | LoadedMarkdownPage of string * IndexItem list

    | LoadTaxonIndex
    | LoadedTaxonIndex of DataAccess.TaxonIndex.TaxonIndexItem list
    | LoadTimelineIndex
    | LoadedTimelineIndex of DataAccess.TimelineIndex.TimelineIndex.Row list

    | Error of exn
    | ClearError

    | LoadIndicatorData of ModelParts.EssentialBiodiversityVariable
    | LoadedIndicatorData of DataAccess.DatasetEBV.Row list
    | SliceIndicatorData

    | ChangeFilterRank of string
    | AddTaxonToFilter of string
    | AddTimelineToFilter of string

let update httpClient message model =
    match message with
    | SetPage page ->
        match page with
        | MarkdownPage tags ->
            { model with page = page }, Cmd.ofMsg(LoadMarkdownPage tags)
        | Home -> { model with page = page }, Cmd.ofMsg(LoadMarkdownPage ["index"])
        | EssentialBioVariable ebv ->
            match ebv |> EssentialBiodiversityVariable.FromSlug with
            | Some ebv -> { model with page = page; data = None }, Cmd.ofMsg(LoadIndicatorData ebv)
            | None -> { model with page = Home; error = Some (sprintf "EBV not found: %s" ebv); data = None }, Cmd.none
        | EssentialBioVariableSpatial ebv ->
            { model with page = Home; error = Some "Spatial EBVs not yet implemented" }, Cmd.none
    | LoadMarkdownPage(name) ->
        let pageToLoad =
            if name.IsEmpty then "index"
            else name |> String.concat "/"
        model, Cmd.OfAsync.either
            (Markdown.getContentAsync httpClient) pageToLoad
            (fun md -> LoadedMarkdownPage md)
            (fun e -> Error e)
    | LoadedMarkdownPage (md,titles) ->
        { model with markdown = Some md; inpagelinks = titles }, Cmd.none

    | LoadTaxonIndex ->
        model, Cmd.OfTask.either (fun _ -> DataAccess.TaxonIndex.load httpClient) () LoadedTaxonIndex Error
    | LoadedTaxonIndex taxa ->
        let map =
            taxa |> List.groupBy(fun i -> i.Rank) |> Map.ofList
        { model with taxonList = map }, Cmd.none

    | LoadTimelineIndex ->
        model, Cmd.OfTask.either (fun _ -> DataAccess.TimelineIndex.load httpClient) () LoadedTimelineIndex Error
    | LoadedTimelineIndex timelines ->
        let timelines =
            timelines
            |> List.map(fun t -> {
                LatitudeDD = t.Latitude_dd
                LongitudeDD = t.Longitude_dd
                TimelineId = t.Timeline_id
                LocationName = t.Site_name })
        { model with timelineList = timelines }, Cmd.none

    | Error exn ->
        printfn "Error! Was %A" exn
        { model with error = Some exn.Message }, Cmd.none
    | ClearError ->
        { model with error = None }, Cmd.none
    
    | LoadIndicatorData ebv ->
        let task =
            match ebv with
            | RawAbundanceData -> DataAccess.loadRawData
            | _ -> DataAccess.loadEbvData ebv
        model, Cmd.OfTask.either (fun _ -> task httpClient) () LoadedIndicatorData Error
    | LoadedIndicatorData raw ->
        { model with data = Some raw }, Cmd.none

    | SliceIndicatorData ->
        match model.data with
        | None -> { model with error = Some "Raw datasets not yet loaded"}, Cmd.none
        | Some dataset ->
            match model.dimension with
            | Temporal timelines ->
                let sliced =
                    timelines
                    |> List.map (fun t -> 
                        model.timelineList |> Seq.find (fun ts -> ts.TimelineId = t), 
                        DataAccess.dataForTimeline model.filterByTaxa t dataset)
                    |> Map.ofList
                { model with dataSlice = sliced }, Cmd.none
            | Spatial _ -> model, Cmd.none

    | ChangeFilterRank r -> { model with selectedRankFilter = r}, Cmd.none
    | AddTaxonToFilter taxon ->
        let toAdd = model.taxonList |> Map.find model.selectedRankFilter |> Seq.find(fun t -> t.LatinName = taxon)
        { model with filterByTaxa = toAdd :: model.filterByTaxa }, Cmd.ofMsg SliceIndicatorData
    | AddTimelineToFilter timeline ->
        match model.dimension with
        | Temporal t -> { model with dimension = Temporal (timeline :: t) }, Cmd.ofMsg SliceIndicatorData
        | Spatial (t,s) -> { model with dimension = Spatial ((timeline :: t),s) }, Cmd.ofMsg SliceIndicatorData

/// Connects the routing system to the Elmish application.
let router = Router.infer SetPage (fun model -> model.page)

module View =

    let markdownPage model dispatch =
        div {
            attr.``class`` "content"
            cond model.markdown <| function
            | Some md -> rawHtml md
            | None -> text "Loading..."
        }

    let menu (model: Model) =
        concat {
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
            p { "Information" }
            forEach [ "About", "about"; "The Arctic Holocene Biodiversity Database (AHBDB)", "ahbdb" ] <|
                function (name,slug) ->
                            li {
                                a {
                                    attr.``class`` (if model.page = (Page.MarkdownPage [slug]) then "is-active" else "")
                                    attr.href (router.Link (Page.MarkdownPage [slug] ))
                                    name
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

    let slicedRawData (model:Map<TimelineWithLocation,seq<int * float * option<string>>>) =
        cond model.IsEmpty <| function
        | true -> text "No data to display"
        | false ->
            model
            |> Seq.map(fun kv ->
                Chart.Line(kv.Value |> Seq.map(fun (_,i,_) -> i), kv.Value |> Seq.map(fun (i,_,_) -> i), Name = kv.Key.LocationName, ShowMarkers = true, Orientation = StyleParam.Orientation.Vertical)
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

    // Assumes that 1 = present, 0.5 = maybe present, 0 = absent, NA = unknown
    let presenceHeatmap (siteData: Map<TimelineWithLocation,seq<int * float * option<string>>>) = // (siteData: (string * float list) list) =
        if siteData.Count = 0 then text "No timelines selected"
        else
            let annotation =
                siteData |> Map.values |> Seq.map(fun p ->
                    p |> Seq.map(fun (_,p2,confidence) ->
                        match p2 with
                        | 1. -> "P" | 0.5 -> "M" | 0. -> "A" | f when Double.IsNaN f -> "Unk." | _ -> ""))
            let timeBinStarts = siteData |> Map.values |> Seq.head |> Seq.map (fun (i,_,_) -> i)
            let zData =
                siteData |> Map.values |> Seq.map(fun d -> d |> Seq.map (fun (_,i,_) -> i))

            printfn "z data was %A" zData

            let scale = 
                StyleParam.Colorscale.Custom [
                    
                ]

            Chart.AnnotatedHeatmap(
                zData = zData,
                annotationText = annotation,
                Y = (siteData.Keys |> Seq.map(fun k -> k.LocationName)),
                X = timeBinStarts,
                ShowScale = false,
                ColorScale = StyleParam.Colorscale.Viridis,
                ReverseYAxis = true
            )
            |> Chart.withXAxisStyle (TitleText = "1,000 Calendar years before present (cal yr BP)")
            |> Chart.withYAxisStyle (TitleText = "Location name")
            |> GenericChart.toChartHTML
            |> rawHtml


    let traitRadial =
        let radial = [ 1; 2; 3; 2; 5; 11 ]
        let theta = [ "Leaf area"; "Leaf nitrogen"; "Leaf mass"; "Plant height"; "Diaspore mass"; "Stem density" ]
        let pointPolar = Chart.PointPolar(r = radial, theta = theta)
        pointPolar |> GenericChart.toChartHTML |> rawHtml

let selectTaxaMulti model dispatch =
    div {
        attr.``class`` "box"
        ul {
            forEach model.filterByTaxa <| fun t ->
                li {
                    textf "%s (%s)" t.LatinName t.Rank
                }
        }
        div {
            attr.``class`` "field"
            label {
                attr.``class`` "label"
                text "Taxonomic rank"
            }
            div {
                attr.``class`` "select"
                select {
                    attr.``class`` "select"
                    bind.change.string model.selectedRankFilter (fun s -> ChangeFilterRank s |> dispatch)
                    forEach [ "Phylum"; "Class"; "Order"; "Family"; "Genus"; "Species"; "Subspecies"; "Variety" ] <| fun t ->
                        option {
                            attr.name t
                            attr.value t
                            text t
                        }
                }
            }
        }
        div {
            attr.``class`` "field"
            label {
                attr.``class`` "label"
                text "Select a taxon"
            }
            div {
                attr.``class`` "select"
                select {
                    bind.change.string "" (fun s -> AddTaxonToFilter s |> dispatch)
                    cond (model.taxonList |> Map.tryFind model.selectedRankFilter) <| function
                    | None -> empty()
                    | Some ls ->
                        forEach ls <| fun t ->
                            option {
                                attr.name t.LatinName
                                attr.value t.LatinName
                                text t.LatinName
                            }
                }
            }
        }
    }

let selectTimelineMulti dimension (timelineList: ModelParts.TimelineWithLocation list) dispatch =
    cond dimension <| function
    | Temporal timelines ->
        div {
            attr.``class`` "box"
            ul {
                forEach timelines <| fun t ->
                    li {
                        text (timelineList |> List.find(fun t2 -> t2.TimelineId = t)).LocationName
                        textf "%f, %f"
                            (timelineList |> List.find(fun t2 -> t2.TimelineId = t)).LatitudeDD
                            (timelineList |> List.find(fun t2 -> t2.TimelineId = t)).LongitudeDD
                    }
            }
            select {
                bind.change.string "" (fun s -> AddTimelineToFilter s |> dispatch)
                concat {
                    option {
                        attr.disabled "disabled"
                        attr.selected "selected"
                        attr.value ""
                        text "-- Select a location --"
                    }
                    forEach timelineList <| fun t ->
                        option {
                            attr.name t.TimelineId
                            attr.value t.TimelineId
                            text t.LocationName
                        }
                }
            }
        }

let ebvPage (ebv:EssentialBiodiversityVariable) model dispatch =
    concat {
        textf "Some page for %s" ebv.Name
        selectTaxaMulti model dispatch
        selectTimelineMulti model.dimension model.timelineList dispatch
        cond ebv <| function
        | TaxonDistribution -> Plots.presenceHeatmap model.dataSlice
        | RawAbundanceData -> Plots.slicedRawData model.dataSlice        
        | Morphology ->
            concat {
                h1 { text "Plant traits test plot" }
                Plots.traitRadial
            }
        | Movement -> failwith "Not Implemented"
        | TaxonomicAndPhylogeneticDiversity -> failwith "Not Implemented"
        | TraitDiversity -> failwith "Not Implemented"
    }

let view model dispatch =
    View.main
        (View.menu model)
        (
            cond model.page <| function
            | Home -> View.markdownPage model dispatch
            | MarkdownPage _ -> View.markdownPage model dispatch
            | EssentialBioVariable ebv ->
                match EssentialBiodiversityVariable.FromSlug ebv with
                | Some ebv -> ebvPage ebv model dispatch
                | None -> text "Not found"
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
        Program.mkProgram (fun _ -> initModel, Cmd.batch [ Cmd.ofMsg LoadTaxonIndex; Cmd.ofMsg LoadTimelineIndex ]) update view
        |> Program.withRouter router
