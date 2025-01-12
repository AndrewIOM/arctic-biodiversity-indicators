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
            | _ -> None

    type EBVCategory = {
        Label: string
        EBVs: EssentialBiodiversityVariable list
    }

type Dataset = Map<string, float * float>

module DataAccess =

    open BiodiversityCoder.Core
    open FSharp.Data

    // RawDataFor: string -> string list -> (decimal * decimal) seq

    // GetProcessedEbvData: ModelParts.EssentialBiodiversityVariable -> ModelParts.DimensionView -> Async<Dataset[]>

    type Dataset = 

    let loadEbvData ebv =
        match ebv with
        | ModelParts.EssentialBiodiversityVariable.TaxonDistribution ->
            


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

    module RawCalibrated =

        type RawCalibratedData = CsvProvider<"../../data-derived/raw-data-calibrated.tsv">

        let load (httpClient:HttpClient) =
            task {
                let! csv = httpClient.GetStringAsync "/content/indicators/raw-data-calibrated.tsv"
                return RawCalibratedData.Parse csv
            }

        let dataForTimeline (taxa: TaxonIndex.TaxonIndexItem list) timeline (data:RawCalibratedData) : (decimal * decimal) seq =
            data.Rows
            |> Seq.filter(fun r -> r.Timeline_id = timeline)
            |> Seq.filter(fun r ->
                taxa |> Seq.exists (fun t ->
                    t.LatinName :: t.IncludedLatinNames |> List.contains r.Taxon))
            |> Seq.map(fun r -> r.Age_ybp, r.``Data-value``)


/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home
    | [<EndPoint "/ebv/{name}">] EssentialBioVariable of name:string
    | [<EndPoint "/ebv/spatial/{name}">] EssentialBioVariableSpatial of name:string
    | [<EndPoint "/sources/{sourceId}">] Sources of sourceId:string option
    | [<EndPoint "/{*tags}">] MarkdownPage of tags: list<string>


/// The Elmish application's model.
type Model =
    {
        page: Page
        markdown: string option
        inpagelinks: IndexItem list
        
        error: string option
        data: DataAccess.RawCalibrated.RawCalibratedData option
        dataSlice: Map<ModelParts.TimelineWithLocation, (decimal * decimal) seq>
        filterByTaxa: DataAccess.TaxonIndex.TaxonIndexItem list
        selectedRankFilter: string
        dimension: ModelParts.DimensionView
        taxonList: Map<string, DataAccess.TaxonIndex.TaxonIndexItem list>
        timelineList: ModelParts.TimelineWithLocation list
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
    | LoadMarkdownPage of string list
    | LoadedMarkdownPage of string * IndexItem list

    | LoadTaxonIndex
    | LoadedTaxonIndex of DataAccess.TaxonIndex.TaxonIndexItem list

    | LoadIndicatorData of ModelParts.EssentialBiodiversityVariable
    | LoadedIndicatorData of Dataset[]
    | Error of exn
    | ClearError

    | LoadRawData
    | LoadedRawData of DataAccess.RawCalibrated.RawCalibratedData
    | SliceRawData

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
        | _ -> { model with page = page}, Cmd.none
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

    | Error exn ->
        printfn "Error! Was %A" exn
        { model with error = Some exn.Message }, Cmd.none
    | ClearError ->
        { model with error = None }, Cmd.none
    
    | LoadRawData ->
        printfn "Loading..."
        model, Cmd.OfTask.either (fun _ -> DataAccess.RawCalibrated.load httpClient) () LoadedRawData Error
    | LoadedRawData raw ->
        let timelines =
            raw.Rows 
            |> Seq.map(fun r -> { TimelineId = r.Timeline_id; LocationName = r.Site_name; Coordinate = r.Coord })
            |> Seq.distinct
            |> Seq.toList
        { model with data = Some raw; timelineList = timelines }, Cmd.none

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

    | ChangeFilterRank r -> { model with selectedRankFilter = r}, Cmd.none
    | AddTaxonToFilter taxon ->
        let toAdd = model.taxonList |> Map.find model.selectedRankFilter |> Seq.find(fun t -> t.LatinName = taxon)
        { model with filterByTaxa = toAdd :: model.filterByTaxa }, Cmd.ofMsg SliceRawData
    | AddTimelineToFilter timeline ->
        match model.dimension with
        | Temporal t -> { model with dimension = Temporal (timeline :: t) }, Cmd.ofMsg SliceRawData
        | Spatial (t,s) -> { model with dimension = Spatial ((timeline :: t),s) }, Cmd.ofMsg SliceRawData

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

    // Assumes that 1 = present, 0.5 = maybe present, 0 = absent, NA = unknown
    let presenceHeatmap (siteData: (string * float list) list) =
        let annotation =
            siteData |> List.map snd |> List.map(fun p ->
                p |> List.map(fun p2 ->
                    match p2 with
                    | 1. -> "P" | 0.5 -> "M" | 0. -> "A" | f when Double.IsNaN f -> "Unk." | _ -> ""))
        Chart.AnnotatedHeatmap(
            zData = (siteData |> List.map snd),
            annotationText = annotation,
            X = (siteData |> List.map fst),
            Y = [ 0 .. 500 .. 12000],
            ReverseYAxis = true
        )


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
                        text (timelineList |> List.find(fun t2 -> t2.TimelineId = t)).Coordinate
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

let homePage (model: Model) dispatch =
    div {
        h1 { text "Slice raw data by taxon and place" }

        h1 { text "Plant traits test plot" }
        Plots.traitRadial
    }

let ebvPage (ebv:EssentialBiodiversityVariable) model dispatch =
    concat {
        textf "Some page for %s" ebv.Name
        selectTaxaMulti model dispatch
        selectTimelineMulti model.dimension model.timelineList dispatch
        cond ebv <| function
        | TaxonDistribution -> Plots.presenceHeatmap model.dataSlice
            // Show a heatmap of the EBV data?
        | RawAbundanceData -> Plots.slicedRawData model.dataSlice
    }

let view model dispatch =
    View.main
        (View.menu model)
        (
            cond model.page <| function
            | Home -> homePage model dispatch
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
        Program.mkProgram (fun _ -> initModel, Cmd.batch [ Cmd.ofMsg LoadRawData; Cmd.ofMsg LoadTaxonIndex]) update view
        |> Program.withRouter router
