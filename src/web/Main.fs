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
        client.GetStringAsync("content/" + tag + ".md")

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

    type TimelineWithLocation = {
        TimelineId: string
        LatitudeDD: float
        LongitudeDD: float
        LocationName: string
    }

    /// Data indexed by variable name, then
    /// sliced by a dimension e.g. timeline
    type DataIndexed =
        | NoData
        | Unindexed of Map<DataVariable,(int * float * option<string>) seq>
        | IndexedByTimeline of Map<TimelineWithLocation, Map<DataVariable,(int * float * option<string>) seq>>
        | IndexedByPolygonId of Map<string, Map<DataVariable,(int * float * option<string>) seq>>

    and DataVariable = {
        VariableName: string
        VariableUnit: string
    }

    type DimensionView =
        | Temporal of timelines: string list * TaxonomicView
        | SpatialStatic of TimeMode * TaxonomicView

    and TimeMode =
        | IndividualTimesteps
        | ChangeOverTime of earlierAge: int * laterAge: int

    and TaxonomicView =
        | CommunityLevel
        | TaxonLevel

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
    open Newtonsoft.Json
    open ModelParts

    module GeoJSON =

        let load geojsonName (httpClient:HttpClient) =
            task {
                let! json = httpClient.GetStringAsync (sprintf "content/geo/%s.json" geojsonName)
                return JsonConvert.DeserializeObject json
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
                let! csv = httpClient.GetStringAsync "content/indicators/taxon-index.tsv"
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
                let! csv = httpClient.GetStringAsync "content/indicators/timeline-index.tsv"
                return (TimelineIndex.Parse csv).Rows |> Seq.toList
            }

    type DatasetEBV = CsvProvider<"../essential-bio-variables/samples/biodiversity-variable.tsv">

    let loadRawData (httpClient:HttpClient) =
        task {
            let! csv = httpClient.GetStringAsync "content/indicators/raw-data-calibrated.tsv"
            return (DatasetEBV.Parse csv).Rows |> Seq.toList
        }

    let loadEbvData isSpatial ebv (httpClient:HttpClient) =
        task {
            let url =
                match ebv with
                | ModelParts.EssentialBiodiversityVariable.TaxonDistribution -> "content/indicators/populations/taxon-presence.tsv"
                | ModelParts.EssentialBiodiversityVariable.Morphology -> "content/indicators/traits/plant-morphology.tsv"
                |> fun u ->
                    match isSpatial with
                    | true -> u.Replace(".tsv", "_spatial.tsv")
                    | false -> u
            let! csv = httpClient.GetStringAsync url
            return (DatasetEBV.Parse csv).Rows |> Seq.toList
        }

    let filterByTaxa (taxa: TaxonIndex.TaxonIndexItem list) (data:seq<DatasetEBV.Row>) =
        data
        |> Seq.filter(fun r ->
            if r.Taxon = "community" then true // Always include 'community-level' data
            else
                taxa |> Seq.exists (fun t ->
                    t.LatinName :: t.IncludedLatinNames |> List.contains r.Taxon))

    let dataForTimeline (taxa: TaxonIndex.TaxonIndexItem list) timeline (data:list<DatasetEBV.Row>) =
        data
        |> Seq.filter(fun r -> r.Location_id = timeline)
        |> filterByTaxa taxa
        |> Seq.groupBy(fun r -> r.Variable, r.Variable_unit)
        |> Seq.map(fun ((v,vu), rows) ->
            { VariableName = v; VariableUnit = vu },
            rows |> Seq.map(fun r -> r.Bin_late, r.Variable_value, r.Variable_confidence_qualitative)
        ) |> Map.ofSeq

    let indexByLocationFor (taxa: TaxonIndex.TaxonIndexItem list) (data:list<DatasetEBV.Row>) =
        data
        |> filterByTaxa taxa
        |> Seq.groupBy(fun r -> r.Location_id)
        |> Seq.map(fun (loc, rows) ->
            loc,
            rows
            |> Seq.groupBy(fun r -> r.Variable, r.Variable_unit)
            |> Seq.map(fun ((v,vu),rows) ->
                { VariableName = v; VariableUnit = vu },
                rows |> Seq.map(fun r -> r.Bin_late, r.Variable_value, r.Variable_confidence_qualitative))
                |> Map.ofSeq
        ) |> Map.ofSeq

    let indexVariableByLocation variable (data:Map<string,Map<DataVariable,seq<int * float * Option<string>>>>) =
        data |> Map.map(fun k v ->
            v |> Map.tryFind variable
            )
        |> Map.filter(fun k v -> v.IsSome)
        |> Map.map(fun k v -> v.Value)


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

open ModelParts

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home
    | [<EndPoint "/ebv/{name}">] EssentialBioVariable of name:string
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

        geojson: Map<string, obj>

        data: DataAccess.DatasetEBV.Row list option
        dataSlice: DataIndexed
        filterByTaxa: DataAccess.TaxonIndex.TaxonIndexItem list
        selectedRankFilter: string
        selectedVariable: DataVariable option
        dimension: ModelParts.DimensionView
    }

let initModel =
    {
        page = Home
        error = None
        data = None
        dataSlice = NoData
        geojson = Map.empty
        filterByTaxa = []
        selectedRankFilter = "Genus"
        selectedVariable = None
        dimension = ModelParts.Temporal ([], TaxonomicView.TaxonLevel)
        taxonList = Map.empty
        timelineList = []
        inpagelinks = []
        markdown = None
    }


// Browse EBVs by time and by spatial aggregation.
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

    | LoadGeoJson of name:string
    | LoadedGeoJson of name:string * json:obj

    | Error of exn
    | ClearError

    | LoadIndicatorData of ModelParts.EssentialBiodiversityVariable
    | LoadedIndicatorData of DataAccess.DatasetEBV.Row list
    | SliceIndicatorData

    | ChangeFilterRank of string
    | AddTaxonToFilter of string
    | AddTimelineToFilter of string
    | SetDimension of DimensionView
    | SetVariable of string option

let update httpClient message model =
    match message with
    | SetPage page ->
        match page with
        | MarkdownPage tags ->
            { model with page = page }, Cmd.ofMsg(LoadMarkdownPage tags)
        | Home -> { model with page = page }, Cmd.ofMsg(LoadMarkdownPage ["index"])
        | EssentialBioVariable ebv ->
            match ebv |> EssentialBiodiversityVariable.FromSlug with
            | Some ebv -> { model with page = page; data = None }, Cmd.batch [ Cmd.ofMsg(LoadIndicatorData ebv); Cmd.ofMsg (SetVariable None) ]
            | None -> { model with page = Home; error = Some (sprintf "EBV not found: %s" ebv); data = None }, Cmd.none
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
            taxa 
            |> List.groupBy(fun i -> i.Rank)
            |> List.map(fun (g,l) -> g, l |> List.sortBy(fun t -> t.LatinName))
            |> Map.ofList
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
            |> List.sortBy(fun t -> t.LocationName)
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
            | _ ->
                match model.dimension with
                | DimensionView.SpatialStatic _ -> DataAccess.loadEbvData true ebv
                | DimensionView.Temporal _ -> DataAccess.loadEbvData false ebv
        { model with data = None }, Cmd.OfTask.either (fun _ -> task httpClient) () LoadedIndicatorData Error
    | LoadedIndicatorData raw ->
        { model with data = Some raw }, Cmd.ofMsg SliceIndicatorData

    | SliceIndicatorData ->
        match model.data with
        | None -> { model with error = Some "Raw datasets not yet loaded"}, Cmd.none
        | Some dataset ->
            match model.dimension with
            | Temporal (timelines, view) ->
                let sliced =
                    timelines
                    |> List.map (fun t -> 
                        model.timelineList |> Seq.find (fun ts -> ts.TimelineId = t), 
                        DataAccess.dataForTimeline model.filterByTaxa t dataset)
                    |> Map.ofList
                { model with dataSlice = DataIndexed.IndexedByTimeline sliced }, Cmd.none
            | SpatialStatic _ ->
                let sliced = DataAccess.indexByLocationFor model.filterByTaxa dataset
                printfn "Sliced %A" sliced
                { model with dataSlice = DataIndexed.IndexedByPolygonId sliced }, Cmd.none

    | SetDimension dim -> { model with dimension = dim }, Cmd.ofMsg (SetPage model.page) // Causes reload of correct data
    | ChangeFilterRank r -> { model with selectedRankFilter = r}, Cmd.none
    | AddTaxonToFilter taxon ->
        let toAdd = model.taxonList |> Map.find model.selectedRankFilter |> Seq.find(fun t -> t.LatinName = taxon)
        { model with filterByTaxa = toAdd :: model.filterByTaxa }, Cmd.ofMsg SliceIndicatorData
    | AddTimelineToFilter timeline ->
        match model.dimension with
        | Temporal (t,t2) -> { model with dimension = Temporal (timeline :: t, t2) }, Cmd.ofMsg SliceIndicatorData
        | SpatialStatic _ -> model, Cmd.none
        // | SpatialDynamic (t,s) -> { model with dimension = SpatialDynamic ((timeline :: t),s) }, Cmd.ofMsg SliceIndicatorData
    | SetVariable someVar ->
        match someVar with
        | None -> { model with selectedVariable = None }, Cmd.none
        | Some someVar ->
            let v =
                match model.dataSlice with
                | IndexedByPolygonId m ->
                    m |> Seq.collect(fun kv -> kv.Value.Keys)
                    |> Seq.tryFind (fun v2 -> v2.VariableName = someVar)
                | IndexedByTimeline m ->
                    m |> Seq.collect(fun kv -> kv.Value.Keys)
                    |> Seq.tryFind (fun v2 -> v2.VariableName = someVar)
                | _ -> None
            { model with selectedVariable = v }, Cmd.none

    | LoadGeoJson name ->
        model, Cmd.OfTask.either (fun _ -> DataAccess.GeoJSON.load name httpClient) () (fun x -> LoadedGeoJson (name,x)) Error
    | LoadedGeoJson (name, json) ->
        { model with geojson = model.geojson |> Map.add name json }, Cmd.none


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
    open Plotly.NET.LayoutObjects

    Defaults.DefaultTemplate <- ChartTemplates.lightMirrored
    Defaults.DefaultHeight <- 300

    let slicedRawData (model:Map<TimelineWithLocation, Map<DataVariable,(int * float * option<string>) seq>>) =
        cond model.IsEmpty <| function
        | true -> text "No data to display"
        | false ->
            model
            |> Seq.collect(fun kv ->
                kv.Value
                |> Seq.map(fun kv2 ->
                    Chart.Line(kv2.Value |> Seq.map(fun (_,i,_) -> i), kv2.Value |> Seq.map(fun (i,_,_) -> i), Name = kv.Key.LocationName, ShowMarkers = true, Orientation = StyleParam.Orientation.Vertical)
                    |> Chart.withYAxisStyle(MinMax = (12000, 0))
                    |> Chart.withXAxisStyle(MinMax = (0, 50), TitleText = sprintf "%s (%s)" kv2.Key.VariableName kv2.Key.VariableUnit)
                )
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
    let presenceHeatmap (siteData: Map<TimelineWithLocation, Map<DataVariable,(int * float * option<string>) seq>>) = // (siteData: (string * float list) list) =
        let v = { VariableName = "presence"; VariableUnit = "present-absent" }
        if siteData.Count = 0 then text "No timelines selected"
        else
            let annotation =
                siteData |> Map.values |> Seq.map(fun p ->
                    p.[v] |> Seq.map(fun (_,p2,confidence) ->
                        match p2 with
                        | 1. -> "P" | 0.5 -> "M" | 0. -> "A" | f when Double.IsNaN f -> "Unk." | _ -> ""))
            let timeBinStarts = (siteData |> Map.values |> Seq.head).[v] |> Seq.map (fun (i,_,_) -> i)
            let zData =
                siteData |> Map.values |> Seq.map(fun d -> d.[v] |> Seq.map (fun (_,i,_) -> i))

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


    let traitRadial (siteData: Map<TimelineWithLocation, Map<DataVariable,(int * float * option<string>) seq>>) =
        cond (siteData.Count = 0) <| function
        | true -> text "No timeline selected"
        | false ->
            let labels, radial =
                siteData
                |> Seq.collect(fun kv ->
                    kv.Value
                    |> Map.map(fun v data -> data |> Seq.head |> fun (_,t,_) -> t)
                    |> Map.toList
                    )
                |> Seq.toList
                |> List.unzip
            let theta = [ "Leaf area"; "Leaf nitrogen"; "Leaf mass"; "Plant height"; "Diaspore mass"; "Stem density" ]
            let pointPolar = Chart.LinePolar(r = radial, theta = theta)
            pointPolar |> GenericChart.toChartHTML |> rawHtml

    let responsiveConfig = Config.init (Responsive = true, DisplayModeBar = false)

    let chloropleth' locations z geoJson =
        Chart.ChoroplethMap(
            locations = locations, z = z,
            LocationMode = StyleParam.LocationFormat.GeoJson_Id,
            GeoJson = geoJson,
            FeatureIdKey = "properties.id"
        )
        |> Chart.withSize(600, 600)
        |> Chart.withConfig responsiveConfig
        |> Chart.withLayout(Layout.init(
            Margin = Margin.init(0,0,0,0)
        ))
        |> Chart.withGeoStyle (
            FitBounds = StyleParam.GeoFitBounds.GeoJson,
            Projection = GeoProjection.init (projectionType = StyleParam.GeoProjectionType.AzimuthalEquidistant)
        )
        |> Chart.withYAxisStyle(ShowGrid = true)
        |> Chart.withXAxisStyle(ShowGrid = true)


    /// Given a geojson with feature IDs 'id',
    /// plot the data as it corresponds to the 'id' field
    /// of the geojson.
    let chloropleth timeMode geoJson (data:Map<string,seq<int * float * Option<string>>>) =
        printfn "Starting chloropleth"
        
        cond timeMode <| function
        | TimeMode.ChangeOverTime (earlier, later) ->

            // Change over time = absolute difference between each time point, summed.
            let locations, z =
                data
                |> Seq.map(fun kv ->
                    kv.Key,
                    kv.Value 
                    |> Seq.filter(fun (i,v,_) -> i >= later && i <= earlier)
                    |> Seq.sortBy(fun (i,_,_) -> -i) // Sort from oldest to newest
                    |> Seq.fold (fun (state, lastVal) (i,v,_) ->
                        if lastVal = Double.MaxValue then state, v
                        else state + abs(v-lastVal), v) (0.,Double.MaxValue)
                    |> fst
                )
                |> Seq.toList
                |> List.unzip

            chloropleth' locations z geoJson
            |> GenericChart.toChartHTML
            |> rawHtml

        | TimeMode.IndividualTimesteps ->

            let steps = [ 0 .. 500 .. 12000 ]
            let sliderSteps =
                steps
                |> List.indexed
                |> List.map (fun (i, step) ->
                    let visible = (fun index -> index = i) |> Array.init steps.Length |> box
                    let title = sprintf "Presence-absence at %i cal yr BP" step |> box
                    SliderStep.init (
                        Args = [ "visible", visible; "title", title ],
                        Method = StyleParam.Method.Update,
                        Label = "v = " + string (step)
                    ))
            let slider =
                Slider.init (
                    CurrentValue = SliderCurrentValue.init (Suffix = "cal yr BP"),
                    Steps = sliderSteps
                )
            
            let map =
                steps
                |> List.map (fun stepYear ->
                    printfn "A"
                    let locations, z =
                        data
                        |> Seq.choose(fun kv ->
                            kv.Value |> Seq.tryPick(fun (i,v,_) -> if i = stepYear then Some v else None)
                            |> Option.map(fun v -> kv.Key, v)
                            )
                        |> Seq.toList
                        |> List.unzip

                    let text =
                        z |> List.map(fun v ->
                            match v with
                            | 1. -> "present"
                            | 0.5 -> "present (borderline)"
                            | 0. -> "absent"
                            | _ -> "unknown whether present or absent" )
                    
                    printfn "B"
                    chloropleth' locations z geoJson
                ) |> Chart.combine
            printfn "C"
            map
            |> Chart.withSlider slider
            |> GenericChart.toChartHTML
            |> rawHtml


    let simpleGeo caffGeoJson =
        let locations, z = [ ("Belarus", 17.5); ("Moldova", 16.8) ] |> List.unzip
        match caffGeoJson with
        | Some json ->
            Chart.ChoroplethMap(
                locations = locations, z = z,
                LocationMode = StyleParam.LocationFormat.CountryNames,
                GeoJson = json
            )
        | None ->
            Chart.ChoroplethMap(
                locations = locations, z = z,
                LocationMode = StyleParam.LocationFormat.CountryNames
            )
        |> Chart.withSize(600, 600)
        |> Chart.withGeoStyle (
            FitBounds = StyleParam.GeoFitBounds.GeoJson,
            Projection = GeoProjection.init (projectionType = StyleParam.GeoProjectionType.AzimuthalEquidistant),
            ShowLakes = true,
            ShowOcean = true,
            OceanColor = Color.fromString "white",
            ShowRivers = true
        )
        |> GenericChart.toChartHTML
        |> rawHtml


let selectTaxaMulti model dispatch =
    div {        
        div {
            attr.``class`` "field is-grouped"
            label {
                attr.``class`` "label"
                text "Add another taxon to view"
            }
            div {
                attr.``class`` "select control"
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
            div {
                attr.``class`` "select control"
                select {
                    bind.change.string "" (fun s -> AddTaxonToFilter s |> dispatch)
                    cond (model.taxonList |> Map.tryFind model.selectedRankFilter) <| function
                    | None -> empty()
                    | Some ls ->
                        concat {
                            option {
                                attr.disabled "disabled"
                                attr.selected "selected"
                                attr.value ""
                                text "-- Select a taxon --"
                            }
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
    }

let selectTimelineMulti (timelineList: ModelParts.TimelineWithLocation list) dispatch =
    concat {
        div {
            attr.``class`` "field"
            label {
                attr.``class`` "label"
                text "Add data from another location"
            }
            div {
                attr.``class`` "select control"
                select {
                    attr.``class`` "select"
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
        }
        label {
            attr.``class`` "label"
            text "Plot locations by:"
        }
        div {
            attr.``class`` "buttons has-addons"
            button {
                attr.``class`` "button is-primary is-selected"
                text "None"
            }
            button {
                attr.``class`` "button"
                attr.disabled "disabled"
                text "Northwards"
            }
        }
    }

let selectSpatialTime (timeMode:TimeMode) dispatch =
    concat {
        label {
            attr.``class`` "label"
            text "Representation of time"
        }
        div {
            attr.``class`` "buttons has-addons"
            button {
                on.click (fun _ -> SetDimension (SpatialStatic(IndividualTimesteps,TaxonLevel)) |> dispatch)
                attr.``class`` (if timeMode.IsIndividualTimesteps then "button is-primary is-selected" else "button")
                text "Show timeline"
            }
            button {
                on.click (fun _ -> SetDimension (SpatialStatic(ChangeOverTime(12000, 0), TaxonLevel)) |> dispatch)
                attr.``class`` (if timeMode.IsChangeOverTime then "button is-primary is-selected" else "button")
                text "Show summed variability through time"
            }
        }
        cond timeMode <| function
        | IndividualTimesteps -> empty ()
        | ChangeOverTime (earlier, later) ->
            div {
                attr.``class`` "field is-grouped is-grouped-multiline"
                div {
                    attr.``class`` "select"
                    select {
                        bind.change.int later (fun i -> SetDimension (SpatialStatic(ChangeOverTime(earlier, i), TaxonLevel)) |> dispatch)
                        forEach [0 .. 500 .. earlier] <| fun i ->
                            option {
                                attr.value i
                                textf "%i cal yr BP" i }
                    }
                }
                div {
                    attr.``class`` "select"
                    select {
                        bind.change.int earlier (fun i -> SetDimension (SpatialStatic(ChangeOverTime(i, later), TaxonLevel)) |> dispatch)
                        forEach [later .. 500 .. 12000] <| fun i ->
                            option {
                                attr.value i
                                textf "%i cal yr BP" i }
                    }
                }
            }
    }

let activeLocations model =
    cond model.dimension <| function
    | Temporal (timelines,_) ->
        div {
            attr.``class`` "tags"
            forEach timelines <| fun t ->
                cond (model.timelineList |> List.tryFind(fun t2 -> t2.TimelineId = t)) <| function
                | Some tl ->
                    span {
                        attr.``class`` "tag is-dark"
                        textf "%s (%fN, %fE)" tl.LocationName tl.LatitudeDD tl.LongitudeDD
                    }
                | None -> empty ()
        }
    | SpatialStatic _ -> empty ()

let ebvPage (ebv:EssentialBiodiversityVariable) model dispatch =
    concat {
        div {
            attr.``class`` "content"
            h2 { text ebv.Name }
        }

        div {
            attr.``class`` "columns"
            div {
                attr.``class`` "column"
                section {
                    attr.id "ebv-details"
                    attr.``class`` "content"
                    cond ebv <| function
                    | TaxonDistribution ->
                        div {
                            attr.``class`` "content"
                            h2 { text "Taxon Presence-Absence" }
                            p { text "The presence or absence of a taxon within each 500-year time window." }
                            cond model.dimension <| function
                            | DimensionView.Temporal _ ->
                                cond model.dataSlice <| function
                                | DataIndexed.NoData -> text "Data not loaded."
                                | DataIndexed.IndexedByTimeline data -> Plots.presenceHeatmap data
                                | _ -> text "Error. data not formatted correctly."
                            | DimensionView.SpatialStatic (timeMode,_) ->
                                cond model.dataSlice <| function
                                | DataIndexed.NoData -> text "Data not loaded."
                                | DataIndexed.IndexedByPolygonId data ->
                                    cond (model.geojson |> Map.tryFind "phyto-subzones-simplified") <| function
                                    | Some geojson -> Plots.chloropleth timeMode geojson (DataAccess.indexVariableByLocation {VariableName = "presence"; VariableUnit = "present-absent"} data)
                                    | None -> text "Cannot load cloropleth: geojson base layer not loaded."
                                | _ -> textf "Error. data not formatted correctly. %A" model.dataSlice
                            p { text "The indicator is 1 if the taxon/taxa was present in the window, or 0 if there was a reconstructed absence. Where the underlying proxy data was tending towards absence, the indicator is set as 'borderline' at 0.5. For example, for pollen percentage data the borderline level is set as below 2.5%." }
                            p { text "The confidence of the indicator is qualitative. Taxonomic uncertainty may exist where identified fossil remains may be from more than one taxon, yielding lower confidence." }
                        }
                    | RawAbundanceData ->
                        cond model.dimension <| function
                        | DimensionView.Temporal _ ->
                            cond model.dataSlice <| function
                            | DataIndexed.IndexedByTimeline data -> Plots.slicedRawData data
                            | _ -> text "Error. Data not formatted correctly."
                        | _ -> text "Error. Data not formatted correctly."
                    | Morphology ->
                        concat {
                            h1 { text "Plant traits test plot" }
                            cond model.dimension <| function
                            | DimensionView.Temporal _ ->
                                cond model.dataSlice <| function
                                | DataIndexed.NoData -> text "Data not loaded."
                                | DataIndexed.IndexedByTimeline data -> Plots.traitRadial data
                                | _ -> text "Error. data not formatted correctly."
                            | DimensionView.SpatialStatic (timeMode, _) ->
                                cond model.dataSlice <| function
                                | DataIndexed.NoData -> text "Data not loaded."
                                | DataIndexed.IndexedByPolygonId data ->
                                    cond (model.geojson |> Map.tryFind "phyto-subzones-simplified") <| function
                                    | Some geojson ->
                                        cond model.selectedVariable <| function
                                        | Some v -> Plots.chloropleth timeMode geojson (DataAccess.indexVariableByLocation v data)
                                        | None -> text "Select a dimension to show first."
                                    | None -> text "Cannot load cloropleth: geojson base layer not loaded."
                                | _ -> textf "Error. data not formatted correctly. %A" model.dataSlice
                        }
                    | Movement -> failwith "Not Implemented"
                    | TaxonomicAndPhylogeneticDiversity -> failwith "Not Implemented"
                    | TraitDiversity -> failwith "Not Implemented"
                }
            }
            div {
                attr.``class`` "column"
                div {
                    attr.``class`` "card"
                    header {
                        attr.``class`` "card-header"
                        p {
                            attr.``class`` "card-header-title has-background-white-ter"
                            text "Filter and Slice"
                        }
                    }
                    div {
                        attr.``class`` "card-content"
                        div {
                            attr.``class`` "buttons has-addons"
                            button {
                                on.click (fun _ -> SetDimension (Temporal([],TaxonLevel)) |> dispatch)
                                attr.``class`` (if model.dimension.IsTemporal then "button is-primary is-selected" else "button")
                                text "Temporal indicator"
                            }
                            button {
                                on.click (fun _ -> SetDimension (SpatialStatic(ChangeOverTime(12000, 0), TaxonLevel)) |> dispatch)
                                attr.``class`` (if model.dimension.IsSpatialStatic then "button is-primary is-selected" else "button")
                                text "Geo-temporal indicator"
                            }
                        }
                        label {
                            attr.``class`` "label"
                            text "Showing indicators for..."
                        }
                        div {
                            attr.``class`` "tags"
                            forEach model.filterByTaxa <| fun t ->
                                span {
                                    attr.``class`` "tag is-light"
                                    textf "%s (%s)" t.LatinName t.Rank
                                }
                        }
                        activeLocations model
                        br {}
                        selectTaxaMulti model dispatch
                        br {}
                        cond model.dimension <| function
                        | Temporal _ -> selectTimelineMulti model.timelineList dispatch
                        | SpatialStatic (timeMode,y) ->
                            concat {
                                selectSpatialTime timeMode dispatch
                                cond model.dataSlice <| function
                                | DataIndexed.IndexedByPolygonId m ->
                                    concat {
                                        label {
                                            attr.``class`` "label"
                                            text "Select a dimension to show"
                                        }
                                        div {
                                            attr.``class`` "select control"
                                            select {
                                                attr.``class`` "select"
                                                bind.change.string (if model.selectedVariable.IsSome then model.selectedVariable.Value.VariableName else "") (fun s -> (if s = "" then None else Some s) |> SetVariable |> dispatch)
                                                option {
                                                    attr.disabled "disabled"
                                                    attr.selected "selected"
                                                    attr.value ""
                                                    text "-- Select a variable --"
                                                }
                                                forEach (m |> Seq.collect(fun kv -> kv.Value.Keys)) <| fun v ->
                                                    option {
                                                        attr.name v.VariableName
                                                        attr.value v.VariableName
                                                        text v.VariableName
                                                    }
                                            }
                                        }
                                    }
                                | _ -> empty ()
                            }
                    }
                }
            }
        }
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
        Program.mkProgram (fun _ -> initModel, Cmd.batch [ Cmd.ofMsg LoadTaxonIndex; Cmd.ofMsg LoadTimelineIndex; Cmd.ofMsg (LoadGeoJson "caff"); Cmd.ofMsg (LoadGeoJson "phyto-subzones-simplified") ]) update view
        |> Program.withRouter router
