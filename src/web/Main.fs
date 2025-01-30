module GraphExplorer.Client.Main

open System
open System.Net.Http
open Microsoft.AspNetCore.Components
open Elmish
open Bolero
open Bolero.Html

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

    type SortMode =
        | Northwards

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

        member this.AggregateFn =
            match this with
            | TaxonDistribution -> Seq.max
            | Morphology -> Seq.average
            | Movement -> Seq.max
            | TaxonomicAndPhylogeneticDiversity -> Seq.average
            | TraitDiversity -> Seq.average
            | RawAbundanceData -> Seq.sum

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
        Slug: string
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

        type CommunityType =
            | CommunityOnly
            | CommunityAndTaxa of TaxonIndexItem list
            | TaxaOnly of TaxonIndexItem list

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
                | ModelParts.EssentialBiodiversityVariable.Movement -> "content/indicators/traits/movement-migration.tsv"
                | ModelParts.EssentialBiodiversityVariable.TaxonomicAndPhylogeneticDiversity -> "content/indicators/community-composition/richness.tsv"
                |> fun u ->
                    match isSpatial with
                    | true -> u.Replace(".tsv", "_spatial.tsv")
                    | false -> u
            let! csv = httpClient.GetStringAsync url
            return (DatasetEBV.Parse csv).Rows |> Seq.toList
        }

    let filterByTaxa (filter: TaxonIndex.CommunityType) (data:seq<DatasetEBV.Row>) =
        data
        |> Seq.filter(fun r ->
            match filter with
            | TaxonIndex.CommunityType.TaxaOnly taxa
            | TaxonIndex.CommunityType.CommunityAndTaxa taxa ->
                taxa |> Seq.exists (fun t -> t.LatinName :: t.IncludedLatinNames |> List.contains r.Taxon)
            | TaxonIndex.CommunityType.CommunityOnly ->
                r.Taxon = "community" )

    let dataForTimeline (taxa:TaxonIndex.CommunityType) aggregateFn aggregateCi timeline (data:list<DatasetEBV.Row>) =
        data
        |> Seq.filter(fun r -> r.Location_id = timeline)
        |> filterByTaxa taxa
        |> Seq.groupBy(fun r -> r.Variable, r.Variable_unit)
        |> Seq.map(fun ((v,vu), rows) ->
            { VariableName = v; VariableUnit = vu },
            rows 
            |> Seq.groupBy(fun r -> r.Bin_late)
            |> Seq.map(fun (binLate, rows) ->
                binLate, 
                rows |> Seq.map(fun r -> r.Variable_value) |> aggregateFn,
                rows |> Seq.map(fun r -> r.Variable_confidence_qualitative) |> aggregateCi )
        ) |> Map.ofSeq

    let indexByLocationFor (taxa: TaxonIndex.CommunityType) (data:list<DatasetEBV.Row>) =
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

    let communityMode selectedTaxa (data:DatasetEBV.Row list) =
        let taxa = data |> Seq.map(fun d -> d.Taxon) |> Seq.distinct |> Seq.toList
        if taxa.Length = 1 then
            if taxa.Head = "community" then TaxonIndex.CommunityType.CommunityOnly
            else TaxonIndex.CommunityType.TaxaOnly selectedTaxa
        else
            if taxa |> List.contains "community"
            then TaxonIndex.CommunityType.CommunityAndTaxa selectedTaxa
            else TaxonIndex.CommunityType.TaxaOnly selectedTaxa




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
    | [<EndPoint "/ebv/{category}/{name}">] EssentialBioVariable of category:string * name:string
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
        sortMode: SortMode
        filterByTaxa: DataAccess.TaxonIndex.CommunityType
        sliceAggregateFn: float seq -> float
        sliceAggregateFnCi: string option seq -> string option
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
        filterByTaxa = DataAccess.TaxonIndex.CommunityType.TaxaOnly []
        sortMode = Northwards
        sliceAggregateFn = Seq.sum
        sliceAggregateFnCi = Seq.tryHead >> Option.bind id
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
      Slug = "populations"
      EBVs = [
        TaxonDistribution
      ] }
    { Label = "Species traits"
      Slug = "traits"
      EBVs = [
        Morphology
        Movement
      ] }
    { Label = "Community composition"
      Slug = "community-composition"
      EBVs = [
        TaxonomicAndPhylogeneticDiversity
        // TraitDiversity
      ] }
    { Label = "Underlying data"
      Slug = "underlying-data"
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
    | ToggleTaxonInFilter of string
    | ToggleTimelineInFilter of string
    | SwitchCommunityMode of DataAccess.TaxonIndex.CommunityType
    | SetDimension of DimensionView
    | SetVariable of string option

let update httpClient message model =
    match message with
    | SetPage page ->
        match page with
        | MarkdownPage tags ->
            { model with page = page }, Cmd.ofMsg(LoadMarkdownPage tags)
        | Home -> { model with page = page }, Cmd.ofMsg(LoadMarkdownPage ["index"])
        | EssentialBioVariable (cat, ebv) ->
            match ebv |> EssentialBiodiversityVariable.FromSlug with
            | Some ebv -> { model with page = page; data = None; sliceAggregateFn = ebv.AggregateFn }, Cmd.batch [ Cmd.ofMsg(LoadIndicatorData ebv); Cmd.ofMsg (SetVariable None) ]
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
        let communityMode =
            match model.filterByTaxa with
            | DataAccess.TaxonIndex.CommunityType.TaxaOnly t
            | DataAccess.TaxonIndex.CommunityType.CommunityAndTaxa t -> DataAccess.communityMode t raw
            | DataAccess.TaxonIndex.CommunityType.CommunityOnly -> DataAccess.communityMode [] raw
        { model with data = Some raw; filterByTaxa = communityMode }, Cmd.ofMsg SliceIndicatorData

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
                        DataAccess.dataForTimeline model.filterByTaxa model.sliceAggregateFn model.sliceAggregateFnCi t dataset)
                    |> Map.ofList
                { model with dataSlice = DataIndexed.IndexedByTimeline sliced }, Cmd.none
            | SpatialStatic _ ->
                let sliced = DataAccess.indexByLocationFor model.filterByTaxa dataset
                printfn "Sliced %A" sliced
                { model with dataSlice = DataIndexed.IndexedByPolygonId sliced }, Cmd.none

    | SetDimension dim -> { model with dimension = dim }, Cmd.ofMsg (SetPage model.page) // Causes reload of correct data
    | ChangeFilterRank r -> { model with selectedRankFilter = r}, Cmd.none
    | ToggleTaxonInFilter taxon ->
        let toAdd = model.taxonList |> Map.find model.selectedRankFilter |> Seq.find(fun t -> t.LatinName = taxon)
        let newFilter = 
            match model.filterByTaxa with
            | DataAccess.TaxonIndex.CommunityType.CommunityOnly -> model.filterByTaxa
            | DataAccess.TaxonIndex.CommunityType.CommunityAndTaxa taxa ->
                if taxa |> List.contains toAdd
                then taxa |> List.except [ toAdd ]
                else toAdd :: taxa
                |> DataAccess.TaxonIndex.CommunityType.CommunityAndTaxa
            | DataAccess.TaxonIndex.CommunityType.TaxaOnly taxa ->
                if taxa |> List.contains toAdd
                then taxa |> List.except [ toAdd ]
                else toAdd :: taxa
                |> DataAccess.TaxonIndex.CommunityType.TaxaOnly
        { model with filterByTaxa = newFilter }, Cmd.ofMsg SliceIndicatorData
    | ToggleTimelineInFilter timeline ->
        match model.dimension with
        | Temporal (t,t2) ->
            if t |> Seq.contains timeline
            then { model with dimension = Temporal (t |> List.except [ timeline ], t2) }, Cmd.ofMsg SliceIndicatorData
            else { model with dimension = Temporal (timeline :: t, t2) }, Cmd.ofMsg SliceIndicatorData
        | SpatialStatic _ -> model, Cmd.none
        // | SpatialDynamic (t,s) -> { model with dimension = SpatialDynamic ((timeline :: t),s) }, Cmd.ofMsg SliceIndicatorData
    | SwitchCommunityMode mode ->
        { model with filterByTaxa = mode }, Cmd.ofMsg SliceIndicatorData
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

    let sideMenu' (links:Node) =
            aside {
                attr.``class`` "column sidebar is-narrow"
                nav {
                    attr.``class`` "menu"
                    ul {
                        attr.``class`` "menu-list"
                        links
                    }
                }
            }

    let sideMenu (model: Model) =
        cond model.page <| function
        | Page.Home -> empty ()
        | Page.MarkdownPage _ ->
            sideMenu'
                (forEach model.inpagelinks <| fun l ->
                    li {
                        a {
                            attr.href (router.Link(model.page, l.Tag))
                            l.Name
                        }
                    } )
        | Page.EssentialBioVariable (cat,_) -> 
            cond (ebvIndex |> Seq.tryFind(fun n -> n.Slug = cat)) <| function
            | Some cat ->
                forEach cat.EBVs <| fun ebv ->
                    li {
                        a {
                            attr.``class`` (if model.page = (Page.EssentialBioVariable (cat.Slug,ebv.Slug)) then "is-active" else "")
                            attr.href (router.Link (Page.EssentialBioVariable (cat.Slug,ebv.Slug)))
                            ebv.Name
                        }
                    }
            | None -> empty ()
            |> sideMenu'

    let navbar (currentPage:Page) =
        nav {
            attr.``class`` "navbar navbar-multi-level"
            attr.aria "label" "main navigation"
            div {
                attr.``class`` "navbar-brand"
                a {
                    attr.``class`` "navbar-item"
                    attr.href "/"
                    img {
                        attr.src "images/ahbm-logo.png"
                        attr.alt "Arctic biodiversity map logo"
                        attr.height "28"
                    }
                    text "Arctic Biodiversity Indicators for the Holocene"
                }
                div {
                    attr.``class`` "navbar-item is-hidden-touch"
                    forEach ["About this project", "about"; "The Arctic Holocene Biodiversity Map", "ahbdb"; "Citing data", "citing" ] <| fun (title,slug) ->
                        a {
                            attr.``class`` "navbar-top-list-item"
                            attr.href (router.Link <| Page.MarkdownPage [slug])
                            text title
                        }
                }
                button {
                    attr.``class`` "button navbar-burger"
                    Attr.Make "data-target" "navMenu"
                    span {}
                    span {}
                    span {}
                }
            }
            div {
                attr.``class`` "navbar-menu"
                attr.id "navMenu"
                div {
                    attr.``class`` "navbar-start"
                    forEach ebvIndex <| fun ebvCat ->
                        cond ebvCat.EBVs.IsEmpty <| function
                        | true -> empty ()
                        | false ->
                            a {
                                attr.``class`` (if (match currentPage with | Page.EssentialBioVariable (c,_) -> c = ebvCat.Slug | _ -> false) then "navbar-item is-active" else "navbar-item")
                                attr.href (router.Link (Page.EssentialBioVariable (ebvCat.Slug,ebvCat.EBVs.Head.Slug) ))
                                ebvCat.Label
                            }
                }
            }
        }

    let main (menuHole: Node) (sidebarHole: Node) (title:Node) (sideMenuHole: Node) (bodyHole: Node) (errorHole: Node) =
        concat {
            menuHole
            div {
                attr.``class`` "level head-row"
                h1 {
                    attr.``class`` "head-row-heading"
                    title
                }
            }
            div {
                attr.``class`` "columns"
                sidebarHole
                div {
                    attr.``class`` "column"
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
    open Microsoft.JSInterop

    Defaults.DefaultTemplate <- ChartTemplates.lightMirrored
    Defaults.DefaultHeight <- 300

    let sortData sortMode (data:Map<TimelineWithLocation,'data>) =
        match sortMode with
        | Northwards -> data |> Seq.sortBy(fun kv -> - kv.Key.LatitudeDD)

    let slicedRawData sortMode xAxisRange (model:Map<TimelineWithLocation, Map<DataVariable,(int * float * option<string>) seq>>) =
        cond model.IsEmpty <| function
        | true -> div { attr.``class`` "notification"; text "Select one or more timelines to display data" }
        | false ->
            model
            |> sortData sortMode
            |> Seq.collect(fun kv ->
                kv.Value
                |> Seq.map(fun kv2 ->
                    Chart.Line(kv2.Value |> Seq.map(fun (_,i,_) -> i), kv2.Value |> Seq.map(fun (i,_,_) -> i), Name = kv.Key.LocationName, ShowMarkers = true, Orientation = StyleParam.Orientation.Vertical)
                    |> Chart.withYAxisStyle(MinMax = (12000, 0))
                    |> fun c -> if Option.isSome xAxisRange then c |> Chart.withXAxisStyle(MinMax = xAxisRange.Value) else c
                    |> Chart.withXAxisStyle(TitleText = sprintf "%s (%s)" kv2.Key.VariableName kv2.Key.VariableUnit)
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
    let presenceHeatmap sortMode (siteData: Map<TimelineWithLocation, Map<DataVariable,(int * float * option<string>) seq>>) = // (siteData: (string * float list) list) =
        printfn "Data %A" siteData
        let v = { VariableName = "presence"; VariableUnit = "present-absent" }
        if siteData.Count = 0 then div { attr.``class`` "notification"; text "Select one or more timelines to display data" }
        else
            let siteData = siteData |> sortData sortMode
            let annotation =
                siteData |> Seq.map(fun kv -> kv.Value) |> Seq.map(fun p ->
                    p |> Map.tryFind v
                    |> Option.defaultValue []
                    |> Seq.map(fun (_,p2,confidence) ->
                        match p2 with
                        | 1. -> "P" | 0.5 -> "M" | 0. -> "A" | f when Double.IsNaN f -> "U" | _ -> ""))
            let timeBinStarts = (siteData |> Seq.map(fun kv -> kv.Value) |> Seq.head) |> Map.tryFind v |> Option.defaultValue [] |> Seq.map (fun (i,_,_) -> i)
            let zData =
                siteData |> Seq.map(fun kv -> kv.Value) |> Seq.map(fun d -> d |> Map.tryFind v |> Option.defaultValue [] |> Seq.map (fun (_,i,_) -> i))

            let scale = 
                StyleParam.Colorscale.Custom [
                    0., Color.fromKeyword ColorKeyword.DarkBlue
                    0.5, Color.fromKeyword ColorKeyword.Teal
                    1., Color.fromKeyword ColorKeyword.Yellow
                ]

            //

            Chart.AnnotatedHeatmap(
                zData = zData,
                annotationText = annotation,
                Y = (siteData |> Seq.map(fun k -> k.Key.LocationName)),
                X = timeBinStarts,
                ShowScale = false,
                ColorScale = scale,
                ReverseYAxis = true
            )
            |> Chart.withXAxisStyle (TitleText = "1,000 Calendar years before present (cal yr BP)")
            |> Chart.withYAxisStyle (TitleText = "Location name")
            |> GenericChart.toChartHTML
            |> rawHtml


    let traitRadial (siteData: Map<TimelineWithLocation, Map<DataVariable,(int * float * option<string>) seq>>) =
        cond (siteData.Count = 0) <| function
        | true -> div { attr.``class`` "notification"; text "Select one or more timelines to display data" }
        | false ->
            let theta = [ "Leaf area"; "Leaf nitrogen"; "Leaf mass"; "Plant height"; "Diaspore mass"; "Stem density" ]
            
            let maxValues =
                siteData
                |> Seq.collect(fun kv ->
                    kv.Value |> Seq.collect(fun kv2 -> kv2.Value |> Seq.map(fun (y,i,v) -> kv2.Key.VariableName, i)) )
                |> Seq.groupBy fst
                |> Seq.map(fun (g,vals) -> g, vals |> Seq.map(fun v -> snd v) |> Seq.max )
                |> Map.ofSeq

            let polarCharts =
                siteData
                |> Seq.map(fun kv ->
                    
                    let radial =
                        kv.Value
                        |> Seq.collect(fun kv2 -> kv2.Value |> Seq.map(fun (y,i,v) -> {| Name = kv2.Key.VariableName; Year = y; V = i; Conf = v |}))
                        |> Seq.groupBy(fun v -> v.Year)
                        |> Seq.map(fun (y,vals) ->
                            let radial, label, conf =
                                vals |> Seq.map(fun v ->
                                    let max = Map.find v.Name maxValues
                                    v.V / max, sprintf "Real value = %f" v.V, v.Conf) |> Seq.toList |> List.unzip3
                            Chart.LinePolar(r = radial, theta = theta, MultiText = label, Name = sprintf "%i cal yr BP" y)
                            )
                        |> Chart.combine
                    radial

                )
            polarCharts 
            |> Chart.Grid(2, 2)
            |> Chart.withSize(600, 600)
            |> GenericChart.toChartHTML
            |> rawHtml

    let responsiveConfig = Config.init (Responsive = true, DisplayModeBar = false)

    let chloropleth' locations z unitLabel geoJson =
        Chart.ChoroplethMap(
            locations = locations, z = z,
            LocationMode = StyleParam.LocationFormat.GeoJson_Id,
            GeoJson = geoJson,
            FeatureIdKey = "properties.id",
            ColorBar = ColorBar.init(Title = Title.init(unitLabel))
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
    let chloropleth timeMode geoJson unitLabel (data:Map<string,seq<int * float * Option<string>>>) =
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

            chloropleth' locations z unitLabel geoJson
            |> GenericChart.toChartHTML
            |> rawHtml

        | TimeMode.IndividualTimesteps ->

            let steps =
                data |> Seq.tryHead
                |> Option.map(fun s -> s.Value |> Seq.map(fun (y,_,_) -> y) |> Seq.sort |> Seq.toList)
                |> Option.defaultValue [ 0 .. 500 .. 12000 ]
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
                    chloropleth' locations z unitLabel geoJson
                ) |> Chart.combine
            printfn "C"
            map
            |> Chart.withSlider slider
            |> GenericChart.toChartHTML
            |> rawHtml

    let simpleGeo caffGeoJson (locations:list<TimelineWithLocation>) (selectedLocations:list<string>) dispatch =

        let lats, lons, names =
            locations
            |> List.map(fun l -> l.LatitudeDD, l.LongitudeDD, l.LocationName)
            |> List.unzip3

        match caffGeoJson with
        | Some json ->
            Chart.PointGeo(
                latitudes = lats, longitudes = lons,
                MultiText = names,
                LocationMode = StyleParam.LocationFormat.GeoJson_Id,
                GeoJson = json,
                FeatureIdKey = "properties.name"
            )
        | None ->
            Chart.PointGeo(
                latitudes = lats, longitudes = lons,
                MultiText = names,
                LocationMode = StyleParam.LocationFormat.CountryNames
            )
        |> Chart.withSize(350, 200)
        |> Chart.withConfig(Config.init(
            Responsive = true,
            Logging = 2,
            DisplayModeBar = true,
            Displaylogo = false,
            ScrollZoom = StyleParam.ScrollZoom.All,
            ModeBarButtons = [[ StyleParam.ModeBarButton.Pan2d; StyleParam.ModeBarButton.Lasso2d; StyleParam.ModeBarButton.ResetGeo ]]))
        |> Chart.withMarginSize(0,0, 0,0, true, true)
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
    cond model.filterByTaxa <| function
    | DataAccess.TaxonIndex.CommunityType.CommunityOnly -> empty ()
    | DataAccess.TaxonIndex.CommunityAndTaxa _
    | DataAccess.TaxonIndex.TaxaOnly _ ->
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
                        bind.change.string "" (fun s -> ToggleTaxonInFilter s |> dispatch)
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

let selectTimelineMulti (timelineList: ModelParts.TimelineWithLocation list) selectedTimelines geojson dispatch =
    concat {
        div {
            attr.``class`` "field"
            label {
                attr.``class`` "label"
                text "Add data from another location"
            }
            div {
                // on.event "plotly_selected" (fun e -> printfn "Selected %A" e; ())
                // on.event "plotly_click" (fun e -> printfn "Clicked %A" e; ())
                // on.task.event "click" (fun e -> task { printfn "Clicked 2 %A" e })
                on.task.event "plotly_click" (fun e -> task { printfn "Clicked 2 %A" e })
                Plots.simpleGeo geojson timelineList selectedTimelines dispatch
            }
            div {
                attr.``class`` "select control"
                select {
                    attr.``class`` "select"
                    bind.change.string "" (fun s -> ToggleTimelineInFilter s |> dispatch)
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
                on.click (fun _ -> SetDimension (SpatialStatic(ChangeOverTime(5000, 2000), TaxonLevel)) |> dispatch)
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

let activeLocations model dispatch =
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
                        button {
                            attr.``class`` "delete is-small"
                            on.click (fun _ -> ToggleTimelineInFilter tl.TimelineId |> dispatch)
                        }
                    }
                | None -> empty ()
        }
    | SpatialStatic _ -> empty ()

module Filters =

    let spatialTemporal model dispatch =
        div {
            attr.``class`` "buttons has-addons"
            button {
                on.click (fun _ -> SetDimension (Temporal([],TaxonLevel)) |> dispatch)
                attr.``class`` (if model.dimension.IsTemporal then "button is-primary is-selected" else "button")
                text "Temporal indicator"
            }
            button {
                on.click (fun _ -> SetDimension (SpatialStatic(ChangeOverTime(5000, 2000), TaxonLevel)) |> dispatch)
                attr.``class`` (if model.dimension.IsSpatialStatic then "button is-primary is-selected" else "button")
                text "Geo-temporal indicator"
            }
        }

    let communityLevelTag =
        span {
            attr.``class`` "tag is-light"
            textf "Community-level metrics"
        }

    let activeTaxonFilters model dispatch =
        concat {
            label {
                attr.``class`` "label"
                text "Showing indicators for..."
            }
            div {
                attr.``class`` "tags"
                cond <| model.filterByTaxa <| function
                | DataAccess.TaxonIndex.CommunityType.CommunityOnly -> communityLevelTag
                | DataAccess.TaxonIndex.CommunityType.CommunityAndTaxa t
                | DataAccess.TaxonIndex.CommunityType.TaxaOnly t ->
                    concat {
                        cond (t.IsEmpty && model.filterByTaxa.IsCommunityAndTaxa) <| function
                        | true -> communityLevelTag
                        | false -> empty ()
                        forEach t <| fun t ->
                            span {
                                attr.``class`` "tag is-light"
                                textf "%s (%s)" t.LatinName t.Rank
                                button {
                                    attr.``class`` "delete is-small"
                                    on.click(fun _ -> ToggleTaxonInFilter t.LatinName |> dispatch)
                                }
                            }
                    }
            }
        }

    let filterSpatialStatic timeMode model dispatch =
        concat {
            selectSpatialTime timeMode dispatch
            cond model.dataSlice <| function
            | DataIndexed.IndexedByPolygonId m ->
                cond ((m |> Seq.collect(fun kv -> kv.Value.Keys) |> Seq.distinct |> Seq.length = 1)) <| function
                | true -> empty ()
                | false ->
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
                                forEach (m |> Seq.collect(fun kv -> kv.Value.Keys) |> Seq.distinct) <| fun v ->
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


let ebvPage' currentPage subtitle (graph: Node) legendText (filterCard: Node) (methodText: Node) =
    concat {
        div {
            attr.``class`` "columns"
            div {
                attr.``class`` "column"
                p {
                    attr.``class`` "subtitle"
                    textf "%s " subtitle
                    a {
                        attr.href (router.Link(currentPage, "method"))
                        text "[Full Description]"
                    }
                }
                graph
                em { text legendText }
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
                        filterCard
                    }
                }
            }
        }
        div {
            attr.``class`` "columns"
            div {
                attr.``class`` "column content is-small dotted-top-border"
                h3 { attr.id "method"; text "Methods" }
                methodText
            }
        }
    }

let defaultFilters model dispatch =
    concat {
        Filters.spatialTemporal model dispatch
        Filters.activeTaxonFilters model dispatch
        activeLocations model dispatch
        br {}
        selectTaxaMulti model dispatch
        br {}
        cond model.dimension <| function
        | Temporal (selected,_) ->
            selectTimelineMulti model.timelineList selected (model.geojson |> Map.tryFind "caff") dispatch
        | SpatialStatic (timeMode,y) -> Filters.filterSpatialStatic timeMode model dispatch
    }

let ebvPage (ebv:EssentialBiodiversityVariable) model dispatch =
    cond ebv <| function
    | TaxonDistribution ->
        ebvPage'
            model.page
            "The presence/absence of specific taxa"
            (cond model.dimension <| function
            | DimensionView.Temporal _ ->
                cond model.dataSlice <| function
                | DataIndexed.NoData -> div { attr.``class`` "notification is-warning"; text "No data is loaded." }
                | DataIndexed.IndexedByTimeline data -> Plots.presenceHeatmap model.sortMode data
                | _ -> div { attr.``class`` "notification is-danger"; text "Data not formatted correctly" }
            | DimensionView.SpatialStatic (timeMode,_) ->
                cond model.dataSlice <| function
                | DataIndexed.NoData -> div { attr.``class`` "notification is-warning"; text "No data is loaded." }
                | DataIndexed.IndexedByPolygonId data ->
                    cond (model.geojson |> Map.tryFind "phyto-subzones-simplified") <| function
                    | Some geojson -> Plots.chloropleth timeMode geojson "Presence/absence" (DataAccess.indexVariableByLocation {VariableName = "presence"; VariableUnit = "present-absent"} data)
                    | None -> text "Cannot load cloropleth: geojson base layer not loaded."
                | _ -> textf "Error. data not formatted correctly. %A" model.dataSlice )
            "1 = present; 0.5 = borderline present; 0 = absent; NA = unknown"
            (defaultFilters model dispatch)
            (concat {
                p { text "The presence or absence of a taxon within each 500-year time window." }
                p { text "The indicator is 1 if the taxon/taxa was present in the window, or 0 if there was a reconstructed absence. Where the underlying proxy data was tending towards absence, the indicator is set as 'borderline' at 0.5. For example, for pollen percentage data the borderline level is set as below 2.5%." }
                p { text "The confidence of the indicator is qualitative. Taxonomic uncertainty may exist where identified fossil remains may be from more than one taxon, yielding lower confidence." }
            })
    | RawAbundanceData ->
        ebvPage'
            model.page
            "Raw datasets in original units"
            (cond model.dimension <| function
                    | DimensionView.Temporal _ ->
                        cond model.dataSlice <| function
                        | DataIndexed.IndexedByTimeline data -> Plots.slicedRawData model.sortMode (Some (0., 50.)) data
                        | _ -> text "Error. Data not formatted correctly."
                    | _ -> text "Error. Data not formatted correctly.")
            "Placeholder legend"
            (defaultFilters model dispatch)
            (concat {
                p { text "Placeholder help text." }
            })
    | Morphology ->
        ebvPage'
            model.page
            "Key morphological traits for plants"
            (cond model.dimension <| function
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
                        | Some v -> Plots.chloropleth timeMode geojson (sprintf "%s (%s)" v.VariableName v.VariableUnit) (DataAccess.indexVariableByLocation v data)
                        | None -> text "Select a dimension to show first."
                    | None -> text "Cannot load cloropleth: geojson base layer not loaded."
                | _ -> textf "Error. data not formatted correctly. %A" model.dataSlice )
            "Placeholder legend"
            (defaultFilters model dispatch)
            (concat {
                p { text "Placeholder help text." }
            })
    | Movement ->
        ebvPage'
            model.page
            "The earliest known date of occurrence"
            (cond model.dimension <| function
            | DimensionView.Temporal _ ->
                cond model.dataSlice <| function
                | DataIndexed.NoData -> div { attr.``class`` "notification is-warning"; text "No data is loaded." }
                | DataIndexed.IndexedByTimeline data -> Plots.slicedRawData model.sortMode None data
                | _ -> div { attr.``class`` "notification is-danger"; text "Data not formatted correctly" }
            | DimensionView.SpatialStatic (timeMode,_) ->
                cond model.dataSlice <| function
                | DataIndexed.NoData -> div { attr.``class`` "notification is-warning"; text "No data is loaded." }
                | DataIndexed.IndexedByPolygonId data ->
                    cond (model.geojson |> Map.tryFind "phyto-subzones-simplified") <| function
                    | Some geojson -> Plots.chloropleth timeMode geojson "Earliest occurrence (cal yr BP)" (DataAccess.indexVariableByLocation {VariableName = "earliest_occurrence_date"; VariableUnit = "cal yr BP"} data)
                    | None -> text "Cannot load cloropleth: geojson base layer not loaded."
                | _ -> textf "Error. data not formatted correctly. %A" model.dataSlice )
            "Calibrated years before present"
            (defaultFilters model dispatch)
            (concat {
                p { text "Placeholder method text" }
            })
    | TaxonomicAndPhylogeneticDiversity ->
        ebvPage'
            model.page
            "Taxonomic richness"
            (cond model.dimension <| function
            | DimensionView.Temporal _ ->
                cond model.dataSlice <| function
                | DataIndexed.NoData -> div { attr.``class`` "notification is-warning"; text "No data is loaded." }
                | DataIndexed.IndexedByTimeline data -> Plots.slicedRawData model.sortMode None data
                | _ -> div { attr.``class`` "notification is-danger"; text "Data not formatted correctly" }
            | DimensionView.SpatialStatic (timeMode,_) ->
                cond model.dataSlice <| function
                | DataIndexed.NoData -> div { attr.``class`` "notification is-warning"; text "No data is loaded." }
                | DataIndexed.IndexedByPolygonId data ->
                    cond (model.geojson |> Map.tryFind "phyto-subzones-simplified") <| function
                    | Some geojson ->
                        cond model.selectedVariable <| function
                        | Some v -> Plots.chloropleth timeMode geojson (sprintf "%s (%s)" v.VariableName v.VariableUnit) (DataAccess.indexVariableByLocation v data)
                        | None -> text "Select a dimension to show first."
                    | None -> text "Cannot load cloropleth: geojson base layer not loaded."
                | _ -> textf "Error. data not formatted correctly. %A" model.dataSlice )
            "placeholder"
            (defaultFilters model dispatch)
            (concat {
                p { text "Placeholder - minimum taxonomic richness." }
            })
    | TraitDiversity -> failwith "Not Implemented"

let pageTitle model =
    cond model.page <| function
    | Page.Home -> text "Home"
    | Page.EssentialBioVariable (cat, ebv) ->
        EssentialBiodiversityVariable.FromSlug ebv
        |> Option.map(fun e -> e.Name)
        |> Option.bind(fun e -> ebvIndex |> Seq.tryFind(fun c -> c.Slug = cat) |> Option.map(fun c -> sprintf "%s - %s" c.Label e))
        |> Option.defaultValue "EBV"
        |> text
    | Page.MarkdownPage md -> text (md |> String.concat " - ")

let view model dispatch =
    View.main
        (View.navbar model.page)
        (View.sideMenu model)
        (pageTitle model)
        (empty ())
        (
            cond model.page <| function
            | Home -> View.markdownPage model dispatch
            | MarkdownPage _ -> View.markdownPage model dispatch
            | EssentialBioVariable (cat, ebv) ->
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
