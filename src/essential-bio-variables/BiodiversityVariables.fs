module BiodiversityVariables

open BiodiversityCoder.Core
open BiodiversityCoder.Core.FieldDataTypes

// What are the biodiversity variables of interest?
// Per-taxon probability of occurrence per 500-year interval

/// Common time-bins to use across EBVs.
let timeBins =
    [ 0<OldDate.calYearBP> .. 500<OldDate.calYearBP> .. 12500<OldDate.calYearBP> ]
    |> List.pairwise

/// Identify time-series that have raw datasets
/// associated with them.
let rawDataByTimeSeriesId graph =
    result {
        let! timelines = DateHarmonisation.readAllTimelineAtoms graph

        let withRawData =
            timelines
            |> List.collect(fun (a:Graph.Atom<GraphStructure.Node, GraphStructure.Relation>) -> 
                (snd a)
                |> List.choose(fun (_,sinkId,_,conn) ->
                    match conn with
                    | GraphStructure.Relation.Exposure r ->
                        match r with
                        | Exposure.ExposureRelation.HasRawData ->
                            Some (a, sinkId)
                        | _ -> None
                    | _ -> None ))
            |> List.choose(fun (a,rdId) ->
                Storage.loadAtom graph.Directory (typeof<Exposure.ExposureNode>.Name) rdId
                |> Result.toOption
                |> Option.bind(fun rawNode ->
                    match rawNode |> fst |> snd with
                    | GraphStructure.Node.DatasetNode d ->
                        match d with
                        | Datasets.DatasetNode.Digitised c -> Some (a ,c)
                    | _ -> None ))

        printfn "Found %A series with raw data" withRawData.Length

        let withSpatial =
            withRawData
            |> List.map(fun a ->
                
                let location =
                    a |> fst |> snd
                    |> List.tryPick(fun (_,sinkId,_,conn) ->
                        match conn with
                        | GraphStructure.Relation.Exposure r ->
                            match r with
                            | Exposure.ExposureRelation.IsLocatedAt ->
                                Some sinkId
                            | _ -> None
                        | _ -> None )
                
                match location with
                | None -> a, None
                | Some lKey ->

                    let spatialContext =
                        lKey
                        |> Storage.loadAtom graph.Directory (typeof<Exposure.ExposureNode>.Name)
                        |> Result.toOption
                        |> Option.bind(fun locAtom ->
                            match locAtom |> fst |> snd with
                            | GraphStructure.Node.PopulationNode e ->
                                match e with
                                | GraphStructure.ContextNode c -> Some c
                                | _ -> None
                            | _ -> None
                        )
                    a, spatialContext
            )


        // Get individual dates.
        // Follow them to the calibration node.
        // Assume age-depth model based for now.
        let withCalibrations =
            withSpatial
            |> List.choose(fun (a,location) ->
                
                let dates =
                    a |> fst |> snd
                    |> List.choose(fun (_,sinkId,_,conn) ->
                        match conn with
                        | GraphStructure.Relation.Exposure r ->
                            match r with
                            | Exposure.ExposureRelation.ConstructedWithDate ->
                                Some sinkId
                            | _ -> None
                        | _ -> None )
                
                dates
                |> Storage.loadAtoms graph.Directory (typeof<Exposure.ExposureNode>.Name)
                |> Result.toOption
                |> Option.bind(fun dateNodes ->
                
                    printfn "Rels are %A" (dateNodes |> Seq.map snd |> Seq.toList)

                    let calibrations =
                        dateNodes
                        |> List.collect(fun (_,rels) ->
                            rels
                            |> List.choose(fun (_,sinkId,_,conn) ->
                            match conn with
                            | GraphStructure.Relation.Exposure r ->
                                match r with
                                | Exposure.ExposureRelation.UsedInCalibration ->
                                    Some sinkId
                                | _ -> None
                            | _ -> None ))
                        |> List.distinct

                    match calibrations.Length with
                    | 0 ->
                        printfn "No calibration found."
                        None
                    | 1 ->
                        Storage.loadAtom graph.Directory (typeof<Exposure.ExposureNode>.Name) calibrations.Head
                        |> Result.toOption
                        |> Option.bind(fun n ->
                            match n |> fst |> snd with
                            | GraphStructure.Node.ExposureNode e ->
                                match e with
                                | Exposure.ExposureNode.DateCalibrationInstanceNode c -> Some (fst a |> fst |> fst, snd a, c, location)
                                | _ -> None
                            | _ -> None
                            )
                    | _ ->
                        printfn "Multiple calibrations existed for a timeline. Skipping."
                        None
                    
                    )
            )

        return withCalibrations
    }

/// Calculate the per-taxon probability of occurrence
/// per time-bin. 
/// - Initially work with genera.
/// - Collapse data to presence-only.
/// - Taxonomic uncertainty - work off 'real' taxa rather than morphotypes.
/// - Temporal uncertainty - assume intersection of calibrated bound 
let perTaxonProbabilityOfOccurrence =

    // Likelihood of presence within a time-period is:
    // - probability of date overlap with time-period.
    // - 

    ()

/// Lookup the age of the depth level in an age-depth
/// model, if one is provided.
let applyAgeDepthModelToData (dataset: Datasets.DigitisedDataset) (cal:Exposure.Reanalysis.DateCalibrationNode) =

    cal.AgeDepthModel
    |> Option.bind(fun ageDepth ->

        let ageDepth =
            ageDepth
            |> List.sortBy(fun d -> d.Depth)
        
        let morphotypes = dataset.DataTable.Morphotypes()

        let indexType, data = dataset.DataTable.Depths()
        match indexType with
        | Datasets.DataTable.IndexUnit.Depths ->
           
           data
           |> Seq.choose(fun kv ->
                
                let nearestDepth = ageDepth |> Seq.tryFind(fun d -> d.Depth >= (kv.Key * 1.<StratigraphicSequence.cm>))
                nearestDepth
                |> Option.map(fun newDepth -> newDepth, kv.Value |> Seq.zip morphotypes |> Map.ofSeq)
            )
            |> Map.ofSeq
            |> Some

        | Datasets.DataTable.IndexUnit.Ages unit ->
            printfn "Skipping age-indexed dataset, as no lookup function configured."
            None
    )

open Population.BioticProxies

let toMorphotypeName (node: Population.BioticProxies.BioticProxyNode) =
    match node with
    | BioticProxyNode.AncientDNA a -> a.Value
    | BioticProxyNode.Morphotype m ->
        match m with
        | Morphotype.Microfossil (_,n) -> n.Value
        | Morphotype.Macrofossil (_,n) -> n.Value
        | Morphotype.Megafossil(_, morphotypeName) -> morphotypeName.Value
    | BioticProxyNode.ContemporaneousWholeOrganism(taxon) -> taxon.Value


module TaxonomyLookup =

    let toTaxonomyNode node =
        match node with
        | GraphStructure.PopulationNode p ->
            match p with
            | GraphStructure.TaxonomyNode t -> Some t
            | _ -> None
        | _ -> None

    let rec crawlTaxonomy' directory taxonAtom tree =
        let isA =
            taxonAtom |> snd
            |> Seq.tryPick(fun (_,sink,_,rel) ->
                match rel with
                | GraphStructure.Relation.Population p ->
                    match p with
                    | Population.PopulationRelation.IsA -> Some sink
                    | _ -> None
                | _ -> None
                )
            |> Option.map (Storage.loadAtom directory "PopulationNode")
        match isA with
        | Some is -> is |> Result.bind(fun is -> crawlTaxonomy' directory is ((is |> fst |> snd |> toTaxonomyNode) :: tree))
        | None -> Ok tree

    let crawlTaxonomy directory taxon =
        crawlTaxonomy' directory taxon [ taxon |> fst |> snd |> toTaxonomyNode ]

    let taxonName = function
        | Population.Taxonomy.Life -> "Life", "Life"
        | Population.Taxonomy.Family f -> f.Value, "Family"
        | Population.Taxonomy.Genus g -> g.Value, "Genus"
        | Population.Taxonomy.Species (g,s,a) -> sprintf "%s %s %s" g.Value s.Value a.Value, "Species"
        | Population.Taxonomy.Subspecies (g,s, ss,a) -> sprintf "%s %s ssp. %s %s" g.Value s.Value ss.Value a.Value, "Subspecies"
        | Population.Taxonomy.Variety (g,s, ss,a) -> sprintf "%s %s var. %s %s" g.Value s.Value ss.Value a.Value, "Variety"
        | Population.Taxonomy.Clade g -> g.Value, "Clade"
        | Population.Taxonomy.Class g -> g.Value, "Class"
        | Population.Taxonomy.Tribe g -> g.Value, "Tribe"
        | Population.Taxonomy.Kingdom g -> g.Value, "Kingdom"
        | Population.Taxonomy.Order g -> g.Value, "Order"
        | Population.Taxonomy.Phylum g -> g.Value, "Phylum"
        | Population.Taxonomy.Subfamily g -> g.Value, "Subfamily"
        | Population.Taxonomy.Subgenus g -> g.Value, "Subgenus"
        | Population.Taxonomy.Subtribe g -> g.Value, "Subtribe"

    /// Given the text representation of the morphotypes
    /// in the dataframe, link this to the morphotypes used
    /// in the biotic proxy nodes. Then, follow to the 'real'
    /// taxa identities and attach to the result.
    let proxiedTaxaLookup timelineId (graph: Storage.FileBasedGraph<GraphStructure.Node,GraphStructure.Relation>) =
        result {
            let! timeline = Storage.loadAtom graph.Directory (typeof<Exposure.ExposureNode>.Name) timelineId
            
            let! proxyHyperedges =
                timeline |> snd
                |> List.choose(fun (_,sinkId,_,conn) ->
                    match conn with
                    | GraphStructure.Relation.Exposure r ->
                        match r with
                        | Exposure.ExposureRelation.HasProxyInfo ->
                            Some sinkId
                        | _ -> None
                    | _ -> None )
                |> Storage.loadAtoms graph.Directory (typeof<GraphStructure.PopulationNode>.Name)
            
            // For each hyperedge, figure out its morphotype <-> real taxon
            let taxonLookup =
                proxyHyperedges
                |> List.choose(fun (_,rels) ->

                    let morphotype =
                        rels |> List.tryPick(fun (_,sinkId,_,conn) ->
                            match conn with
                            | GraphStructure.Relation.Population r ->
                                match r with
                                | Population.PopulationRelation.InferredFrom ->
                                    Some sinkId
                                | _ -> None
                            | _ -> None
                        )
                        |> Option.bind (fun x -> x |> Storage.loadAtom graph.Directory (typeof<GraphStructure.PopulationNode>.Name) |> Result.toOption)
                        |> Option.bind(fun atom ->
                            match atom |> fst |> snd with
                            | GraphStructure.Node.PopulationNode p ->
                                match p with
                                | GraphStructure.BioticProxyNode b -> Some b
                                | _ -> None
                            | _ -> None
                            )

                    let taxa =
                        rels |> List.choose(fun (_,sinkId,_,conn) ->
                            match conn with
                            | GraphStructure.Relation.Population r ->
                                match r with
                                | Population.PopulationRelation.InferredAs ->
                                    Some sinkId
                                | _ -> None
                            | _ -> None
                        )
                        |> Storage.loadAtoms graph.Directory (typeof<GraphStructure.PopulationNode>.Name)
                        |> Result.defaultValue []
                        |> List.choose(fun atom ->
                                match atom |> fst |> snd with
                                | GraphStructure.Node.PopulationNode p ->
                                    match p with
                                    | GraphStructure.TaxonomyNode b ->
                                        crawlTaxonomy graph.Directory atom |> Result.toOption |> Option.map(fun t -> t |> List.choose id)
                                    | _ -> None
                                | _ -> None )

                    match morphotype, taxa with
                    | Some m, t when t.Length > 0 -> Some (toMorphotypeName m, t)
                    | _ -> None
                )

            return taxonLookup |> Map.ofList
        }
        
    let applyRealTaxaToDataMorphotypes (lookup:Map<string,Population.Taxonomy.TaxonNode>) (morphotypes:string list) =
        morphotypes |> List.map(fun name -> Map.tryFind name lookup )



module Traits =

    open FSharp.Data

    type PlantTraits = CsvProvider<"../../data-third-party/traits/arctic_traits.tsv">

    let weightedMedian (values: (float * float) list) =
        let sortedValues = List.sortBy fst values
        let totalWeight = List.sumBy snd sortedValues
        let cumulativeWeight = 
            sortedValues 
            |> List.scan (fun acc (value, weight) -> acc + weight) 0.0
            |> List.tail
        let medianIndex = cumulativeWeight |> List.findIndex (fun w -> w >= totalWeight / 2.0)
        fst sortedValues.[medianIndex]

    let weightedMean valuesWeights =
        let values, weights = List.unzip valuesWeights
        let totalWeight = List.sum weights
        let weightedSum = List.sum (List.map2 (*) values weights)
        weightedSum / float totalWeight

    let pooledStandardDeviation (datasetInfo: (float * float * int * float) list) =
        let overallVariance = 
            (datasetInfo |> List.sumBy (fun (mean, stdDev, n, weight) -> (float(n) - 1.0) * (stdDev ** 2.0) * weight))
                / (datasetInfo |> List.sumBy (fun (_, _, n, weight) -> weight * float(n - 1)))
        sqrt overallVariance


module DataFiles =

    open FSharp.Data

    type BiodiversityVariableFile = CsvProvider<"samples/biodiversity-variable.tsv">
    type TaxonIndex = CsvProvider<"samples/taxon-index.tsv">
    type TimelineIndex = CsvProvider<"samples/timeline-index.tsv">

module Conversions =

    let dmsToDecimalDegrees (degrees: float) (minutes: float) (seconds: float) : float =
        degrees + (minutes / 60.0) + (seconds / 3600.0)

    let locationToDecimalDegrees (context:Population.Context.ContextNode option) =
        context |> Option.map(fun l ->
            match l.SamplingLocation with
            | Geography.SamplingLocation.Site (lat,lon) -> lat.Value, lon.Value, "point"
            | Geography.SamplingLocation.SiteDMS coord ->

                let regex = """([0-9]{2})°([0-9]{2})'([0-9]{2})"N,([0-9]{2})°([0-9]{2})'([0-9]{2})"([EW]{1})"""
                let m = System.Text.RegularExpressions.Regex.Match(coord.Value, regex)
                let latDD = dmsToDecimalDegrees (int m.Groups.[1].Value) (int m.Groups.[2].Value) (int m.Groups.[3].Value) * 1.<Geography.DD>
                let lonDD = dmsToDecimalDegrees (int m.Groups.[4].Value) (int m.Groups.[5].Value) (int m.Groups.[6].Value) * 1.<Geography.DD>
                if m.Groups.[7].Value = "W" then latDD, -lonDD, "point" else latDD, lonDD, "point"
            | _ -> nan * 1.<Geography.DD>, nan * 1.<Geography.DD>, "other-unprocessed"
        ) |> Option.defaultValue (nan * 1.<Geography.DD>, nan * 1.<Geography.DD>, "not-specified")


module Spatial =

    open MaxRev.Gdal.Core
    open OSGeo.OGR

    /// Test if a point intersects a phytogeographic realm x subzone union,
    /// and return the ID if there is an intersect.
    let intersectsRegionFn () =
        
        GdalBase.ConfigureAll()
        Ogr.RegisterAll()

        let file = Ogr.Open("/Users/andrewmartin/Documents/GitHub Projects/arctic-biodiversity-indicators/data-third-party/cavm/phyto-subzones.geojson", 0)
        let layer = file.GetLayerByIndex(0)
        let crs = layer.GetSpatialRef()

        let rec tryFindIntersectId (layer:Layer) (point:Geometry) =
            let next = layer.GetNextFeature()
            if isNull next then None
            else
                let nextGeo = next.GetGeometryRef()
                if point.Intersects(next.GetGeometryRef())
                then Some (next.GetFieldAsString("id"))
                else tryFindIntersectId layer point
        
        fun lat lon ->
            let coord = sprintf "POINT (%f %f)" lon lat
            let testPoint = Ogr.CreateGeometryFromWkt(ref coord, crs)
            layer.ResetReading()
            tryFindIntersectId layer testPoint


let calculateBiodiversityVariables graph =
    result {

        let! series = rawDataByTimeSeriesId graph

        printfn "Found %i series with age-depth models and raw data." series.Length

        let newAges =
            series |> List.choose(fun (timeline, data, calibration, location ) ->
                applyAgeDepthModelToData data calibration
                |> Option.map(fun r ->
                    let taxonLookup = TaxonomyLookup.proxiedTaxaLookup timeline graph |> Result.forceOk
                    timeline, r, taxonLookup, location)
            )

        printfn "Matched age-depth model and data for %i series." series.Length

        /// Get all the taxonomic trees including for
        /// each higher taxon (i.e. the Kingom, Plylum, Class etc.)
        /// for a species are listed as seperate trees.
        let taxonomicTreesAllLevels (taxonLookup:Map<string,Population.Taxonomy.TaxonNode list list>) =
            taxonLookup.Values
            |> Seq.collect id
            |> Seq.collect(fun tree ->
                let taxonTree = tree |> List.map TaxonomyLookup.taxonName
                List.scan(fun state t -> List.append state [t] )
                    [ taxonTree.Head ] taxonTree.Tail
            ) |> Seq.distinct

        printfn "Generating taxonomic index"
        let taxonIndex =
            newAges 
            |> Seq.collect(fun (_,_,lookup,_) -> taxonomicTreesAllLevels lookup)
            |> Seq.filter(fun tree -> tree.IsEmpty |> not)
            |> Seq.map(fun tree ->
                DataFiles.TaxonIndex.Row(fst (List.last tree), snd (List.last tree), tree.Tail |> Seq.map fst |> String.concat " > ")
                )
            |> fun d -> new DataFiles.TaxonIndex(d)

        taxonIndex.Save("../../data-derived/taxon-index.tsv")

        let intersects = Spatial.intersectsRegionFn ()

        printfn "Generating location index"
        let locationIndex =
            newAges |> List.map(fun (tsId, ageDepth, taxonLookup, location) ->
                let earliestDate = ageDepth.Keys |> Seq.map(fun k -> k.Date) |> Seq.max
                let latestDate = ageDepth.Keys |> Seq.map(fun k -> k.Date) |> Seq.min
                let locName = location |> Option.map(fun l ->l.Name.Value.Replace(",", " ")) |> Option.defaultValue "Unknown location"
                let latDd, lonDd, geomType = Conversions.locationToDecimalDegrees location
                let isIntersect = intersects latDd lonDd
                printfn "Intersects? %A" isIntersect

                DataFiles.TimelineIndex.Row(
                    timelineId = tsId.AsString,
                    siteName = locName,
                    latitudeDd = float latDd,
                    longitudeDd = float lonDd,
                    geometryType = geomType,
                    extentEarliestYbp = int earliestDate,
                    extentLatestYbp = int latestDate,
                    phytoSubzoneRegionId = isIntersect
                )
            )
            |> fun d -> new DataFiles.TimelineIndex(d)

        locationIndex.Save("../../data-derived/timeline-index.tsv")


        // Output dataset against calibrated ages:
        let dataWithCalibratedAges =
            newAges |> List.collect(fun (tsId, ageDepth, taxonLookup, location) ->
 
                ageDepth
                |> Seq.collect(fun kv ->
                    kv.Value
                    |> Seq.collect(fun x ->
                        taxonLookup
                        |> Map.tryFind x.Key
                        |> Option.defaultValue []
                        |> List.filter(fun tree -> not tree.IsEmpty)
                        |> List.map(fun tree ->
                            let taxon, rank = tree |> Seq.last |> TaxonomyLookup.taxonName
                            let taxonTree = tree |> List.map (TaxonomyLookup.taxonName >> fst) |> String.concat " > "
                            let othersInMorphotype =
                                taxonLookup |> Map.tryFind x.Key |> Option.defaultValue []
                                |> List.map(fun t -> t |> Seq.last |> TaxonomyLookup.taxonName |> fst)
                            DataFiles.BiodiversityVariableFile.Row(
                                locationId = tsId.AsString,
                                binEarly = (kv.Key.Date |> int),
                                binLate = (kv.Key.Date |> int),
                                morphotype = x.Key,
                                taxon = taxon,
                                rank = rank,
                                taxonomicTree = taxonTree,
                                taxonAmbiguousWith = (othersInMorphotype |> String.concat ";"),
                                variable = "pollen",
                                variableUnit = "percentage",
                                variableValue = x.Value,
                                variableCi = None,
                                variableConfidenceQualitative = None )
                        )
                    )
                ) |> Seq.toList )
            |> fun d -> new DataFiles.BiodiversityVariableFile(d)

        dataWithCalibratedAges.Save("../../data-derived/raw-data-calibrated.tsv")

        // Presence-absence:

        let overlapsBin binEarly binLate (k:Exposure.Reanalysis.AgeDepthModelDepth) =
            match k.StandardDeviation with
            | None -> k.Date <= binEarly && k.Date > binLate
            | Some sd ->
                k.Date <= binEarly + sd && k.Date > binLate - sd

        let presenceClass percentageValue =
            match percentageValue with
            | p when p > 2.50 -> "present"
            | p when p > 0.00 -> "borderline present"
            | _ -> "absent"

        let presenceToValue = function
            | "present" -> 1.0
            | "absent" -> 0.0
            | "borderline present" -> 0.5
            | _ -> nan

        /// Taxonomic trees for all taxa in the datasets
        let masterTaxonList =
            newAges 
            |> Seq.collect(fun (_,_,lookup,_) -> taxonomicTreesAllLevels lookup)
            |> Seq.filter(fun tree -> tree.IsEmpty |> not)
            |> Seq.toList

        let presenceInBin =
            newAges |> List.collect(fun (tsId, ageDepth, taxonLookup, _) ->
                
                timeBins
                |> List.collect(fun (binLate, binEarly) ->

                    let binEarly = float binEarly * 1.<OldDate.calYearBP>
                    let binLate = float binLate * 1.<OldDate.calYearBP>
                    
                    let taxaOrDefault t =
                        taxonLookup
                        |> Map.tryFind t
                        |> Option.map(fun l ->
                            l |> List.map(fun tree ->
                                tree |> List.map TaxonomyLookup.taxonName
                        ))
                        |> Option.defaultValue ([["Unverified taxon", "Unknown"]])

                    // Find the presence category for each morphotype
                    // present within the time window.
                    let binTaxaByPresenceCategory =
                        ageDepth
                        |> Map.filter (fun k _ -> overlapsBin binEarly binLate k)
                        |> Map.toList
                        |> Seq.collect (snd >> Seq.toList)
                        |> Seq.groupBy(fun t -> t.Key)
                        |> Seq.map(fun (_,g) ->
                            let maxValue = g |> Seq.maxBy(fun kv -> kv.Value)
                            taxaOrDefault maxValue.Key, presenceClass maxValue.Value )
                        |> Seq.toList

                    let forBotanicalTaxa =
                        masterTaxonList
                        |> List.map(fun thisTree ->

                            let thisTaxon = thisTree |> Seq.last

                            // Exact matches
                            let exactMorphotypeMatches =
                                binTaxaByPresenceCategory
                                |> List.filter(fun (taxa,c) ->
                                    taxa |> List.exists(fun t -> t |> Seq.last = thisTaxon))

                            // Matches to lower-rank morphotype links (e.g. matched Pinaceae to Pinus sp. pollen)
                            let lowerRankMatches =
                                binTaxaByPresenceCategory
                                |> List.filter(fun (taxa,c) ->
                                    taxa |> List.exists(fun t -> t |> Seq.contains thisTaxon))

                            // Matches at lower levels (e.g if this is J. communis and there is a pollen type for Juniper).
                            // NB could state uncertainty as distance between ranks, or ... ?
                            let upperRankMatches =
                                binTaxaByPresenceCategory
                                |> List.filter(fun (b,c) ->
                                    b |> List.exists(fun t ->
                                        not (Set.intersect (Set.ofList t) (Set.ofList thisTree)).IsEmpty))

                            thisTaxon,
                            match exactMorphotypeMatches.Length, lowerRankMatches.Length, upperRankMatches.Length with
                            | 1, _, _ -> true, snd exactMorphotypeMatches.Head, "high-confidence"
                            | i, _, _ when i > 1 ->
                                match exactMorphotypeMatches |> Seq.map snd |> Seq.distinct |> Seq.length with
                                | 1 -> true, snd exactMorphotypeMatches.Head, "high-confidence"
                                | _ ->
                                    if exactMorphotypeMatches |> Seq.map snd |> Seq.contains "present"
                                    then true, "present", "medium-confidence"
                                    else true, "borderline present", "medium-confidence"
                            | 0, 0, 0 -> false, "unknown", "unknown"
                            | 0, 1, _ -> false, snd lowerRankMatches.Head, "high-confidence"
                            | _, _, 1 -> false, snd upperRankMatches.Head, "low-confidence"
                            | _, _, 0 ->
                                // No matches exactly, but matches at lower ranks.
                                match lowerRankMatches |> Seq.distinct |> Seq.length with
                                | 1 -> false, snd exactMorphotypeMatches.Head, "high-confidence"
                                | _ -> false, "unknown", "unknown"
                            | _, _, _ ->
                                // No matches exactly, but matches at upper ranks.
                                match lowerRankMatches |> Seq.distinct |> Seq.length with
                                | 1 -> false, snd exactMorphotypeMatches.Head, "low-confidence"
                                | _ -> false, "unknown", "unknown"
                        )

                    forBotanicalTaxa |> List.map(fun b ->
                        (b |> snd |> (fun (isExactMatch,_,_) -> isExactMatch)),
                        DataFiles.BiodiversityVariableFile.Row(
                            locationId = tsId.AsString,
                            binEarly = int binEarly - 1,
                            binLate = int binLate,
                            morphotype = "unknown",
                            taxon = fst (fst b),
                            rank = snd (fst b),
                            taxonomicTree = "unknown",
                            taxonAmbiguousWith = "unknown",
                            variable = "presence",
                            variableUnit = "present-absent",
                            variableValue = (b |> snd |> (fun (_,b,_) -> b) |> presenceToValue),
                            variableCi = None,
                            variableConfidenceQualitative = Some(b |> snd |> (fun (_,_,c) -> c)) )
                    )
                )
            )

        let presenceFile = new DataFiles.BiodiversityVariableFile(presenceInBin |> List.map snd)
        presenceFile.Save("../../data-derived/populations/taxon-presence.tsv")

        // Trait spaces
        let traitData =
            Traits.PlantTraits.Load "/Users/andrewmartin/Documents/GitHub Projects/arctic-biodiversity-indicators/data-third-party/traits/arctic_traits.tsv"
            |> fun t -> t.Rows |> Seq.toList

        let sixTraits = [
            "Leaf area (in case of compound leaves: leaf, petiole included)"
            "Leaf nitrogen (N) content per leaf dry mass"
            "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included"
            "Plant height vegetative"
            "Seed dry mass"
            "Stem specific density (SSD) or wood density (stem dry mass per stem fresh volume)"
        ]

        let withTraits =
            presenceInBin
            |> List.filter(fun (isExactMatch, _) -> isExactMatch)
            |> List.map snd
            |> List.filter(fun x -> x.Variable_value <> 0.) // remove absent taxa
            |> List.groupBy(fun r -> r.Bin_early, r.Bin_late, r.Location_id)
            |> List.collect(fun ((binEarly, binLate, timeline),records) ->

                // Within each timeline, and for each bin...
                // - Connect taxa to traits dataset
                let taxaValues = 
                    records
                    |> List.map(fun x -> x.Taxon, x.Taxon_ambiguous_with)
                    |> List.distinct
                    |> List.collect(fun (taxon, ambiguousWith) ->

                        // Weight taxon values by taxonomic ambiguity
                        let taxonWeight = 1. / float (ambiguousWith.Split(";").Length)
                        
                        sixTraits
                        |> List.map(fun t ->
                            t,
                            traitData 
                            |> Seq.tryFind(fun i ->
                                i.Taxon = taxon && i.Trait_name = t && i.Origin = "arctic mean" && i.N > 1)
                            |> Option.orElseWith (fun () ->
                                traitData 
                                |> Seq.tryFind(fun i -> i.Taxon = taxon && i.Trait_name = t && i.Origin = "global mean"))
                            |> Option.map(fun r -> {| Taxon = taxon; Mean = r.Mean_value; Sd = r.Sd; Weight = taxonWeight; Unit = r.Unit; N = r.N |})
                        )
                    )

                // Summarise across taxa
                
                taxaValues
                |> List.groupBy fst
                |> List.map(fun (t,g) ->
                    
                    let median = 
                        g |> List.map snd |> List.choose id
                        // Filter out mean of NaN i.e. sample size of 0
                        |> List.filter(fun x -> System.Double.IsNaN x.Mean |> not)
                        |> List.map(fun x -> x.Mean, x.Weight)
                        |> fun l ->
                            if l.IsEmpty
                            then
                                printfn "Warning: no trait data for [%s] [%A/%A]" t binEarly binLate
                                nan
                            else Traits.weightedMean l

                    let units = g |> List.tryHead |> Option.bind snd |> Option.map(fun v -> v.Unit) |> Option.defaultValue "unknown unit"

                    let sd =
                        g |> List.map snd |> List.choose id
                        // Filter out SD of NaN i.e. sample size of 0 or 1
                        |> List.filter(fun x -> System.Double.IsNaN x.Sd |> not)
                        |> List.map(fun x -> x.Mean, x.Sd, x.N, x.Weight)
                        |> Traits.pooledStandardDeviation

                    DataFiles.BiodiversityVariableFile.Row(
                        locationId = timeline,
                        binEarly = binEarly,
                        binLate = binLate,
                        morphotype = "NA",
                        taxon = "community",
                        rank = "NA",
                        taxonomicTree = "NA",
                        taxonAmbiguousWith = "NA",
                        variable = t,
                        variableUnit = units,
                        variableValue = median,
                        variableCi = Some sd,
                        variableConfidenceQualitative = None )
                    )
            )

        let traitFile = new DataFiles.BiodiversityVariableFile(withTraits)
        traitFile.Save("../../data-derived/traits/plant-morphology.tsv")

        // SPATIAL INTERSECTIONS
        // Now to do intersections spatially
        let traitsSpatial =
            withTraits
            |> List.choose(fun r ->
                let loc = locationIndex.Rows |> Seq.find(fun l -> l.Timeline_id = r.Location_id)
                loc.Phyto_subzone_region_id |> Option.map(fun x -> x, r))
            |> List.groupBy fst
            |> List.collect(fun (regionId,records) ->
                
                // Use mean of mean and sd for region for each trait
                let traits =
                    records
                    |> List.map snd
                    |> List.groupBy(fun r -> r.Variable, r.Bin_early, r.Bin_late)
                    |> List.map(fun (_, rows) ->
                        
                        let newMean = rows |> Seq.map(fun r -> r.Variable_value) |> Seq.average
                        let newSd = rows |> Seq.choose(fun r -> r.Variable_ci) |> Seq.average

                        DataFiles.BiodiversityVariableFile.Row(
                            locationId = regionId,
                            binEarly = rows.Head.Bin_early,
                            binLate = rows.Head.Bin_late,
                            morphotype = rows.Head.Morphotype,
                            taxon = rows.Head.Taxon,
                            rank = rows.Head.Rank,
                            taxonomicTree = rows.Head.Taxonomic_tree,
                            taxonAmbiguousWith = rows.Head.Taxon_ambiguous_with,
                            variable = rows.Head.Variable,
                            variableUnit = rows.Head.Variable_unit,
                            variableValue = newMean,
                            variableCi = Some newSd,
                            variableConfidenceQualitative = None )
                        )

                traits
            )

        let traitSpatialFile = new DataFiles.BiodiversityVariableFile(traitsSpatial)
        traitSpatialFile.Save("../../data-derived/traits/plant-morphology_spatial.tsv")


        let presenceSpatial =
            presenceInBin
            |> List.choose(fun (isExact,r) ->
                let loc = locationIndex.Rows |> Seq.find(fun l -> l.Timeline_id = r.Location_id)
                loc.Phyto_subzone_region_id |> Option.map(fun x -> x, r))
            |> List.groupBy fst
            |> List.collect(fun (regionId,records) ->
                    records
                    |> List.map snd
                    |> List.groupBy(fun r -> r.Bin_early, r.Bin_late, r.Taxon)
                    |> List.map(fun (_, rows) ->
                        let newPresence = rows |> List.map(fun r -> r.Variable_value) |> Seq.max
                        let confidences = rows |> List.choose(fun r -> r.Variable_confidence_qualitative)
                        let newConfidence =
                            if confidences |> List.contains "high-confidence" then "high-confidence"
                            else if confidences |> List.contains "medium-confidence" then "medium-confidence"
                            else if confidences |> List.contains "low-confidence" then "medium-confidence"
                            else "unknown"
                        DataFiles.BiodiversityVariableFile.Row(
                            locationId = regionId,
                            binEarly = rows.Head.Bin_early,
                            binLate = rows.Head.Bin_late,
                            morphotype = rows.Head.Morphotype,
                            taxon = rows.Head.Taxon,
                            rank = rows.Head.Rank,
                            taxonomicTree = rows.Head.Taxonomic_tree,
                            taxonAmbiguousWith = rows.Head.Taxon_ambiguous_with,
                            variable = rows.Head.Variable,
                            variableUnit = rows.Head.Variable_unit,
                            variableValue = newPresence,
                            variableCi = None,
                            variableConfidenceQualitative = Some newConfidence )
                        )
            )

        let presenceSpatialFile = new DataFiles.BiodiversityVariableFile(presenceSpatial)
        presenceSpatialFile.Save("../../data-derived/populations/taxon-presence_spatial.tsv")

        return newAges
    }
