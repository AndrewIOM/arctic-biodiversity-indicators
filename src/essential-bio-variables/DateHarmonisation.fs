module DateHarmonisation

open BiodiversityCoder.Core
open BiodiversityCoder.Core.GraphStructure
open BiodiversityCoder.Core.FieldDataTypes
open RProvider
open RProvider.Operators
open RProvider.oxcAAR

/// The standard deviation to apply to a date
/// where there was no standard deviatin specified
/// in the text / coded into the graph.
let defaultSdWhenUnspecified = 100.<OldDate.calYearBP>

let analysisDate = Time.SimpleDateOnly.TryCreate(Text (System.DateOnly.FromDateTime(System.DateTime.Now).ToShortDateString())) |> Option.get
let analysisPerson = "Martin, A.C." |> Author.create |> Result.forceOk


let readAllTimelineAtoms (graph:Storage.FileBasedGraph<GraphStructure.Node,GraphStructure.Relation>) =
    result {
        let! sourceIds = 
            graph.Nodes<Sources.SourceNode> ()
            |> Result.ofOption "Could not load sources"

        // Find temporal extents from sources
        let! sourceAtoms =
            sourceIds
            |> Seq.map(fun kv -> kv.Key)
            |> Seq.toList
            |> Storage.loadAtoms graph.Directory (typeof<Sources.SourceNode>).Name

        let! allTimelines =
            sourceAtoms
            |> List.collect(fun (s:Graph.Atom<GraphStructure.Node, GraphStructure.Relation>) -> 
                (snd s)
                |> List.choose(fun (_,sinkId,_,conn) ->
                    match conn with
                    | GraphStructure.Relation.Source r ->
                        match r with
                        | Sources.SourceRelation.HasTemporalExtent -> Some sinkId
                        | _ -> None
                    | _ -> None )
            )
            |> Storage.loadAtoms graph.Directory (typeof<Exposure.ExposureNode>.Name)

        return allTimelines
}

/// Queries the graph database to obtain timelines for which
/// we have at leaat two individual dates. Returns the timeline
/// ID * a list of individual dates.
let queryGraphForDates (graph:Storage.FileBasedGraph<GraphStructure.Node,GraphStructure.Relation>) =
    result {

        let! allTimelines = readAllTimelineAtoms graph

        let! datesByTimelineId =
            allTimelines
            |> List.map(fun (timelineNode,timelineRelations) ->
                timelineNode,
                timelineRelations
                |> List.choose(fun (_,sinkId,_,conn) ->
                    match conn with
                    | Relation.Exposure r ->
                        match r with
                        | Exposure.ExposureRelation.ConstructedWithDate -> Some sinkId
                        | _ -> None
                    | _ -> None ),
                timelineRelations
                |> List.tryPick(fun (_,sinkId,_,conn) ->
                    match conn with
                    | Relation.Exposure r ->
                        match r with
                        | Exposure.ExposureRelation.IsLocatedAt -> Some sinkId
                        | _ -> None
                    | _ -> None ),
                timelineRelations
                |> List.tryPick(fun (_,sinkId,_,conn) ->
                    match conn with
                    | Relation.Exposure r ->
                        match r with
                        | Exposure.ExposureRelation.ExtentLatestSpecified _ -> Some sinkId
                        | Exposure.ExposureRelation.ExtentLatest -> Some sinkId
                        | _ -> None
                    | _ -> None )

                )
            |> List.filter(fun (_,l,_,_) -> l.Length > 1)
            |> List.map(fun (node, dates, contextId, extentLatestId) ->

                if (node |> fst).AsString.Contains "40f5222c-4060-4b23-8623-81944b44844f"
                then printfn "Found it"

                let latestExtentTime = 
                    extentLatestId
                    |> Option.map(fun k -> ((System.Text.RegularExpressions.Regex.Match(k.AsString, "calyearnode_(.*)ybp").Groups.[1].Value |> int) * 1<OldDate.calYearBP>))

                let context =
                    contextId
                    |> Option.map (fun c -> c |> Storage.loadAtom graph.Directory (typeof<Population.Context.ContextNode>.Name) |> Result.toOption)
                    |> Option.bind id
                    |> Option.bind(fun node ->
                        match node |> fst |> snd with
                        | GraphStructure.Node.PopulationNode p ->
                            match p with
                            | PopulationNode.ContextNode c -> Some c
                            | _ -> None
                        | _ -> None )

                dates
                |> Storage.loadAtoms graph.Directory (typeof<Exposure.StudyTimeline.IndividualDateNode>.Name)
                |> Result.map(fun (nodes: Graph.Atom<GraphStructure.Node,GraphStructure.Relation> list) ->
                    nodes
                    |> List.choose(fun ((k,n),dateRels) ->
                        match n with
                        | GraphStructure.Node.ExposureNode p ->
                            match p with
                            | Exposure.ExposureNode.DateNode d ->
                                
                                // Has date been used before to calibrate?
                                let usedInCalibration =
                                    dateRels
                                    |> List.tryPick(fun (_,sinkId,_,conn) ->
                                        match conn with
                                        | Relation.Exposure r ->
                                            match r with
                                            | Exposure.ExposureRelation.UsedInCalibration -> Some sinkId
                                            | _ -> None
                                        | _ -> None )
                                
                                Some (k,d,usedInCalibration)
                            | _ -> None
                        | _ -> None
                    )
                )
                |> Result.lift(fun dates -> node |> fst, context, latestExtentTime, dates)
            )
            |> Result.ofList

        return datesByTimelineId
    }

let measurementError measureError =
    match measureError with
    | FieldDataTypes.OldDate.MeasurementError.DatingErrorPlusMinus sd -> sd
    | FieldDataTypes.OldDate.MeasurementError.DatingErrorPlusMinusSigma (_,sd) -> sd
    | FieldDataTypes.OldDate.MeasurementError.DatingErrorRangeSigma (_,low,hi) -> Seq.average [ low; hi ]
    | FieldDataTypes.OldDate.MeasurementError.NoDatingErrorSpecified -> defaultSdWhenUnspecified

/// Converts AD dates to cal yr BP dates
let adToBp (date:float<OldDate.AD>) =
    let newDate = 1950.<OldDate.AD> - date
    if newDate <= 0.<OldDate.AD>
    then newDate + 1.<OldDate.AD> |> float |> (*) 1.<OldDate.calYearBP>
    else newDate |> float |> (*) 1.<OldDate.calYearBP>

module OxCal =

    open BiodiversityCoder.Core.Exposure.Reanalysis
    open FieldDataTypes.OldDate.Harmonised
    open RDotNet

    type OxCalInputDate =
        | Radiocarbon of identifier: string * r:float<OldDate.uncalYearBP> * sd:float<OldDate.calYearBP>
        | DateAD of ad:float<OldDate.AD>
        | DateBC of ad:float<OldDate.BC>
        | UndatedDepth of identifier: string

    let adDate (adDate:float<OldDate.AD>) depth = sprintf "Date(AD(%i)) { z=%f; };" (int adDate) depth
    let bcDate (bcDate:float<OldDate.BC>) depth = sprintf "Date(BC(%i)) { z=%f; };" (int bcDate) depth
    let radiocarbonDate identifier year sd depth = sprintf """R_Date("%s",%i,%i){ z=%f; };""" identifier year sd depth
    let radiocarbonDateNoDepth identifier year sd = sprintf """R_Date("%s",%i,%i);""" identifier year sd
    let undatedDepth identifier depth = sprintf """Date("%s"){ z=%f; };""" identifier depth

    let bpToAd (oldDate:int<OldDate.calYearBP>) =
        match oldDate with
        | o when o = 1950<OldDate.calYearBP> -> DateBC 1.<OldDate.BC>
        | o when o <= 1949<OldDate.calYearBP> ->
            (1950<OldDate.calYearBP> - o) |> float |> (*) 1.<OldDate.AD> |> DateAD
        | o ->
            (o - 1950<OldDate.calYearBP> + 1<OldDate.calYearBP>) |> float |> (*) 1.<OldDate.BC> |> DateBC

    /// Set k0 as 1 when using cm or 100 when using metres.
    let oxCalSedimendarySequenceScript (dates:string seq) =
        let k0 = 1
        sprintf """ Plot() { P_Sequence("variable",%i,2,U(-2,2)) { Boundary("Bottom"); %s Boundary("Top"); }; };""" k0 (dates |> String.concat "\n")

    /// Get date ranges for a particular date 
    let getSigmaLevel sigmaLevel sigma d =
        let nRanges =
            if d?sigma_ranges.Member(sigmaLevel).IsDataFrame()
            then d?sigma_ranges.Member(sigmaLevel)?probability.AsList().Length
            else 0
        match nRanges with
        | 1 ->
            [{
                Sigma = sigma
                Probability = d?sigma_ranges.Member(sigmaLevel)?probability.GetValue<float> () / 100. |> Percent.create |> Result.forceOk
                LaterBound = d?sigma_ranges.Member(sigmaLevel)?``end``.GetValue<float> () * 1.<OldDate.AD> |> adToBp
                EarlierBound = d?sigma_ranges.Member(sigmaLevel)?start.GetValue<float> () * 1.<OldDate.AD> |> adToBp
            }]
        | 0 -> []
        | _ ->
            [ 1 .. d?sigma_ranges.Member(sigmaLevel)?probability.AsList().Length ] |> List.map(fun r ->
                {
                    Sigma = sigma
                    Probability = d?sigma_ranges.Member(sigmaLevel)?probability.AsList().[r - 1].GetValue<float> () / 100. |> Percent.create |> Result.forceOk
                    LaterBound = d?sigma_ranges.Member(sigmaLevel)?``end``.AsList().[r - 1].GetValue<float> () * 1.<OldDate.AD> |> adToBp
                    EarlierBound = d?sigma_ranges.Member(sigmaLevel)?start.AsList().[r - 1].GetValue<float> () * 1.<OldDate.AD> |> adToBp
                }
            )

    let warningsByName (fullModel:RDotNet.SymbolicExpression) =
        R.names(fullModel).GetValue<string list> ()
        |> List.filter(fun n -> n.StartsWith "ocd")
        |> List.choose(fun ocd ->
            let l = fullModel.Member(ocd)?likelihood
            let name =
                match fullModel.Member(ocd)?name with
                | RDotNet.ActivePatterns.Null -> ""
                | _ -> fullModel.Member(ocd)?name.GetValue<string>()
            if l.AsList().Length > 1
            then
                let warnings =
                    R.names(l).GetValue<string list> ()
                    |> List.filter(fun s -> s.StartsWith "warning")
                if warnings.Length > 0
                then
                    warnings
                    |> List.map(fun warning ->
                        l.Member(warning).AsList().[0].AsCharacter().GetValue<string list>() |> List.tryHead)
                    |> List.choose id
                    |> fun w -> Some (name, w)
                else None
            else None
        ) |> Map.ofList

    let calibrateDatesFromSequence (dates:(float<StratigraphicSequence.cm> * OxCalInputDate) list) =
        
        let oxCalScript : string =
            dates
            |> Seq.map(fun (depth,d) ->
                match d with
                | Radiocarbon (identifier,r,sd) -> radiocarbonDate identifier (int r) (int sd) depth
                | DateAD(ad) -> adDate ad depth
                | DateBC(bc) -> bcDate bc depth
                | UndatedDepth identifier -> undatedDepth identifier depth )
            |> oxCalSedimendarySequenceScript

        printfn "OXCAL SCRIPT: %s" oxCalScript

        let my_result_file = R.executeOxcalScript(oxcal__script = oxCalScript)

        printfn "Finished OxCal. Press any key to continue..."
        // System.Console.ReadLine() |> ignore

        let my_result_text = R.readOxcalOutput(my_result_file)
        let my_result_data = R.parseOxcalOutput(my_result_text)

        // Age-depth model:
        let fullModel = R.parseFullOxcalOutput(my_result_text)
        let depths = fullModel?model?``element[1]``?age_depth_z.GetValue<float list>() |> List.map(fun f -> f * 1.<StratigraphicSequence.cm>)
        let ages = fullModel?model?``element[1]``?age_depth_mean.GetValue<float list>() |> List.map(fun f -> f * 1.<OldDate.AD>)
        let agesSd = fullModel?model?``element[1]``?age_depth_sd.GetValue<float list>() |> List.map(fun f -> f * 1.<OldDate.calYearBP>)
        let calibrationCurve = fullModel?``calib[0]``?ref.GetValue<string> ()

        let warnings = warningsByName fullModel

        let ageDepthModel : AgeDepthModelDepth list = 
            List.zip3 depths ages agesSd
            |> List.map(fun (d,a,sd) ->
                {
                    Depth = d
                    Date = adToBp a
                    StandardDeviation = Some sd
                })

        (fun curve modelCode softwareName softwareVersion ->

            // Find individual date calibrations.
            let individualDates =
                my_result_data.AsList()
                |> Seq.map(fun d ->
                    d?name.GetValue<string> (),
                    
                    {
                        CalibrationCurve = curve
                        InputDate = float (d?bp.GetValue<int> ()) * 1.<OldDate.uncalYearBP>
                        InputStandardDeviation = float (d?std.GetValue<int> ()) * 1.<OldDate.uncalYearBP> |> Some
                        DateRanges = ([
                            getSigmaLevel "one_sigma" OldDate.OneSigma d
                            getSigmaLevel "two_sigma" OldDate.TwoSigma d
                            getSigmaLevel "three_sigma" OldDate.ThreeSigma d ] |> List.concat)
                        SoftwareUsed = softwareName
                        Origin = PartOfReanalysis(analysisPerson, analysisDate)
                        HasWarnings = (
                            warnings
                            |> Map.tryFind (d?name.GetValue<string> ())
                            |> Option.bind(fun l ->
                                let warn = l |> List.map (Text.create >> Result.toOption) |> List.choose id
                                if warn.Length = 0 then None else Some warn )
                        )
                    }
                )
                |> Map.ofSeq

            {
                CalibrationCurve = curve
                ModelApplied = OxCalModel modelCode
                SoftwareName = softwareName
                SoftwareVersion = softwareVersion
                Origin = PartOfReanalysis(analysisPerson, analysisDate)
                AgeDepthModel = Some ageDepthModel
            }, individualDates)
        <!> (Text.createShort calibrationCurve)
        <*> (Text.create oxCalScript)
        <*> ("OxCal" |> Text.createShort)
        <*> ("4.4.4" |> Text.createShort)

/// Gets an exposure relation and parses the cal yr BP date
/// from the node key.
let exposureDateByRelationType thingToMatch (atom:Graph.Atom<GraphStructure.Node, GraphStructure.Relation>) =
    atom
    |> snd
    |> List.choose(fun (_,sinkId,_,conn) ->
        match conn with
        | Relation.Exposure r ->
            match r with
            | s when s = thingToMatch -> Some ((System.Text.RegularExpressions.Regex.Match(sinkId.AsString, "calyearnode_(.*)ybp").Groups.[1].Value |> int) * 1<OldDate.calYearBP>)
            | _ -> None
        | _ -> None )


let calibrateExtent (timelineAtom:Graph.Atom<GraphStructure.Node,GraphStructure.Relation>) graph =
    // TODO
    // - Read in extents for all timelines
    // - Where radiocarbon, recalibrate using OxCal (single date mode)
    // - Link early and latest extent with relations into calyr nodes.
    let extentEarly = exposureDateByRelationType Exposure.ExposureRelation.ExtentEarliest timelineAtom
    let extentEarlyUncertainty = exposureDateByRelationType Exposure.ExposureRelation.ExtentEarliestUncertainty timelineAtom
    graph

/// For sedimentary records, applies a sequence model in OxCal
/// to calibrate ages and a fresh age-depth model using IntCal20.
/// If data falls into depth range outside of given dates, sets the
/// depths in the OxCal model to cause ages to be generated to the
/// stated depth bounds (e.g. when extrapolation occurs at the top or
/// bottom of a core).
/// - TODO What about surface age?
/// - TODO If there are dates not from depths, do not use a sequence model.
let processTimeline (timeline:list<Graph.UniqueKey * Exposure.StudyTimeline.IndividualDateNode>) (context:option<Population.Context.ContextNode>) (latestExtent: option<int<OldDate.calYearBP>>) graph =

    printfn "Latest extent is %A" latestExtent

    let preparedDates =
        timeline
        |> List.filter(fun (_,date) -> not date.Discarded)
        |> List.map(fun (indDateKey,date) ->

            let depth =
                match date.SampleDepth with
                | Some d ->
                    match d with
                    | StratigraphicSequence.DepthInCore.DepthBand (low,up) -> Some <| Seq.average [ low.Value; up.Value ]
                    | StratigraphicSequence.DepthInCore.DepthNotStated -> None
                    | StratigraphicSequence.DepthInCore.DepthPoint p -> Some p.Value
                    | StratigraphicSequence.DepthInCore.DepthQualitativeLevel _ -> None
                | None -> None

            let handleUncalDate name (uncal:OldDate.UncalDate) =
                Some <| OxCal.Radiocarbon(name, uncal.Date, measurementError uncal.UncalibratedDateError)

            let handleCalDate name (cal:OldDate.CalibratedRadiocarbonDate) =
                match cal.UncalibratedDate with
                | Some uncal -> handleUncalDate name uncal
                | None ->
                    printfn "Ignoring calibrated radiocarbon date that has no uncalibrated value."
                    None

            let handleOldDate name d =
                match d with
                | OldDate.OldDate.CalYrBP cal -> handleCalDate name cal
                | OldDate.OldDate.BP bp -> Some <| OxCal.Radiocarbon(name, bp, measurementError date.MeasurementError)
                | OldDate.OldDate.HistoryYearAD ad -> Some <| OxCal.DateAD ad
                | OldDate.OldDate.HistoryYearBC bc -> Some <| OxCal.DateBC bc                    

            let name = indDateKey.AsString

            indDateKey,
            depth,
            match date.Date with
            | OldDate.OldDatingMethod.RadiocarbonCalibrated cal -> handleCalDate name cal
            | OldDate.OldDatingMethod.RadiocarbonCalibratedRanges ranges ->
                ranges.UncalibratedDate |> Option.bind (fun u -> handleUncalDate name u)
            | OldDate.OldDatingMethod.RadiocarbonUncalibrated bp
            | OldDate.OldDatingMethod.RadiocarbonUncalibratedConventional bp -> 
                Some <| OxCal.Radiocarbon(name, bp, measurementError date.MeasurementError)
            | OldDate.OldDatingMethod.CollectionDate y -> Some <| OxCal.DateAD y
            | OldDate.OldDatingMethod.HistoricEvent (_, d)
            | OldDate.OldDatingMethod.Tephra (_,d) -> handleOldDate name d
            | OldDate.OldDatingMethod.Lead210 _
            | OldDate.OldDatingMethod.DepositionalZone _
            | OldDate.OldDatingMethod.Radiocaesium _ -> None )

    let totalDates = preparedDates |> Seq.choose (fun (_,_,d) -> d) |> Seq.filter(fun d -> match d with | OxCal.Radiocarbon _ -> true | _ -> false) |> Seq.length
    if totalDates = 0
    then
        printfn "Skipping time series as there were no radiocarbon dates left after pre-processing."
        graph
    else

        let isDepthSequence = preparedDates |> List.map(fun (_,d,_) -> d) |> List.contains None |> not
        if isDepthSequence then

            // Add in the depths above and below given dates.
            let bottomDepth, topDepth =
                match context with
                | None -> None, None
                | Some c ->
                    printfn "Sample origin is %A." c.SampleOrigin
                    match c.SampleOrigin with
                    | Population.Context.SampleOrigin.LakeSediment depths
                    | Population.Context.SampleOrigin.PeatCore depths
                    | Population.Context.SampleOrigin.Excavation depths ->
                        match depths with
                        | StratigraphicSequence.DepthExtent.DepthRange (top, bottom) -> (Some bottom), (Some top)
                        | StratigraphicSequence.DepthExtent.DepthRangeNotStated -> None, None
                    | Population.Context.SampleOrigin.OtherOrigin (_, depths) ->
                        match depths with
                        | None -> None, None
                        | Some depths ->
                            match depths with
                            | StratigraphicSequence.DepthExtent.DepthRange (top, bottom) -> (Some bottom), (Some top)
                            | StratigraphicSequence.DepthExtent.DepthRangeNotStated -> None, None
                    | Population.Context.SampleOrigin.LivingOrganism
                    | Population.Context.SampleOrigin.Subfossil -> None, None

            let usedDateKeys, orderedDates =
                preparedDates
                |> fun dates ->
                    if topDepth.IsSome then
                        if dates |> List.forall (fun (_,d,_) -> d.Value > topDepth.Value.Value)
                        then
                            // There is a top depth that is higher than the dates;
                            // determine if constrained at the top by latest extent date.
                            if topDepth.Value.Value <= 3.<StratigraphicSequence.cm> && latestExtent.IsSome
                            then
                                if latestExtent.Value <= 10<OldDate.calYearBP> && not (preparedDates |> Seq.exists(fun (_,_,d) -> d = Some(OxCal.bpToAd latestExtent.Value))) then
                                    printfn "Use the latest extent as a 'pin' for top depth, as it is close to modern-day. Pinning as %A (%A)" latestExtent.Value (OxCal.bpToAd latestExtent.Value)
                                    (Graph.UniqueKey.FriendlyKey ("depthextent", "top"), topDepth |> Option.map(fun v -> v.Value), Some <| OxCal.bpToAd latestExtent.Value) :: dates
                                else (Graph.UniqueKey.FriendlyKey ("depthextent", "top"), topDepth |> Option.map(fun v -> v.Value), Some <| OxCal.UndatedDepth "top") :: dates
                            else (Graph.UniqueKey.FriendlyKey ("depthextent", "top"), topDepth |> Option.map(fun v -> v.Value), Some <| OxCal.UndatedDepth "top") :: dates
                        else dates
                    else dates
                |> fun dates ->
                    if bottomDepth.IsSome then
                        if dates |> List.forall (fun (_,d,_) -> d.Value < bottomDepth.Value.Value)
                        then (Graph.UniqueKey.FriendlyKey ("depthextent", "bottom"), bottomDepth |> Option.map(fun v -> v.Value), Some <| OxCal.UndatedDepth "bottom") :: dates
                        else dates
                    else dates
                |> List.sortByDescending(fun (_,d,_) -> d.Value)
                |> List.filter(fun (_,_,d) -> d.IsSome)
                |> List.map(fun (k,depth,d) -> k, Option.map2(fun a b -> (a,b)) depth d)
                |> List.unzip

            result {
                let! calibrated, individualCalDates = OxCal.calibrateDatesFromSequence (orderedDates |> List.choose id)
                printfn "Calibrated is %A" calibrated

                let! (g,addedNodes) = Storage.addNodes graph [ calibrated |> Exposure.ExposureNode.DateCalibrationInstanceNode |> Node.ExposureNode ]
                let newCalNode = addedNodes.Head

                let linkDateToCalNode g (indDateKey:Graph.UniqueKey) =
                    g |> Result.bind(fun g ->
                        match indDateKey.AsString with
                        | s when s.StartsWith("individualdatenode") ->
                            Storage.addRelationByKey g indDateKey (newCalNode |> fst |> fst) (ProposedRelation.Exposure Exposure.ExposureRelation.UsedInCalibration)
                        | _ -> Ok g
                    )

                let! graphWithInboundLinks = 
                    usedDateKeys
                    |> List.fold linkDateToCalNode (Ok g)

                // Link cal node back to dates with their sigma ranges:
                let linkCalNodeToDates g (key:string) (calibratedDate:OldDate.Harmonised.DateCalibration) =
                    g |> Result.bind(fun g ->
                        match key with
                        | s when s.StartsWith("individualdatenode") ->
                            match preparedDates |> Seq.tryFind(fun (x,_,_) -> x.AsString = key) with
                            | None -> Ok g
                            | Some (indDateNodeKey,_,_) ->
                                Storage.addRelationByKey g (newCalNode |> fst |> fst) indDateNodeKey
                                    (ProposedRelation.Exposure(Exposure.ExposureRelation.Calibrated calibratedDate))
                        | _ -> Ok g
                    )

                let! graphWithOutboundLinks = 
                    individualCalDates
                    |> Map.fold linkCalNodeToDates (Ok graphWithInboundLinks)
                
                printfn "Waiting for key..."
                // System.Console.ReadLine() |> ignore

                return graphWithOutboundLinks
            } |> Result.forceOk
        else
            printfn "Is not a depth sequence. Skipping..."
            graph


