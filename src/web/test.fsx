#r "nuget: FSharp.Data"

open FSharp.Data

type Index = CsvProvider<"../../../data-derived/ebvs/raw-data-calibrated.csv">

let data = Index.Load "../../../data-derived/ebvs/raw-data-calibrated.csv"

let dataForTimeline morphotypes timeline : (decimal * decimal) seq =
    data.Rows
    |> Seq.filter(fun r -> r.Timeline_id = timeline)
    |> Seq.filter(fun r -> morphotypes |> Seq.contains r.Morphotype)
    |> Seq.map(fun r -> r.Age_ybp, r.``Data-value``)