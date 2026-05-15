#r "nuget: FSharp.Data"
#load "./Utils.fsx"
#load "./Layout.fsx"
#load "./Corpus.fsx"

open Utils
open Layout
open Corpus
open FSharp.Data
open System.IO

type Distribution = {
    Left: Percent
    Right: Percent
}
module Distribution =
    let toString distribution =
        let left =
            distribution.Left
            |> Percent.toString 1
        
        let right =
            distribution.Right
            |> Percent.toString 1

        $"{left}-{right}"


    let tendancy distribution =
        if distribution.Left > distribution.Right then
            Left, distribution.Left
        else
            Right, distribution.Right

type Alternation = Alternation of (int * Side option) list
module Alternation =
    let distribution (Alternation series) =
        let rightInputs =
            series
            |> List.filter (snd >> (=) (Some Right))
            |> List.sumBy fst
        let leftInputs =
            series
            |> List.filter (snd >> (=) (Some Left))
            |> List.sumBy fst

        let total = rightInputs + leftInputs

        let percent inputs =
            decimal inputs / decimal total
            |> Percent

        {
            Left = percent leftInputs
            Right = percent rightInputs
        }

        
    let toString (Alternation series) =
        series
        |> List.map (fun (n, side) ->
            match side with
            | None -> "_"
            | Some s ->
                let strSide = Side.toString s
                $"{n}{strSide}"
        )
        |> joinWith " "


type Sequence = {
    Text: string
    Inputs: Input list
}
module Sequence =
    let showInputs sequence =
        sequence.Inputs
        |> List.map Input.toString
        |> joinWith " "

    let from layout (str: string) : Sequence =
        {
            Text = str
            Inputs =
                str
                |> Seq.collect (fun c ->
                    layout
                    |> Layout.inputs (string c)
                )
                |> Seq.toList
        }

    let alternation (sequence: Sequence) =
        sequence.Inputs
        |> splitWhenValueChanged Input.getSide
        |> List.map (fun (side, inputs) ->
            inputs.Length, side
        )
        |> Alternation


type NgramStats = {
    Ngram: string
    Count: int
    Frequency: decimal
}

type CorpusStatsJson = JsonProvider<"./corpus/white-fang.en.json">

type NgramStatsCategory = {
    Total: int
    Stats: NgramStats list
}
module NgramStatsCategory =
    let generate getNgram ngrams =
        let stats =
            ngrams
            |> Seq.map getNgram
            |> Seq.toList
        {
            Total = stats |> List.sumBy _.Count
            Stats = stats
        }

type CorpusStats = {
    Corpus: Corpus
    Symbols: NgramStatsCategory
    Digrams: NgramStatsCategory
    Trigrams: NgramStatsCategory
    Words: NgramStatsCategory
}
module CorpusStats =    
    let analyze corpus =
        let stats =
            $"./corpus/{corpus.FileName}.json"
            |> CorpusStatsJson.Load

        {
            Corpus = corpus
            Symbols =
                stats.Symbols
                |> NgramStatsCategory.generate (fun s -> {
                    Ngram = s.Char
                    Count = s.Count
                    Frequency = s.Frequency
                })
            Digrams =
                stats.Digrams
                |> NgramStatsCategory.generate (fun d -> {
                    Ngram = d.Digram.String.Value
                    Count = d.Count
                    Frequency = d.Frequency
                })
            Trigrams =
                stats.Trigrams
                |> NgramStatsCategory.generate (fun t -> {
                    Ngram = t.Trigram.String.Value
                    Count = t.Count
                    Frequency = t.Frequency
                })
            Words =
                stats.Words
                |> NgramStatsCategory.generate (fun w -> {
                    Ngram = w.Word.String.Value
                    Count = w.Count
                    Frequency = w.Frequency
                })
            
        }


type AlternationStats = {
    Alternation: Alternation
    Distribution: Distribution
    Tendancy: Side * Percent
}
module AlternationStats =
    let analyze alternation =
        let distribution = alternation |> Alternation.distribution
        {
            Alternation = alternation
            Distribution = distribution
            Tendancy = distribution |> Distribution.tendancy
        }

type WordLayoutStats = {
    Word: string
    Count: int
    Sequence: Sequence
    AlternationStats: AlternationStats
}
module WordLayoutStats =
    let analyze layout (ns: NgramStats) =
        let sequence =
            ns.Ngram
            |> Sequence.from layout

        {
            Word = ns.Ngram
            Count = ns.Count
            Sequence = sequence
            AlternationStats =
                sequence
                |> Sequence.alternation
                |> AlternationStats.analyze
        }

type LayoutWordStats = {
    Layout: Layout
    Corpus: Corpus
    StatsByWord: WordLayoutStats list
}
module LayoutWordStats =
    let alternationBeyond threshold stats =
        { stats with
            StatsByWord =
                stats.StatsByWord
                |> List.filter (fun s ->
                    let (Alternation series) = s.AlternationStats.Alternation
                    let total = series |> List.sumBy fst
                    
                    let ratio =
                        decimal series.Length / decimal total
                        |> Percent

                    threshold <= ratio
                )
        }

    let alternationAfter n stats =
        let mots =
            stats.StatsByWord
            |> List.sortByDescending (fun s ->
                let (Alternation series) = s.AlternationStats.Alternation
                series |> List.maxBy fst
            )
            |> List.filter (fun s ->
                let (Alternation series) = s.AlternationStats.Alternation
                series |> List.exists (fun (nombre, _) -> nombre >= n)
            )
        { stats with
            StatsByWord = mots
        }

    let filterByDistribution threshold stats =
        { stats with
            StatsByWord =
                stats.StatsByWord
                |> List.sortByDescending _.AlternationStats.Distribution
                |> List.filter (fun r ->
                    snd r.AlternationStats.Tendancy >= threshold
                )
        }

    let filter predicate stats =
        { stats with
            StatsByWord =
                stats.StatsByWord
                |> List.filter predicate
        }

    let take n stats =
        { stats with
            StatsByWord =
                stats.StatsByWord
                |> List.take n
        }

    let show stats =
        let language =
            stats.Corpus.Language
            |> Language.toString

        let words = stats.StatsByWord.Length
        let total = stats.StatsByWord |> List.sumBy _.Count

        printfn "%s - %s" stats.Layout.Name language
        printfn "%i mots (%i au total)" words total
        printfn "------"

        stats.StatsByWord
        |> List.iter (fun s ->
            printfn "%s (%i occ.)" s.Word s.Count

            s.Sequence
            |> Sequence.showInputs
            |> printfn "\tSéquence : %s"

            let strDistribution =
                s.AlternationStats.Distribution
                |> Distribution.toString

            let strAlternation =
                s.AlternationStats.Alternation
                |> Alternation.toString

            printfn "\tAlternance : %s (%s)" strAlternation strDistribution
        )

    let analyze layout (corpusStats: CorpusStats) =
        {
            Layout = layout
            Corpus = corpusStats.Corpus
            StatsByWord =
                corpusStats.Words.Stats
                |> List.map (WordLayoutStats.analyze layout)
        }



let erglace =
    @"/home/chris/Programmes/Erglace/erglace.toml"
    |> File.ReadAllText
    |> Layout.parse

let ergol =
    @"/home/chris/Programmes/ergol/keymaps/fr/ergol.toml"
    |> File.ReadAllText
    |> Layout.parse

let statsCSharp =
    Corpus.CSharp.aspnet
    |> CorpusStats.analyze

let statsWhiteFangFr =
    Corpus.Novel.whiteFang.French
    |> CorpusStats.analyze

let stats layout =
    statsWhiteFangFr
    |> LayoutWordStats.analyze layout
    |> LayoutWordStats.filter (fun s ->
        s.Word.Length >= 4
    )
    |> LayoutWordStats.take 200
    

let statsErglace = stats erglace
let statsErgol = stats ergol

type DistributionComparison = {
    Erglace: LayoutWordStats
    Ergol: LayoutWordStats
}
module DistributionComparison =
    let show comparison =
        comparison.Erglace
        |> LayoutWordStats.show

        printfn "\n"

        comparison.Ergol
        |> LayoutWordStats.show

    let compare filter =
        {
            Erglace = filter statsErglace
            Ergol = filter statsErgol
        }

// alternance à plus de 80%
DistributionComparison.compare (
    LayoutWordStats.alternationBeyond %80m 
)
|> DistributionComparison.show


// alternance après 4 frappes
DistributionComparison.compare (
    LayoutWordStats.alternationAfter 4
)
|> DistributionComparison.show

// repartition au dessus 80%
DistributionComparison.compare (
    LayoutWordStats.filterByDistribution %80m
)
|> DistributionComparison.show
