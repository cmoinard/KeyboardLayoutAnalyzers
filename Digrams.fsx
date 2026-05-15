#r "nuget: FSharp.Data"
#load "./Utils.fsx"

open Utils
open FSharp.Data
open System.Text

let [<Literal>] Precision = 4
let [<Literal>] FreqSize = Precision + 2


type Symbol = private Symbol of string
module Symbol =
    let from (s: string) =
        s
        |> Encoding.UTF8.GetBytes
        |> Encoding.UTF8.GetString
        |> Symbol

    let toString (Symbol s) = s


type Digram = Digram of Symbol * Symbol
module Digram =
    let combinations s1 s2 =
        [
            Digram (s1, s2)
            Digram (s2, s1)
        ]

    let toString (Digram (s1,  s2)) =
        Symbol.toString s1 + Symbol.toString s2


type Language =
    | English
    | French
module Language =
    let label =
        function
        | English -> "en"
        | French -> "fr"

type Corpus =
    | DonQuixote of Language
    | WhiteFang of Language
    | CSharp
module Corpus =
    let all = [
        DonQuixote French
        WhiteFang French
        DonQuixote English
        WhiteFang English
        CSharp
    ]

    let label =
        function
        | DonQuixote l -> (l |> Language.label) + " - DonQuixote"
        | WhiteFang l -> (l |> Language.label) + " - WhiteFang"
        | CSharp -> "C# aspnet"

    let fileName =
        function
        | DonQuixote l -> l |> Language.label
        | WhiteFang l -> "white-fang." + (l |> Language.label)
        | CSharp -> "aspnet_csharp"

    let maxSize =
        all
        |> List.map (label >> _.Length)
        |> List.max

type CorpusStats = JsonProvider<"./corpus/en.json">
type CorpusWithName = {
    Stats: CorpusStats.Root
    Corpus: Corpus
}
module CorpusStats =
    let private getFrequency (corpus: CorpusStats.Root) (digram: Digram) =
        let strDigram = digram |> Digram.toString

        corpus.Digrams
        |> Array.tryFind (fun d ->
            d.Digram.String.Value = strDigram
        )
        |> Option.map _.Frequency
        |> Option.defaultValue 0m

    let private all =
        Corpus.all
        |> List.map (fun c -> {
            Corpus = c
            Stats =
                c
                |> Corpus.fileName
                |> fun name -> $"./corpus/{name}.json"
                |> CorpusStats.Load 
        })

    let getFrequencyByCorpus digram =
        all
        |> List.map (fun corpus ->
            let freq =
                digram
                |> getFrequency corpus.Stats

            corpus.Corpus, freq
        )
        |> Map.ofList

type DigramCompare = {
    Digram: Digram
    FrequencyByCorpus: Map<Corpus, decimal>
}

type DigramStat = {
    Symbols: Symbol list
    Comparisons: DigramCompare list
    TotalFrequencyByCorpus:  Map<Corpus, decimal>
}

type DigramFrequencies = {
    Corpus: Corpus
    Freq1: decimal
    Freq2: decimal
    Total: decimal
}
module DigramStat =
    let private frequencies stats =
        Corpus.all
        |> List.map (fun corpus -> {
            Corpus = corpus
            Freq1 = stats.Comparisons[0].FrequencyByCorpus[corpus]
            Freq2 = stats.Comparisons[1].FrequencyByCorpus[corpus]
            Total = stats.TotalFrequencyByCorpus[corpus]
        })

    let private getFrequency (corpus: CorpusStats.Root) (digram: Digram) =
        let strDigram = digram |> Digram.toString

        corpus.Digrams
        //|> Array.map (fun d -> Digram.from d.Digram.String.Value)
        |> Array.tryFind (fun d ->
            d.Digram.String.Value = strDigram
        )
        |> Option.map _.Frequency
        |> Option.defaultValue 0m

    let compare (s1: Symbol) (s2: Symbol) =
        Digram.combinations s1 s2
        |> List.map (fun d ->
            {
                Digram = d
                FrequencyByCorpus = CorpusStats.getFrequencyByCorpus d
            }
        )

    let compareAll (s1: Symbol) (s2: Symbol) =
        let comparisons = compare s1 s2

        let totalFrequencyByCorpus =
            comparisons
            |> Seq.collect _.FrequencyByCorpus
            |> Seq.groupBy _.Key
            |> Seq.map (fun (key, freqs) ->
                key, freqs |> Seq.sumBy _.Value
            )
            |> Map.ofSeq

        {
            Symbols = [ s1; s2 ]
            Comparisons = comparisons
            TotalFrequencyByCorpus = totalFrequencyByCorpus
        }

    let compareStrings (symbols: string list) (s: string) =
        let symbol = Symbol.from s

        symbols
        |> List.map (Symbol.from >> compareAll symbol)

    let table (digramStats: DigramStat list) =
        let frequencyCol (f: decimal) =
            let format = "0" |> String.replicate Precision

            f.ToString $"0.{format}"
            |> padRight FreqSize

        let langSize = Corpus.maxSize

        let getLines stats =
            let header =
                let lang = " " |> String.replicate langSize
                let all = "all" |> padRight FreqSize

                let digramsLabel =
                    stats.Comparisons
                    |> List.map (fun c ->
                        c.Digram
                        |> Digram.toString
                        |> padRight FreqSize
                    )
                    |> joinWith " | "

                $"| {lang} | {digramsLabel} | {all} |"

            let corpusLines =
                stats
                |> frequencies
                |> List.map (fun f ->
                    let langLabel =
                        f.Corpus
                        |> Corpus.label
                        |> padRight langSize

                    let freq1Col = f.Freq1 |> frequencyCol
                    let freq2Col = f.Freq2 |> frequencyCol
                    let totalCol = f.Total |> frequencyCol
                    
                    $"| {langLabel} | {freq1Col} | {freq2Col} | {totalCol} |"
                )

            let separator =
                let lang = "-" |> String.replicate (langSize + 2)
                let frequency = "-" |> String.replicate (FreqSize + 2)
                $"|{lang}|{frequency}|{frequency}|{frequency}|"

            [
                yield header
                yield! corpusLines
                yield separator
            ]

        digramStats
        |> List.collect getLines
        |> String.concat "\n"

module HomeRow =
    let left = [ "a"; "i"; "e"; "u" ]
    let right = [ "t"; "n"; "s"; "r" ]

["w"; "x"]
|> List.collect (
    [
        yield! HomeRow.right
        yield "c"
        yield "l"
    ]
    |> DigramStat.compareStrings
)
|> List.sortByDescending (fun d ->
    d.TotalFrequencyByCorpus
    |> Seq.sumBy _.Value
)
|> DigramStat.table
|> printfn "%s"