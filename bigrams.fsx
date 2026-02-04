#r "nuget: FSharp.Data"

open FSharp.Data

open System.Text

type Symbol = private Symbol of string
module Symbol =
    let from (s: string) =
        s
        |> Encoding.UTF8.GetBytes
        |> Encoding.UTF8.GetString
        |> Symbol

    let toString (Symbol s) = s

let padLeft l (s: string) = s.PadLeft l
let padRight l (s: string) = s.PadRight l


type ColumnData<'a> =
    | ColumnString of ('a -> string)
    | ColumnPercent of ('a -> decimal)

type ColumnDefinition<'a> = {
    Title: string
    Length: int
    ValueGetter: ColumnData<'a>
}
module ColumnDefinition =
    let asString title length getString =
        {
            Title = title
            Length = length
            ValueGetter = ColumnString getString
        }
    let asPercent title length getPercent =
        {
            Title = title
            Length = length
            ValueGetter = ColumnPercent getPercent
        }

    let formatPercent length (d: decimal) =
        d
        |> fun f -> f.ToString "#0.000"
        |> sprintf "%s%%"
        |> padLeft length

    let format item c =
        match c.ValueGetter with
        | ColumnString getString ->
            item
            |> getString
            |> padRight c.Length

        | ColumnPercent getPercent ->
            item
            |> getPercent
            |> formatPercent c.Length

type Table<'a> = {
    Columns: ColumnDefinition<'a> list
    ShowTotal: bool
}
module Table =
    let show (table: Table<'a>) (items: 'a list)  =
        let row toString =
            table.Columns
            |> List.map toString
            |> String.concat " | "
            |> fun h -> $"| {h} |"

        let header =
            row (fun c ->
                c.Title
                |> padRight c.Length
            )
        
        let item item =
            row (ColumnDefinition.format item)

        let total () =
            row (fun c ->
                match c.ValueGetter with
                | ColumnString _ ->
                    "Total" |> padLeft c.Length

                | ColumnPercent getPercent ->
                    items
                    |> List.sumBy getPercent
                    |> ColumnDefinition.formatPercent c.Length
            )
        [
            yield header

            yield! items |> List.map item

            if table.ShowTotal then
                yield total ()                
        ]
        |> String.concat "\n"
        |> printfn "%s"


type Corpus = JsonProvider<"./corpus/en.json">
module Corpus =
    let english = Corpus.Load "./corpus/en.json"
    
    let french = Corpus.Load "./corpus/fr.json"

type SymbolStat = {
    Rank: int
    Frequency: decimal
}
type SymbolFrequency = {
    Symbol: Symbol
    FrStats: SymbolStat option
    EnStats: SymbolStat option
}

let letters =
    "abcdefghijklmnopqrstuvwxyz"
    |> Seq.map (string >> Symbol.from)
    |> Seq.toList

module SymbolFrequency =
    let stats (corpus: Corpus.Root) =
        corpus.Symbols
        |> Array.map (fun c -> {|
            Symbol = Symbol.from c.Char
            CorpusData = c
        |})
        |> Array.sortByDescending _.CorpusData.Frequency
        |> Array.mapi (fun index s ->
            s.Symbol, {
                Rank = index + 1
                Frequency = s.CorpusData.Frequency
            }
        )
        |> Map.ofArray

    let all =        
        let frStats = Corpus.french |> stats
        let enStats = Corpus.english |> stats
        
        [
            yield! frStats.Keys
            yield! enStats.Keys
        ]
        |> List.distinct
        |> List.map (fun c -> {
            Symbol = c
            FrStats = frStats |> Map.tryFind c
            EnStats = enStats |> Map.tryFind c
        })

type Digram = Digram of Symbol * Symbol
module Digram =
    let combinations s1 s2 =
        [
            Digram (s1, s2)
            Digram (s2, s1)
        ]

    let from (s: string) =
        let letters =
            s
            |> Seq.map (string >> Symbol.from)
            |> Seq.toArray
        Digram (letters[0], letters[1])

    let endsWith symbol (Digram (_, s)) =
        s = symbol


    let toString (Digram (s1,  s2)) =
        Symbol.toString s1 + Symbol.toString s2

type DigramCompare = {
    Digram: Digram
    FrequencyFr: decimal
    FrequencyEn: decimal
}

type DigramStat = {
    Symbols: Symbol list
    Comparisons: DigramCompare list
    TotalFrequencyFr: decimal
    TotalFrequencyEn: decimal
}

module DigramStat =
    let private getFrequency (corpus: Corpus.Root) (digram: Digram) =
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
                FrequencyFr = d |> getFrequency Corpus.french
                FrequencyEn = d |> getFrequency Corpus.english
            }
        )

    let compareAll (s1: Symbol) (s2: Symbol) =
        let comparisons = compare s1 s2

        {
            Symbols = [ s1; s2 ]
            Comparisons = comparisons
            TotalFrequencyFr = comparisons |> List.sumBy _.FrequencyFr
            TotalFrequencyEn = comparisons |> List.sumBy _.FrequencyEn
        }

    let compareStrings (symbols: string list) (s: string) =
        let symbol = Symbol.from s

        symbols
        |> List.map (Symbol.from >> compareAll symbol)

    let compareSymbols (symbols: Symbol list) (symbol: Symbol) =
        symbols
        |> List.map (compareAll symbol)


    let table hasSeparator digrams =
        let frequencyCol (f: decimal) =
            f.ToString "#0.000"
            |> sprintf "%s%%"
            |> padLeft 8

        let digramCompareLines (digrams: DigramCompare list) =
            let line d =
                let letters = d.Digram |> Digram.toString |> padRight 6
                let fr = d.FrequencyFr |> frequencyCol
                let en = d.FrequencyEn |> frequencyCol
                $"| {letters} | {fr} | {en} |"

            let d1 = digrams[0]
            let d2 = digrams[1]

            let line1 = line d1 + $"          |          |"
            let line2 =
                let total =
                    let totalFr = d1.FrequencyFr + d2.FrequencyFr |> frequencyCol
                    let totalEn = d1.FrequencyEn + d2.FrequencyEn |> frequencyCol
                    
                    $" {totalFr} | {totalEn} |"
                
                line d2 + total

            [
                yield line1
                yield line2
                if hasSeparator then
                    yield "|--------|----------|----------|----------|----------|"
            ]

        
        [
            yield "| Digram | Fr       | En       | Total Fr | Total En |"

            yield!
                digrams
                |> List.collect (fun d ->
                    d.Comparisons
                    |> digramCompareLines
                )

        ]
        |> String.concat "\n"

module HomeRow =
    let left = [ "a"; "i"; "e"; "u" ]
    let right = [ "t"; "n"; "s"; "r" ]


let digramTable : Table<DigramCompare> =
    {
        Columns = [
            ColumnDefinition.asString "Digram" 6 (fun c -> c.Digram |> Digram.toString)
            ColumnDefinition.asPercent "Fr" 6 _.FrequencyFr
            ColumnDefinition.asPercent "En" 6 _.FrequencyEn
        ]
        ShowTotal = true
    }




["q"]
|> List.collect (
    DigramStat.compareStrings [
        "s"
        "n"
        //yield! HomeRow.right
    ])
|> List.sortByDescending (fun d ->
    d.Symbols
    //d.TotalFrequencyEn + d.TotalFrequencyFr
)
|> DigramStat.table false
|> printfn "%s"