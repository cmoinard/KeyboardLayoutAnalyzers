#load "./Utils.fsx"

open Utils
open System
open System.IO
open System.Text.RegularExpressions

let getWords
    (splitWords: (string -> string list) option)
    path =

    let lines =
        path
        |> File.ReadAllLines
        |> Array.filter (String.IsNullOrWhiteSpace >> not)

    let rgxAllWords = Regex @"\w+"
    let rgxWords = Regex @"[^\d_]+"

    let words =
        lines
        |> Seq.collect (fun l -> rgxAllWords.Matches l)
        |> Seq.collect (fun m -> rgxWords.Matches m.Value)
        |> Seq.map _.Value
        |> Seq.toList

    let extraWords =
        match splitWords with
        | None -> words
        | Some f -> words |> List.collect f

    extraWords
    |> Seq.map _.ToLowerInvariant()
    |> Seq.toList


let statsText path =
    let separators = @"[\s\d""\[\]*↑»«°]"

    path
    |> File.ReadAllLines
    |> Array.collect (fun l -> Regex.Split(l, separators))
    |> Array.filter (String.IsNullOrWhiteSpace >> not)
    |> Array.map _.ToLowerInvariant()
    |> Array.toList


let statsCsharpFiles folderPath =
    let csFiles =
        Directory.GetFiles(folderPath, "*.cs", SearchOption.AllDirectories)
        |> Array.filter (fun cs ->
            cs.EndsWith("")
        )
        |> Array.toList

    let splitPascalCase (s: string) =
        let indexesWithUpperCase =
            s
            |> Seq.mapi (fun i char -> i, char)
            |> Seq.filter (snd >> Char.IsUpper)
            |> Seq.map fst
            |> Seq.append [0; s.Length]
            |> Seq.distinct
            |> Seq.sort
            |> Seq.toList
        
        indexesWithUpperCase
        |> List.pairwise
        |> List.map (fun (index, nextIndex) ->
            let length = nextIndex - index
            s.Substring(index, length)
        )

    csFiles
    |> List.collect (getWords (Some splitPascalCase))

type NgramCount = {
    Ngram: string
    Count: int
}
module NgramCount =        
    let splitToNgrams n ngramCount =
        [0..ngramCount.Ngram.Length - n]
        |> List.map (fun i -> {
            Ngram = ngramCount.Ngram.Substring(i, n)
            Count = ngramCount.Count
        })

    let merge ngramCounts =
        ngramCounts
        |> List.groupBy _.Ngram
        |> List.map (fun (ngram, ngrams) -> {
            Ngram = ngram
            Count = ngrams |> List.sumBy _.Count
        })


type NgramStats = {
    Ngram: string
    Count: int
    Frequency: decimal
}
module NgramStats =
    let toStats total (nc: NgramCount) = {
        Ngram = nc.Ngram
        Count = nc.Count
        Frequency = average nc.Count total
    }

    let generate (ngramCounts: NgramCount list) =
        let total = ngramCounts |> List.sumBy _.Count

        ngramCounts
        |> List.map (toStats total)
        |> List.sortByDescending _.Frequency

      
    let toNgrams n (wordCounts: NgramCount list) =
        let splittedNgrams =
            wordCounts
            |> List.filter (fun wc -> wc.Ngram.Length >= n)
            |> List.collect (NgramCount.splitToNgrams n)
            |> NgramCount.merge
        
        let total = splittedNgrams |> List.sumBy _.Count
        
        splittedNgrams
        |> List.map (toStats total)
        |> List.sortByDescending _.Frequency

    let toJson name tabsCount ngramFrequencies =
        let tabs = "\t" |> String.replicate tabsCount

        ngramFrequencies
        |> List.map (fun nf ->
            sprintf
                "%s{ \"%s\": \"%s\", \"frequency\": %s, \"count\": %i }"
                tabs
                name
                nf.Ngram
                (nf.Frequency |> sprintf "%.4f")
                nf.Count
        )
        |> joinWith ",\n"

type Stats = {
    Words: NgramStats list
    Symbols: NgramStats list
    Digrams: NgramStats list
    Trigrams: NgramStats list
}

module Stats =
    let generate wordCounts =
        {
            Symbols = wordCounts |> NgramStats.toNgrams 1
            Digrams = wordCounts |> NgramStats.toNgrams 2
            Trigrams = wordCounts |> NgramStats.toNgrams 3
            Words = wordCounts |> NgramStats.generate
        }
    
    let toJson corpusPath stats =
        let symbols =
            stats.Symbols
            |> NgramStats.toJson "char" 2
        
        let bigrams =
            stats.Digrams
            |> NgramStats.toJson "digram" 2
        
        let trigrams =
            stats.Trigrams
            |> NgramStats.toJson "trigram" 2
        
        let words =
            stats.Words
            |> NgramStats.toJson "word" 2

        $"""
{{
    "corpus": "{corpusPath}",
    "symbols": [
{symbols}
    ],
    "digrams": [
{bigrams}
    ],
    "trigrams": [
{trigrams}
    ],
    "words": [
{words}
    ]
}}
        """

type CorpusSource = {
    Origin: string
    Path: string
    Destination: string
}

let generate getWords source =
    let wordCounts =
        source.Path
        |> getWords
        |> List.groupBy id
        |> List.map (fun (word, words) -> {
            Ngram = word
            Count = words.Length
        })
        |> List.sortByDescending _.Count

    wordCounts
    |> Stats.generate
    |> Stats.toJson source.Origin
    |> fun json -> File.WriteAllText(source.Destination, json)
(*
let source =
    {
        Path = "./corpus/white-fang.en.txt"
        Origin = "https://en.wikisource.org/wiki/White-Fang"
        Destination = "./corpus/white-fang.en.json"
    }

source
|> generate statsText

{
    Path = 
    Origin = "https://github.com/dotnet/aspnetcore"
    Destination = "./corpus/aspnet.csharp.json"
}
|> generate statsCsharpFiles
*)