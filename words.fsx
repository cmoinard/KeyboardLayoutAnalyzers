let path lang = $"/home/chris/Programmation/ergol/corpus/{lang}.txt"

let readFile = System.IO.File.ReadAllText

let fullText =
    path "en"
    |> readFile

let regex = System.Text.RegularExpressions.Regex "\S+"

let words =
    regex.Matches fullText
    |> Seq.map _.Value
    |> Seq.toList

let contains (value: string) (s: string) = s.ToLowerInvariant().Contains value

let wordsWithO =
    words
    |> List.filter (contains "o")
    
wordsWithO
|> List.length

let wordsWithoutDonQuixote =
    wordsWithO
    |> List.filter (fun w ->
        w |> contains "don" |> not &&
        w |> contains "quixote" |> not
    )
    |> List.length

//let bigrams = [ "xo"; "ix" ]
let bigrams = [ "z," ]
let wordsWithBigram =
    words
    |> List.filter (fun w ->
        bigrams
        |> List.exists (fun b -> w.Contains b )
    )

printfn "Words with za : %i" wordsWithBigram.Length

let contains (v: string) (s: string) = s.Contains v
let startWithUpper (s: string) =
    let firstLetter = string s[0]
    firstLetter.ToUpper() = firstLetter

let wordsWithBigramNotQuichotte =
    wordsWithBigram
    |> List.filter (fun word ->
        word |> startWithUpper |> not &&
        word |> contains "_" |> not &&
        [
            "lcazar"; "fuerza"; "zanaga"; "bizarria"; "cabeza"
            "destroza"; "proezas"; "bizarría"; "esperanza"; "caza"
            "rolliza"; "ceniza"; "castiza"
        ]
        |> List.exists (fun w ->
            word |> contains w
        )
        |> not
    )
printfn "Words with za, not aldonza nor panza : %i" wordsWithBigramNotQuichotte.Length


wordsWithBigram
|> List.groupBy id
|> List.map (fun (k, items) -> {| Word = k; Count = items.Length |})
|> List.sortByDescending _.Count
|> List.iter (fun a -> printfn "%s : %i" a.Word a.Count)