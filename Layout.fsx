#r "nuget: FSharp.Data"

open System.Text.RegularExpressions

type Finger = Index | Middle | Ring | Pinkie
module Finger =
    let toString =
        function
        | Index -> "1"
        | Middle -> "2"
        | Ring -> "3"
        | Pinkie -> "4"

type Row = HomeRow | UpperRow | LowerRow
module Row =
    let toString =
        function
        | HomeRow -> ""
        | UpperRow -> "↑"
        | LowerRow -> "↓"

type Extension = Extension of int
module Extension =
    let toString (Extension e) =
        String.replicate e "E"

type HandPosition = HandPosition of Finger * Row * Extension option
module HandPosition =
    let toString (HandPosition (finger, row, extension)) =
        let strFinger = Finger.toString finger
        let strRow = Row.toString row
        let strExt =
            extension
            |> Option.map Extension.toString
            |> Option.defaultValue ""
        strFinger + strRow + strExt

type Side = Left | Right
module Side =
    let toString =
        function
        | Left -> "←"
        | Right -> "→"

type SidedInput = SidedInput of HandPosition * Side
module SidedInput =
    let toString (SidedInput (pos, side)) =
        let strPos = HandPosition.toString pos
        match side with
        | Left -> $"<{strPos}"
        | Right -> $"{strPos}>"
        
    let getSide (SidedInput (_, side)) = side

type Input =
    | Input of SidedInput
    | Space

module Input =
    let getSide =
        function
        | Space -> None
        | Input i -> SidedInput.getSide i |> Some

    let toString input =
        match input with
        | Space -> "space"
        | Input i -> SidedInput.toString i

type KeyType =
    | Symbol of string
    | DeadKey

type Key = {
    Normal: KeyType option
    Shifted: string option
    Dead: string option
    ShiftedDead: string option 
}
module Key =
    let parse (sUpper: string) (sLower: string) : Key =
        let noneIfEmpty (s: string) =
            match s.Trim() with
            | "" -> None
            | sTrimmed -> Some sTrimmed

        let key =
            if sLower[1..].StartsWith "***" then
                {
                    Normal = Some DeadKey
                    Shifted = noneIfEmpty sUpper[1..2]
                    Dead = noneIfEmpty sLower[4..]
                    ShiftedDead = noneIfEmpty sUpper[4..]
                }
            else
                {
                    Normal = noneIfEmpty sLower[1..2] |> Option.map Symbol
                    Shifted = noneIfEmpty sUpper[1..2]
                    Dead = noneIfEmpty sLower[3..]
                    ShiftedDead = noneIfEmpty sUpper[3..]
                }

        match key with
        | { Normal = None; Shifted = Some v } ->
            { key with Normal = Some (Symbol (v.ToLowerInvariant())) }
        | _ -> key

type SymbolPath = SymbolPath of string * (Input list)
module SymbolPath =   
    let space (deadKey: Input) =  
        [
            SymbolPath (" ", [Space])
            SymbolPath (" ", [Space])
            SymbolPath ("’", [deadKey; Space])
        ]

    let generate side row finger ext (deadKey: Input) (key: Key) =
        let input =
            (HandPosition (finger, row, ext), side)
            |> SidedInput
            |> Input

        let generateSymbols inputs symbol =
            symbol
            |> Option.map (fun l -> [SymbolPath (l, inputs)])
            |> Option.defaultValue []

        let normal =
            match key.Normal with
            | Some (Symbol l) ->
                [SymbolPath (l, [input])]
            | _ -> []
            
        [
            yield! normal
            yield! key.Shifted |> generateSymbols [input]
            yield! key.Dead |> generateSymbols [deadKey; input]
            yield! key.ShiftedDead |> generateSymbols [deadKey; input]
        ]


type Layout = {
    Name: string
    Ngrams: SymbolPath list
}
module Layout =
    let extractName lines =
        let regexName = Regex @"name\s+=\s+""(?<Name>.*)"""

        let matchName =
            lines
            |> Seq.map regexName.Match
            |> Seq.find _.Success

        matchName.Groups["Name"].Value
    
    let positionsInRow = [
        Left, Pinkie, None
        Left, Ring, None
        Left, Middle, None
        Left, Index, None
        Left, Index, Some (Extension 1)
        Right, Index, Some (Extension 1)
        Right, Index, None
        Right, Middle, None
        Right, Ring, None
        Right, Pinkie, None
    ]

    let spaceKey strLines =
        let regex = Regex """(?<alteration>\w+)\s+=\s+"(<key>.+)".*"""
        let lines =
            strLines
            |> Array.skipWhile ((<>) "[spacebar]")
            |> Array.skip 1
            |> Array.map regex.Match
            |> Array.filter _.Success
            |> Array.map (fun m -> {|
                Alteration = m.Groups["alteration"].Value
                Lettre = m.Groups["key"].Value
            |})
            |> Array.toList

        let symbolWithAlteration alt =
            lines
            |> List.tryFind (fun l -> l.Alteration = alt)
            |> Option.map _.Lettre
        
        {
            Normal = Some (Symbol " ")
            Shifted = symbolWithAlteration "shift"
            Dead = symbolWithAlteration "1dk"
            ShiftedDead = symbolWithAlteration "1dk_shift"
        }

    let keysByRows strLines =
        let lines =
            strLines
            |> Array.skipWhile ((<>) "base = '''")
            |> Array.skip 1
            |> Array.takeWhile ((<>) "'''")
            |> Array.toList

        let parseKeys (lines: string list) =
            [1..10]
            |> List.map (fun i -> i * 6)
            |> List.map (fun i ->
                Key.parse
                    (lines[0].Substring(i, 6))
                    (lines[1].Substring(i, 6))
            )

        Map [
            UpperRow, parseKeys lines[4..5]
            HomeRow, parseKeys lines[7..8]
            LowerRow, parseKeys lines[10..11]
        ]
    
    let deadKeyInput (keysByRow: Map<Row, Key list>) =
        keysByRow.Values
        |> Seq.collect id
        |> Seq.findIndex (fun t ->
            match t.Normal with
            | Some DeadKey -> true
            | _ -> false
        )
        |> fun i ->
            let side, finger, ext = positionsInRow[i % 10]
            let line =
                match i / 10 with
                | 0 -> UpperRow
                | 2 -> LowerRow
                | _ -> HomeRow
            
            (HandPosition (finger, line, ext), side)
            |> SidedInput
            |> Input

    let parse (tomlText: string) =
        let strLines = tomlText.Split "\n"

        let keysByRows = keysByRows strLines
        let deadKey = deadKeyInput keysByRows

        let space = SymbolPath.space deadKey

        let ngrams =
            [
                yield! space

                for row in [UpperRow; HomeRow; LowerRow] do
                    let keys = keysByRows[row]

                    yield!
                        positionsInRow
                        |> List.mapi (fun i (side, finger, ext) ->
                            SymbolPath.generate side row finger ext deadKey keys[i]
                        )
                        |> List.collect id
            ]
       
        {
            Name = extractName strLines
            Ngrams = ngrams
        }

    let inputs s layout =
        layout.Ngrams
        |> List.tryFind (fun (SymbolPath (ng,_)) -> s = ng)
        |> Option.map (fun (SymbolPath (_, inputs)) -> inputs)
        |> Option.defaultValue []
