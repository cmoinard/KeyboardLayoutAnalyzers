open System

// Numbers

let average (n: int) (total: int) =
    Math.Round(decimal n / decimal total, 6) * 100m

// String
let joinWith (separator: string) (strings: string list) =
    String.Join(separator, strings)

let padLeft l (s: string) = s.PadLeft l
let padRight l (s: string) = s.PadRight l

// Percent
type Percent = Percent of decimal
let (~%) percent = Percent (percent / 100m)
module Percent =
    let toString decimals (Percent p) =
        let intFormat = "##0"
        let format =
            match decimals with
            | 0 -> intFormat
            | _ -> 
                intFormat
                + "."
                + String.replicate decimals "0"

        let str = (p * 100m).ToString format
        $"{str}%%"

// Collections
let splitWhenValueChanged (getValue: 'a -> 'b) (l: 'a list) : ('b * 'a list) list  =
    let rec loop groups list =
        match list with
        | [] -> groups
        | head::tail ->
            let value = getValue head
            let newGroups =
                match List.tryLast groups with
                | None -> [value, [head]]
                | Some (v, _) when v <> value ->
                    let newLastGroup = value, [head]
                    groups @ [newLastGroup]                    
                | Some (_, currentGroup) ->
                    let otherGroups =
                        groups
                        |> List.take (groups.Length - 1)

                    let newLastGroup = value, currentGroup @ [head]
                    otherGroups @ [newLastGroup]
            
            loop newGroups tail
        
    loop [] l

