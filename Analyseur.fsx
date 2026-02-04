type Doigt = Index | Majeur | Annulaire | Auriculaire
module Doigt =
    let toString =
        function
        | Index -> "1"
        | Majeur -> "2"
        | Annulaire -> "3"
        | Auriculaire -> "4"

type Ligne = Milieu | Haut | Bas
module Ligne =
    let toString =
        function
        | Milieu -> ""
        | Haut -> "H"
        | Bas -> "B"

type Extension = Extension of int
module Extension =
    let toString (Extension e) =
        String.replicate e "E"

type PositionMain = PositionMain of Doigt * Ligne * Extension option
module PositionMain =
    let toString (PositionMain (doigt, ligne, extension)) =
        let strDoigt = Doigt.toString doigt
        let strLigne = Ligne.toString ligne
        let strExt =
            extension
            |> Option.map Extension.toString
            |> Option.defaultValue ""
        strDoigt + strLigne + strExt

type Cote = Gauche | Droite
module Cote =
    let toString =
        function
        | Gauche -> "G"
        | Droite -> "D"

type Frappe = Frappe of PositionMain * Cote
module Frappe =
    let toString (Frappe (pos, cote)) =
        let strCote = Cote.toString cote
        let strPos = PositionMain.toString pos
        strCote + strPos

type TypeTouche =
    | Lettre of string
    | LettreMorte

type Touche = {
    Normal: TypeTouche option
    AvecMaj: string option
    Morte: string option
    MorteMaj: string option 
}
module Touche =
    let parse (sHaut: string) (sBas: string) : Touche =
        let noneIfEmpty (s: string) =
            match s.Trim() with
            | "" -> None
            | sTrimmed -> Some sTrimmed

        let touche =
            if sBas[1..].StartsWith "***" then
                {
                    Normal = Some LettreMorte
                    AvecMaj = noneIfEmpty sHaut[1..2]
                    Morte = noneIfEmpty sBas[4..]
                    MorteMaj = noneIfEmpty sHaut[4..]
                }
            else
                {
                    Normal = noneIfEmpty sBas[1..2] |> Option.map Lettre
                    AvecMaj = noneIfEmpty sHaut[1..2]
                    Morte = noneIfEmpty sBas[3..]
                    MorteMaj = noneIfEmpty sHaut[3..]
                }

        match touche with
        | { Normal = None; AvecMaj = Some v } ->
            { touche with Normal = Some (Lettre (v.ToLowerInvariant())) }
        | _ -> touche

type PositionLettre = PositionLettre of string * (Frappe list)

module PositionLettre =

    let generer cote ligne doigt ext (toucheMorte: Frappe) (touche: Touche) =
        let frappe = Frappe (PositionMain (doigt, ligne, ext), cote)

        let genererLettres frappes lettre =
            lettre
            |> Option.map (fun l -> [PositionLettre (l, frappes)])
            |> Option.defaultValue []

        let normal =
            match touche.Normal with
            | Some (Lettre l) ->
                [PositionLettre (l, [frappe])]
            | _ -> []
            
        [
            yield! normal
            yield! touche.AvecMaj |> genererLettres [frappe]
            yield! touche.Morte |> genererLettres [frappe; toucheMorte]
            yield! touche.MorteMaj |> genererLettres [frappe; toucheMorte]
        ]


type Disposition = {
    Nom: string
    Lettres: PositionLettre list
}
module Disposition =
    let private extraireNom lignes =
        let regexName = System.Text.RegularExpressions.Regex @"name\s+=\s+""(?<Name>.*)"""

        let matchName =
            lignes
            |> Seq.map regexName.Match
            |> Seq.find _.Success

        matchName.Groups["Name"].Value
    
    let private positionsLigne = [
        Gauche, Auriculaire, None
        Gauche, Annulaire, None
        Gauche, Majeur, None
        Gauche, Index, None
        Gauche, Index, Some (Extension 1)
        Droite, Index, Some (Extension 1)
        Droite, Index, None
        Droite, Majeur, None
        Droite, Annulaire, None
        Droite, Auriculaire, None
    ]

    let private touchesParLignes strLignes =
        let lignes =
            strLignes
            |> Array.skipWhile ((<>) "base = '''")
            |> Array.takeWhile ((<>) "'''")
            |> Array.toList

        let parseTouches (lignes: string list) =
            [1..10]
            |> List.map (fun i -> i * 6)
            |> List.map (fun i ->
                Touche.parse
                    (lignes[0].Substring(i, 6))
                    (lignes[1].Substring(i, 6))
            )

        Map [
            Haut, parseTouches lignes[4..5]
            Milieu, parseTouches lignes[7..8]
            Bas, parseTouches lignes[10..11]
        ]
    
    let private frappeToucheMorte (mapTouchesParLigne: Map<Ligne, Touche list>) =
        mapTouchesParLigne.Values
        |> Seq.collect id
        |> Seq.findIndex (fun t ->
            match t.Normal with
            | Some LettreMorte -> true
            | _ -> false
        )
        |> fun i ->
            let cote, doigt, ext = positionsLigne[i % 10]
            let ligne =
                match i / 10 with
                | 0 -> Haut
                | 2 -> Bas
                | _ -> Milieu
            
            Frappe (PositionMain (doigt, ligne, ext), cote)

    let parse (tomlText: string) =
        let strLignes = tomlText.Split "\n"

        let mapTouchesParLigne =
            touchesParLignes strLignes

        let toucheMorte =
            frappeToucheMorte mapTouchesParLigne
        
        let lettres =
            [
                for ligne in [Haut; Milieu; Bas] do
                    for touche in mapTouchesParLigne[ligne] do
                        for cote, doigt, ext in positionsLigne do
                            yield!
                                touche
                                |> PositionLettre.generer cote ligne doigt ext toucheMorte
            ]
       
        {
            Nom = extraireNom strLignes
            Lettres = lettres
        }

open System.IO

let erglace =
    @"/home/chris/kDrive/Dispositions/Erglace-ng-mirror/erglace-jeanfulbert-0.7.0.toml"
    |> File.ReadAllText
    |> Disposition.parse