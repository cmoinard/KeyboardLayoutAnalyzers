open System.IO
open System.Text.RegularExpressions

let stringJoin separator (strings: string list) =
    System.String.Join(separator, strings)

type Pourcent = Pourcent of decimal
let (~%) pourcent = Pourcent (pourcent / 100m)

module Pourcent =
    let toString decimales (Pourcent p) =
        let formatEntier = "##0"
        let format =
            match decimales with
            | 0 -> formatEntier
            | _ -> 
                formatEntier
                + "."
                + String.replicate decimales "0"

        let str = (p * 100m).ToString format
        $"{str}%%"

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
        | Gauche -> "g"
        | Droite -> "d"

type FrappeLateralisee = FrappeLateralisee of PositionMain * Cote
module FrappeLateralisee =
    let toString (FrappeLateralisee (pos, cote)) =
        let strPos = PositionMain.toString pos
        match cote with
        | Gauche -> $"<{strPos}"
        | Droite -> $"{strPos}>"
        
    let getCote (FrappeLateralisee (_, cote)) = cote

type Frappe =
    | Frappe of FrappeLateralisee
    | Espace

module Frappe =
    let getCote =
        function
        | Espace -> None
        | Frappe f -> FrappeLateralisee.getCote f |> Some

    let toString frappe =
        match frappe with
        | Espace -> "space"
        | Frappe f -> FrappeLateralisee.toString f

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
    let espace (toucheMorte: Frappe) =  
        [
            PositionLettre (" ", [Espace])
            PositionLettre (" ", [Espace])
            PositionLettre ("’", [toucheMorte; Espace])
        ]

    let generer cote ligne doigt ext (toucheMorte: Frappe) (touche: Touche) =
        let frappe =
            (PositionMain (doigt, ligne, ext), cote)
            |> FrappeLateralisee
            |> Frappe

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
            yield! touche.Morte |> genererLettres [toucheMorte; frappe]
            yield! touche.MorteMaj |> genererLettres [toucheMorte; frappe]
        ]


type Disposition = {
    Nom: string
    Lettres: PositionLettre list
}
module Disposition =
    let extraireNom lignes =
        let regexName = Regex @"name\s+=\s+""(?<Name>.*)"""

        let matchName =
            lignes
            |> Seq.map regexName.Match
            |> Seq.find _.Success

        matchName.Groups["Name"].Value
    
    let positionsLigne = [
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

    let toucheEspace strLignes =
        let regex = Regex """(?<alteration>\w+)\s+=\s+"(<key>.+)".*"""
        let lignes =
            strLignes
            |> Array.skipWhile ((<>) "[spacebar]")
            |> Array.skip 1
            |> Array.map regex.Match
            |> Array.filter _.Success
            |> Array.map (fun m -> {|
                Alteration = m.Groups["alteration"].Value
                Lettre = m.Groups["key"].Value
            |})
            |> Array.toList

        let lettreAvecAlteration alt =
            lignes
            |> List.tryFind (fun l -> l.Alteration = alt)
            |> Option.map _.Lettre
        
        {
            Normal = Some (Lettre " ")
            AvecMaj = lettreAvecAlteration "shift"
            Morte = lettreAvecAlteration "1dk"
            MorteMaj = lettreAvecAlteration "1dk_shift"
        }


    let touchesParLignes strLignes =
        let lignes =
            strLignes
            |> Array.skipWhile ((<>) "base = '''")
            |> Array.skip 1
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
    
    let frappeToucheMorte (mapTouchesParLigne: Map<Ligne, Touche list>) =
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
            
            (PositionMain (doigt, ligne, ext), cote)
            |> FrappeLateralisee
            |> Frappe

    let parse (tomlText: string) =
        let strLignes = tomlText.Split "\n"

        let mapTouchesParLigne =
            touchesParLignes strLignes

        let toucheMorte =
            frappeToucheMorte mapTouchesParLigne

        let espace =
            PositionLettre.espace toucheMorte

        let lettres =
            [
                yield! espace

                for ligne in [Haut; Milieu; Bas] do
                    let touches = mapTouchesParLigne[ligne]

                    yield!
                        positionsLigne
                        |> List.mapi (fun i (cote, doigt, ext) ->
                            PositionLettre.generer cote ligne doigt ext toucheMorte touches[i]
                        )
                        |> List.collect id
            ]
       
        {
            Nom = extraireNom strLignes
            Lettres = lettres
        }

    let frappes s disposition =
        disposition.Lettres
        |> List.find (fun (PositionLettre (l,_)) -> s = l)
        |> fun (PositionLettre (_, frappes)) -> frappes

type Repartition = {
    Gauche: Pourcent
    Droite: Pourcent
}
module Repartition =
    let toString repartition =
        let gauche =
            repartition.Gauche
            |> Pourcent.toString 1
        
        let droite =
            repartition.Droite
            |> Pourcent.toString 1

        $"{gauche}-{droite}"


    let tendance repartition =
        if repartition.Gauche > repartition.Droite then
            Gauche, repartition.Gauche
        else
            Droite, repartition.Droite

type Alternance = Alternance of (int * Cote option) list
module Alternance =
    let repartition (Alternance series) =
        let frappesDroite =
            series
            |> List.filter (snd >> (=) (Some Droite))
            |> List.sumBy fst
        let frappesGauche =
            series
            |> List.filter (snd >> (=) (Some Gauche))
            |> List.sumBy fst

        let total = frappesDroite + frappesGauche

        let pourcent frappes =
            decimal frappes / decimal total
            |> Pourcent

        {
            Gauche = pourcent frappesGauche
            Droite = pourcent frappesDroite
        }

        
    let toString (Alternance alternance) =
        alternance
        |> List.map (fun (n, cote) ->
            match cote with
            | None -> "_"
            | Some c ->
                let strCote = Cote.toString c
                $"{n}{strCote}"
        )
        |> stringJoin ""


type Sequence = {
    Texte: string
    Frappes: Frappe list
}
module Sequence =
    let afficherFrappes sequence =
        sequence.Frappes
        |> List.map Frappe.toString
        |> stringJoin " "

    let from disposition (str: string) : Sequence =
        {
            Texte = str
            Frappes =
                str
                |> Seq.collect (fun c ->
                    disposition
                    |> Disposition.frappes (string c)
                )
                |> Seq.toList
        }

    let alternance (sequence: Sequence) =
        sequence.Frappes
        |> splitWhenValueChanged Frappe.getCote
        |> List.map (fun (cote, frappes) ->
            frappes.Length, cote
        )
        |> Alternance


// Mots du corpus

type Langue = En | Fr
module Langue =
    let codeFichier =
        function
        | En -> "en"
        | Fr -> "fr"

    let libelle =
        function
        | En -> "English"
        | Fr -> "Français"

type OccurenceParMot = {
    Mot: string
    Occurrences: int
}

type Corpus = {
    Langue: Langue
    OccurrencesParMot: OccurenceParMot list
    TotalMots: int
}
module Corpus =
    let motsDePlusDe n corpus =
        { corpus with
            OccurrencesParMot =
                corpus.OccurrencesParMot
                |> List.filter (fun o ->
                    o.Mot.Length >= n
                )
        }

    let prendre n corpus = 
        let occurrences =
            corpus.OccurrencesParMot
            |> List.take n
        { corpus with
            OccurrencesParMot = occurrences
            TotalMots = 
                occurrences
                |> List.sumBy _.Occurrences
        }

    let afficher corpus =
        corpus.Langue
        |> Langue.libelle
        |> printfn "%s"

        printfn "------"

        corpus.OccurrencesParMot
        |> List.iter (fun o ->
            printfn "%s (%i)" o.Mot o.Occurrences
        )

    let charger langue =
        let nomFichier =
            Langue.codeFichier langue

        let mots =
            $"./corpus/{nomFichier}.txt"
            |> File.ReadAllText
            |> fun text -> Regex.Split(text, @"\s+")

        let occurrences =
            mots
            |> Array.map (fun s -> s.ToLowerInvariant())
            |> Array.groupBy id
            |> Array.map (fun (mot, g) -> {
                Mot = mot
                Occurrences = g.Length
            })
            |> Array.sortByDescending _.Occurrences
            |> Array.toList

        {
            Langue = langue
            OccurrencesParMot = occurrences
            TotalMots = occurrences |> List.sumBy _.Occurrences
        }

type StatsAlternance = {
    Alternance: Alternance
    Repartition: Repartition
    Tendance: Cote * Pourcent
}
module StatsAlternance =
    let analyser alternance =
        let repartition = alternance |> Alternance.repartition
        {
            Alternance = alternance
            Repartition = repartition
            Tendance = repartition |> Repartition.tendance
        }

type StatsMotDisposition = {
    Mot: string
    Occurrences: int
    Sequence: Sequence
    Alternance: StatsAlternance
}
module StatsMotDisposition =
    let analyser disposition (o: OccurenceParMot) =
        let sequence =
            o.Mot
            |> Sequence.from disposition

        {
            Mot = o.Mot
            Occurrences = o.Occurrences
            Sequence = sequence
            Alternance =
                sequence
                |> Sequence.alternance
                |> StatsAlternance.analyser
        }

type StatsCorpusDisposition = {
    Disposition: Disposition
    Corpus: Corpus
    StatsParMot: StatsMotDisposition list
}
module StatsCorpusDisposition =
    let alternanceAuDela seuil stats =
        { stats with
            StatsParMot =
                stats.StatsParMot
                |> List.filter (fun s ->
                    let (Alternance series) = s.Alternance.Alternance
                    let total = series |> List.sumBy fst
                    
                    let ratio =
                        decimal series.Length / decimal total
                        |> Pourcent

                    seuil <= ratio
                )
        }

    let alternanceApres n stats =
        let mots =
            stats.StatsParMot
            |> List.sortByDescending (fun s ->
                let (Alternance series) = s.Alternance.Alternance
                series |> List.maxBy fst
            )
            |> List.filter (fun s ->
                let (Alternance series) = s.Alternance.Alternance
                series |> List.exists (fun (nombre, _) -> nombre >= n)
            )
        { stats with
            StatsParMot = mots
        }

    let filtrerParRepartition seuil stats =
        { stats with
            StatsParMot =
                stats.StatsParMot
                |> List.sortByDescending _.Alternance.Repartition
                |> List.filter (fun r ->
                    snd r.Alternance.Tendance >= seuil
                )
        }

    let afficher stats =
        let langue =
            stats.Corpus.Langue
            |> Langue.libelle

        let mots = stats.StatsParMot.Length
        let total = stats.StatsParMot |> List.sumBy _.Occurrences

        printfn "%s - %s" stats.Disposition.Nom langue
        printfn "%i mots (%i au total)" mots total
        printfn "------"

        stats.StatsParMot
        |> List.iter (fun s ->
            printfn "%s (%i occ.)" s.Mot s.Occurrences

            s.Sequence
            |> Sequence.afficherFrappes
            |> printfn "\tSéquence : %s"

            s.Alternance.Repartition
            |> Repartition.toString
            |> printfn "\tRépartition : %s"

        )

    let analyser disposition corpus =
        {
            Disposition = disposition
            Corpus = corpus
            StatsParMot =
                corpus.OccurrencesParMot
                |> List.map (StatsMotDisposition.analyser disposition)
        }



let erglace =
    @"/home/chris/Programmes/Erglace/erglace.toml"
    |> File.ReadAllText
    |> Disposition.parse

let ergol =
    @"/home/chris/Programmes/ergol/keymaps/fr/ergol.toml"
    |> File.ReadAllText
    |> Disposition.parse


let corpusFr = Corpus.charger Fr

let corpusFrReduit =
    corpusFr
    |> Corpus.motsDePlusDe 5
    |> Corpus.prendre 200

let stats disposition =
    corpusFrReduit
    |> StatsCorpusDisposition.analyser disposition


let statsErglace = stats erglace
let statsErgol = stats ergol

type ComparaisonRepartition = {
    Erglace: StatsCorpusDisposition
    Ergol: StatsCorpusDisposition
}
module ComparaisonRepartition =
    let afficher comparaison =
        comparaison.Erglace
        |> StatsCorpusDisposition.afficher

        comparaison.Ergol
        |> StatsCorpusDisposition.afficher

    let comparer filtrer =
        {
            Erglace = filtrer statsErglace
            Ergol = filtrer statsErgol
        }

// alternance à plus de 80%
ComparaisonRepartition.comparer (
    StatsCorpusDisposition.alternanceAuDela %80m 
)
|> ComparaisonRepartition.afficher


// alternance après 4 frappes
ComparaisonRepartition.comparer (
    StatsCorpusDisposition.alternanceApres 4
)
|> ComparaisonRepartition.afficher

// repartition au dessus 80%
ComparaisonRepartition.comparer (
    StatsCorpusDisposition.filtrerParRepartition %80m
)
|> ComparaisonRepartition.afficher
