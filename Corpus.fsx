type Language = English | French | CSharp
module Language =
    let toString =
        function
        | English -> "English"
        | French -> "Français"
        | CSharp -> "C#"

type Corpus = {
    Name: string
    FileName: string
    Language: Language
}
type CorpusNovel = {
    Name: string
    English: Corpus
    French: Corpus
}
module Corpus =
    module CSharp =
        let aspnet = {
            Name = "Aspnet"
            FileName = "aspnet_csharp"
            Language = CSharp
        }
        let automapper = {
            Name = "AutoMapper"
            FileName = "automapper_csharp"
            Language = CSharp
        }
    
    module Novel =
        let whiteFang =
            {
                Name = "White Fang"
                English = {
                    Name = "White Fang"
                    FileName = "white-fang.en"
                    Language = English
                }
                French = {
                    Name = "Croc Blanc"
                    FileName = "white-fang.fr"
                    Language = French
                }
        }

        let donQuixote =
            {
                Name = "Don Quixote"
                English = {
                    Name = "Don Quixote"
                    FileName = "en"
                    Language = English
                }
                French = {
                    Name = "Don Quichote"
                    FileName = "fr"
                    Language = French
                }
        }

