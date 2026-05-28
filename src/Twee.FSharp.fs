namespace Twine.Twee.FSharp

open Twine.Twee.FSharp.Parser
open Twine.Twee.FSharp.Printer

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Document =
    open FsharpMyExtension.Serialization.Serializers

    module Printer =
        open FsharpMyExtension.Serialization.Serializers.ShowList

        let shows showPassageBody newlineType (document: Document<'PassageBody>) =
            let newline =
                showString <| NewlineType.toString newlineType
            let newlines =
                newline
                << newline << newline // add two empty blanks
            document
            |> List.map (Passage.shows showPassageBody newlineType)
            |> joinsEmpty newlines

    let toString showPassageBody newlineType (document: Document<'PassageBody>) =
        Printer.shows showPassageBody newlineType document
        |> ShowList.show

    let updatePassages update (twee: Document<'PassageBody>) =
        twee
        |> List.mapFold
            (fun changedPassagesCount passage ->
                match update passage with
                | None ->
                    passage, changedPassagesCount
                | Some updatedPassage ->
                    updatedPassage, changedPassagesCount + 1
            )
            0
