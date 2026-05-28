[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Twine.Twee.FSharp.Printer.Document
open FsharpMyExtension.Serialization.Serializers.ShowList

open Twine.Twee.FSharp

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
    shows showPassageBody newlineType document
    |> show
