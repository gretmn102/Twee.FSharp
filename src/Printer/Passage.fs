[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Twine.Twee.FSharp.Printer.Passage
open FsharpMyExtension.Serialization.Serializers.ShowList

open Twine.Twee.FSharp

let shows showBody newlineType (passage: Passage<'PassageBody>) =
    PassageHeader.shows passage.Header
    << (showString <| NewlineType.toString newlineType)
    << showBody newlineType passage.Body
