[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Twine.Twee.FSharp.Printer.PassageTags
open FsharpMyExtension.Serialization.Serializers.ShowList

open Twine.Twee.FSharp
open Twine.Twee.FSharp.Printer

let shows (tags: PassageTags) : ShowS =
    between (showChar '[') (showChar ']') (
        tags
        |> Seq.map PassageTag.shows
        |> List.ofSeq
        |> joinsEmpty showSpace
    )
