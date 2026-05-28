[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Twine.Twee.FSharp.Printer.PassageHeader
open FsharpMyExtension.Serialization.Serializers.ShowList

open Twine.Twee.FSharp

let shows (header: PassageHeader) : ShowS =
    showString "::" << showSpace
    << showString header.Name
    << (header.Tags
        |> Option.map (fun tags ->
            showSpace << PassageTags.shows tags
        )
        |> Option.defaultValue empty)
    << (header.Metadata
        |> Option.map (fun metadata ->
            showSpace << PassageMetadata.shows metadata
        )
        |> Option.defaultValue empty)
