[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Twine.Twee.FSharp.Printer.PassageTag
open FsharpMyExtension.Serialization.Serializers.ShowList

open Twine.Twee.FSharp
open Twine.Twee.FSharp.Printer

let shows (tag: PassageTag) : ShowS =
    tag.Trim()
    |> showString // todo: add escape \]
