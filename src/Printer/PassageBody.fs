[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Twine.Twee.FSharp.Printer.PassageBody
open FsharpMyExtension.Serialization.Serializers.ShowList

open Twine.Twee.FSharp
open Twine.Twee.FSharp.Printer

let shows newlineType (passageBody: PassageBody) =
    let newline =
        showString <| NewlineType.toString newlineType
    passageBody
    |> List.map showString
    |> joinsEmpty newline
