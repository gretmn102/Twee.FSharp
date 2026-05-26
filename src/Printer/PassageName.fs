[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Twine.Twee.FSharp.Printer.PassageName
open FsharpMyExtension.Serialization.Serializers.ShowList

open Twine.Twee.FSharp

let shows (passageName: PassageName) =
    showString passageName
