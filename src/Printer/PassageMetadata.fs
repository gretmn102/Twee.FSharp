[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Twine.Twee.FSharp.Printer.PassageMetadata
open FsharpMyExtension.Serialization.Serializers.ShowList

open Twine.Twee.FSharp

let shows (metadata: PassageMetadata) : ShowS =
    between (showChar '{') (showChar '}') (
        showString metadata
    )
