[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Twine.Twee.FSharp.Parser.PassageTags
open FParsec

open Twine.Twee.FSharp
open Common

let parser: PassageTags Parser =
    between
        (pchar '[' >>. spaces)
        (pchar ']')
        (many (PassageTag.parser .>> spaces))
    |>> Set.ofList
