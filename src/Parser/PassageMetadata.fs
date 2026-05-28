[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Twine.Twee.FSharp.Parser.PassageMetadata
open FParsec

open Twine.Twee.FSharp
open Common

let parser: PassageMetadata Parser =
    between
        (pchar '{' >>. spaces)
        (pchar '}')
        (manySatisfy ((<>) '}'))
