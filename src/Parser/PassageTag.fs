[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Twine.Twee.FSharp.Parser.PassageTag
open FParsec

open Twine.Twee.FSharp
open Common

let parser: PassageTag Parser =
    many1Satisfy (isNoneOf " ]") // todo: add escape \]
    |>> fun x -> x.TrimEnd() // optimize: remove trailing whitespaces by parser
