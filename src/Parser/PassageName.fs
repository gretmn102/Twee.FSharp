[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Twine.Twee.FSharp.Parser.PassageName
open FParsec

open Twine.Twee.FSharp
open Common

let parser: PassageName Parser =
    manySatisfy (isNoneOf "[{\n")
    |>> fun x -> x.TrimEnd() // optimize: remove trailing whitespaces by parser
