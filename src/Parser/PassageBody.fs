[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Twine.Twee.FSharp.Parser.PassageBody
open FParsec

open Twine.Twee.FSharp
open Twine.Twee.FSharp.Parser.Common

let parser: PassageBody Parser =
    let pline: _ Parser =
        notFollowedByString "::"
        >>? many1Satisfy ((<>) '\n')
    let pemptyBlanks1: _ Parser =
        many1 (newlineReturn "")
        .>>? notFollowedBy (skipString "::" <|> eof)
    many (choice [
        pline .>> skipNewline |>> List.singleton // todo: or eof
        pemptyBlanks1
    ])
    |>> List.concat
