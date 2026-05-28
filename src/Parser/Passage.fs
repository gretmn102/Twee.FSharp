[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Twine.Twee.FSharp.Parser.Passage
open FParsec

open Twine.Twee.FSharp
open Common

let parser (pbody: Parser<'Body>) : Passage<'Body> Parser =
    pipe2
        (PassageHeader.parser .>> optional skipNewline)
        pbody
        (fun header body ->
            {
                Header = header
                Body = body
            }
        )
