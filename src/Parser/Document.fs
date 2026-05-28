[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Twine.Twee.FSharp.Parser.Document
open FParsec
open FsharpMyExtension.Serialization.Deserializers

open Twine.Twee.FSharp
open Common

let parser (ppassageBody: Parser<'PassageBody>) : Document<'PassageBody> Parser =
    many (Passage.parser ppassageBody .>> spaces)

let parse ppassageBody (rawTwee: string) =
    FParsec.runResult (parser ppassageBody) rawTwee

let rawParse =
    parse PassageBody.parser

let parseFile ppassageBody (rawTwee: string) =
    runParserOnFile
        (parser ppassageBody)
        ()
        rawTwee
        System.Text.Encoding.UTF8
    |> FParsec.ParserResult.toResult
    |> Result.map (fun (result, _, _) -> result)

let rawParseFile =
    parseFile PassageBody.parser
