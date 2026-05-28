namespace Twine.Twee.FSharp

open Twine.Twee.FSharp.Parser
open Twine.Twee.FSharp.Printer

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PassageMetadata =
    module Parser =
        open FParsec

        open Parser.Common

        let parser: PassageMetadata Parser =
            between
                (pchar '{' >>. spaces)
                (pchar '}')
                (manySatisfy ((<>) '}'))

    module Printer =
        open FsharpMyExtension.Serialization.Serializers.ShowList

        let shows (metadata: PassageMetadata) : ShowS =
            between (showChar '{') (showChar '}') (
                showString metadata
            )

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PassageHeader =
    module Parser =
        open FParsec

        open Twine.Twee.FSharp.Parser
        open Twine.Twee.FSharp.Parser.Common

        let parser: PassageHeader Parser =
            skipString "::" >>. spaces
            >>. pipe3
                PassageName.parser
                (opt (PassageTags.parser .>> whitespaces))
                (opt PassageMetadata.Parser.parser)
                (fun name tags metadata ->
                    {
                        Name = name
                        Tags = tags
                        Metadata = metadata
                    }
                )

    module Printer =
        open FsharpMyExtension.Serialization.Serializers.ShowList

        let shows (header: PassageHeader) : ShowS =
            showString "::" << showSpace
            << showString header.Name
            << (header.Tags
                |> Option.map (fun tags ->
                    showSpace << PassageTags.shows tags
                )
                |> Option.defaultValue empty)
            << (header.Metadata
                |> Option.map (fun metadata ->
                    showSpace << PassageMetadata.Printer.shows metadata
                )
                |> Option.defaultValue empty)

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Passage =
    module Parser =
        open FParsec

        open Parser.Common

        let parser (pbody: Parser<'Body>) : Passage<'Body> Parser =
            pipe2
                (PassageHeader.Parser.parser .>> optional skipNewline)
                pbody
                (fun header body ->
                    {
                        Header = header
                        Body = body
                    }
                )

    module Printer =
        open FsharpMyExtension.Serialization.Serializers.ShowList

        let shows showBody newlineType (passage: Passage<'PassageBody>) =
            PassageHeader.Printer.shows passage.Header
            << (showString <| NewlineType.toString newlineType)
            << showBody newlineType passage.Body

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Document =
    open FsharpMyExtension.Serialization.Deserializers
    open FsharpMyExtension.Serialization.Serializers

    module Parser =
        open FParsec

        open Parser.Common

        let parser (ppassageBody: Parser<'PassageBody>) : Document<'PassageBody> Parser =
            many (Passage.Parser.parser ppassageBody .>> spaces)

    let parse ppassageBody (rawTwee: string) =
        FParsec.runResult (Parser.parser ppassageBody) rawTwee

    let rawParse =
        parse PassageBody.parser

    let parseFile ppassageBody (rawTwee: string) =
        FParsec.CharParsers.runParserOnFile
            (Parser.parser ppassageBody)
            ()
            rawTwee
            System.Text.Encoding.UTF8
        |> FParsec.ParserResult.toResult
        |> Result.map (fun (result, _, _) -> result)

    let rawParseFile =
        parseFile PassageBody.parser

    module Printer =
        open FsharpMyExtension.Serialization.Serializers.ShowList

        let shows showPassageBody newlineType (document: Document<'PassageBody>) =
            let newline =
                showString <| NewlineType.toString newlineType
            let newlines =
                newline
                << newline << newline // add two empty blanks
            document
            |> List.map (Passage.Printer.shows showPassageBody newlineType)
            |> joinsEmpty newlines

    let toString showPassageBody newlineType (document: Document<'PassageBody>) =
        Printer.shows showPassageBody newlineType document
        |> ShowList.show

    let updatePassages update (twee: Document<'PassageBody>) =
        twee
        |> List.mapFold
            (fun changedPassagesCount passage ->
                match update passage with
                | None ->
                    passage, changedPassagesCount
                | Some updatedPassage ->
                    updatedPassage, changedPassagesCount + 1
            )
            0
