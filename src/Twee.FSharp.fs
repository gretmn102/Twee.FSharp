namespace Twee.FSharp

module CommonParser =
    open FParsec

    type 'a Parser = Parser<'a, unit>

[<RequireQualifiedAccess>]
type NewlineType =
    | Lf
    | CrLf

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module NewlineType =
    let toString newlineType =
        match newlineType with
        | NewlineType.Lf -> "\n"
        | NewlineType.CrLf -> "\r\n"

type PassageName = string

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PassageName =
    module Parser =
        open FParsec

        open CommonParser

        let parser: string Parser =
            manySatisfy ((<>) '\n')
            |>> fun x -> x.TrimEnd() // optimize: remove trailing whitespaces by parser

    module Printer =
        open FsharpMyExtension.Serialization.Serializers.ShowList

        let shows (passageName: PassageName) =
            showString passageName

type PassageBody = string list

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PassageBody =
    module Parser =
        open FParsec

        open CommonParser

        let parser: PassageBody Parser =
            let pline: _ Parser =
                notFollowedByString "::"
                >>? many1Satisfy ((<>) '\n')
            many (choice [
                pline .>> skipNewline
                newlineReturn ""
            ])

    module Printer =
        open FsharpMyExtension.Serialization.Serializers.ShowList

        let shows newlineType (passageBody: PassageBody) =
            let newline =
                showString <| NewlineType.toString newlineType
            passageBody
            |> List.map showString
            |> joinsEmpty newline

type PassageTag = string

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PassageTag =
    module Parser =
        open FParsec

        open CommonParser

        let parser: PassageTag Parser =
            many1Satisfy (isNoneOf " ]") // todo: add escape \]
            |>> fun x -> x.TrimEnd() // optimize: remove trailing whitespaces by parser

    module Printer =
        open FsharpMyExtension.Serialization.Serializers.ShowList

        let shows (tag: PassageTag) : ShowS =
            tag.Trim()
            |> showString // todo: add escape \]

type PassageTags = PassageTag Set

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PassageTags =
    module Parser =
        open FParsec

        open CommonParser

        let parser: PassageTags Parser =
            between
                (pchar '[' >>. spaces)
                (pchar ']')
                (many (PassageTag.Parser.parser .>> spaces))
            |>> Set.ofList

    module Printer =
        open FsharpMyExtension.Serialization.Serializers.ShowList

        let shows (tags: PassageTags) : ShowS =
            between (showChar '[') (showChar ']') (
                tags
                |> Seq.map PassageTag.Printer.shows
                |> List.ofSeq
                |> joinsEmpty showSpace
            )

type PassageMetadata = string

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PassageMetadata =
    module Parser =
        open FParsec

        open CommonParser

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

type PassageHeader =
    {
        Name: PassageName
        Tags: PassageTags option
        Metadata: PassageMetadata option
    }

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PassageHeader =
    module Parser =
        open FParsec

        open CommonParser

        let parser: PassageHeader Parser =
            skipString "::" >>. spaces
            >>. pipe3
                PassageName.Parser.parser
                (opt PassageTags.Parser.parser)
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

        let shows (tags: PassageHeader) : ShowS =
            showString "::" << showSpace
            << (tags.Tags
                |> Option.map (fun tags ->
                    PassageTags.Printer.shows tags << showSpace
                )
                |> Option.defaultValue empty)
            << (tags.Metadata
                |> Option.map PassageMetadata.Printer.shows
                |> Option.defaultValue empty)

type Passage = {
    Header: PassageHeader
    Body: PassageBody
}

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Passage =
    module Parser =
        open FParsec

        open CommonParser

        let parser: Passage Parser =
            pipe2
                PassageHeader.Parser.parser
                PassageBody.Parser.parser
                (fun header body ->
                    {
                        Header = header
                        Body = body
                    }
                )

    module Printer =
        let shows newlineType (passage: Passage) =
            PassageHeader.Printer.shows passage.Header
            << PassageBody.Printer.shows newlineType passage.Body

type Document = Passage list

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Document =
    open FsharpMyExtension.Serialization.Deserializers
    open FsharpMyExtension.Serialization.Serializers

    module Parser =
        open FParsec

        open CommonParser

        let parser: Document Parser =
            many (Passage.Parser.parser .>> spaces)

    let parse (rawTwee: string) =
        FParsec.runResult Parser.parser rawTwee

    let parseFile (rawTwee: string) =
        FParsec.CharParsers.runParserOnFile
            Parser.parser
            ()
            rawTwee
            System.Text.Encoding.UTF8
        |> FParsec.ParserResult.toResult
        |> Result.map (fun (result, _, _) -> result)

    module Printer =
        open FsharpMyExtension.Serialization.Serializers.ShowList

        let shows newlineType (document: Document) =
            let newline =
                showString <| NewlineType.toString newlineType
            document
            |> List.map (Passage.Printer.shows newlineType)
            |> joinsEmpty newline

    let toString newlineType (document: Document) =
        Printer.shows newlineType document
        |> ShowList.show

    let updatePassage passageName update (twee: Document) =
        twee
        |> List.map (fun passage -> // todo: убедиться, что такой пассаж вообще существует
            if passage.Header <> passageName then passage
            else update passage
        )
