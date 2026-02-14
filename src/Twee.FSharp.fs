namespace Twee.FSharp

module CommonParser =
    open FParsec

    type 'a Parser = Parser<'a, unit>

    let whitespaces =
        skipManySatisfy (fun c -> c = ' ' || c = '\t' )

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
            manySatisfy (isNoneOf "[{\n")
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
            let pemptyBlanks1: _ Parser =
                many1 (newlineReturn "")
                .>>? notFollowedBy (skipString "::" <|> eof)
            many (choice [
                pline .>> skipNewline |>> List.singleton // todo: or eof
                pemptyBlanks1
            ])
            |>> List.concat

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
                (opt (PassageTags.Parser.parser .>> whitespaces))
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
                    showSpace << PassageTags.Printer.shows tags
                )
                |> Option.defaultValue empty)
            << (header.Metadata
                |> Option.map (fun metadata ->
                    showSpace << PassageMetadata.Printer.shows metadata
                )
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
                (PassageHeader.Parser.parser .>> optional skipNewline)
                PassageBody.Parser.parser
                (fun header body ->
                    {
                        Header = header
                        Body = body
                    }
                )

    module Printer =
        open FsharpMyExtension.Serialization.Serializers.ShowList

        let shows newlineType (passage: Passage) =
            PassageHeader.Printer.shows passage.Header
            << (showString <| NewlineType.toString newlineType)
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
            let newlines =
                newline
                << newline << newline // add two empty blanks
            document
            |> List.map (Passage.Printer.shows newlineType)
            |> joinsEmpty newlines

    let toString newlineType (document: Document) =
        Printer.shows newlineType document
        |> ShowList.show

    let updatePassage predicate update (twee: Document) =
        twee
        |> List.map (fun passage -> // todo: убедиться, что такой пассаж вообще существует
            if not (predicate passage) then passage
            else update passage
        )

    let updatePassageByName passageName update (twee: Document) =
        twee
        |> List.map (fun passage -> // todo: убедиться, что такой пассаж вообще существует
            if passage.Header.Name <> passageName then passage
            else update passage
        )
