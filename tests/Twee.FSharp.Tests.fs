module Twee.FSharp.Tests
open FsharpMyExtension.Serialization.Deserializers
open FsharpMyExtension.Serialization.Serializers
open Expecto

[<Tests>]
let ``PassageName.Parser.parser`` =
    let parser = PassageName.Parser.parser
    testList "PassageName.Parser.parser" [
        testCase "1" <| fun () ->
            Expect.equal
                (FParsec.runResult parser (
                    String.concat "\n" [
                        "Passage"
                    ]
                ))
                (Ok "Passage")
                ""
        testCase "with trailing spaces" <| fun () ->
            Expect.equal
                (FParsec.runResult parser (
                    String.concat "\n" [
                        "Passage   "
                    ]
                ))
                (Ok "Passage")
                ""
        testCase "with [" <| fun () ->
            Expect.equal
                (FParsec.runResult parser (
                    String.concat "\n" [
                        "Passage [tag1]"
                    ]
                ))
                (Ok "Passage")
                ""
        testCase "with {" <| fun () ->
            Expect.equal
                (FParsec.runResult parser (
                    String.concat "\n" [
                        """Passage {"key":"value"}"""
                    ]
                ))
                (Ok "Passage")
                ""
    ]

[<Tests>]
let ``PassageName.Printer.shows`` =
    let shows = PassageName.Printer.shows
    testList "PassageName.Printer.shows" [
        testCase "1" <| fun () ->
            Expect.equal
                (shows "Passage" |> ShowList.show)
                "Passage"
                ""
    ]

[<Tests>]
let ``PassageTags.Parser.parser`` =
    let parser = PassageTags.Parser.parser
    testList "PassageTags.Parser.parser" [
        testCase "1" <| fun () ->
            Expect.equal
                ("[tag1   tag2]" |> FParsec.runResult parser)
                (Ok (Set.ofList ["tag1"; "tag2"]))
                ""
    ]

[<Tests>]
let ``PassageTags.Printer.shows`` =
    let shows = PassageTags.Printer.shows
    testList "PassageTags.Printer.shows" [
        testCase "1" <| fun () ->
            Expect.equal
                (["tag1"; "tag2"] |> Set.ofList |> shows |> ShowList.show)
                "[tag1 tag2]"
                ""
    ]

[<Tests>]
let ``PassageMetadata.Parser.parser`` =
    let parser = PassageMetadata.Parser.parser
    testList "PassageMetadata.Parser.parser" [
        testCase "1" <| fun () ->
            Expect.equal
                ("{\"position\":\"800,5700\",\"size\":\"100,100\"}" |> FParsec.runResult parser)
                ("\"position\":\"800,5700\",\"size\":\"100,100\"" |> Ok)
                ""
    ]

[<Tests>]
let ``PassageMetadata.Printer.shows`` =
    let shows = PassageMetadata.Printer.shows
    testList "PassageMetadata.Printer.shows" [
        testCase "1" <| fun () ->
            Expect.equal
                ("\"position\":\"800,5700\",\"size\":\"100,100\"" |> shows |> ShowList.show)
                "{\"position\":\"800,5700\",\"size\":\"100,100\"}"
                ""
    ]

[<Tests>]
let ``PassageHeader.Parser.parser`` =
    let parser = PassageHeader.Parser.parser
    testList "PassageHeader.Parser.parser" [
        testCase "name, tag" <| fun () ->
            Expect.equal
                (String.concat " " [
                    "::"
                    "StoryStylesheet"
                    "[stylesheet]"
                ] |> FParsec.runResult parser)
                (Ok {
                    Name = "StoryStylesheet"
                    Tags = Some (Set.ofList ["stylesheet"])
                    Metadata = None
                })
                ""
        testCase "1" <| fun () ->
            Expect.equal
                (String.concat " " [
                    "::"
                    "PassageName"
                    "[tag1 tag2]"
                    "{\"position\":\"800,5700\",\"size\":\"100,100\"}"
                ] |> FParsec.runResult parser)
                (Ok {
                    Name = "PassageName"
                    Tags = Some (Set.ofList ["tag1"; "tag2"])
                    Metadata = Some "\"position\":\"800,5700\",\"size\":\"100,100\""
                })
                ""
    ]

[<Tests>]
let ``PassageHeader.Printer.shows`` =
    let shows = PassageHeader.Printer.shows
    testList "PassageHeader.Printer.shows" [
        testCase "only tags" <| fun () ->
            Expect.equal
                ({
                    Name = "PassageName"
                    Tags = Some (Set.ofList ["tag1"; "tag2"])
                    Metadata = None
                } |> shows |> ShowList.show)
                (String.concat " " [
                    "::"
                    "PassageName"
                    "[tag1 tag2]"
                ])
                ""
        testCase "tags, metadata" <| fun () ->
            Expect.equal
                ({
                    Name = "PassageName"
                    Tags = Some (Set.ofList ["tag1"; "tag2"])
                    Metadata = Some "\"position\":\"800,5700\",\"size\":\"100,100\""
                } |> shows |> ShowList.show)
                (String.concat " " [
                    "::"
                    "PassageName"
                    "[tag1 tag2]"
                    "{\"position\":\"800,5700\",\"size\":\"100,100\"}"
                ])
                ""
    ]

[<Tests>]
let ``PassageBody.Parser.parser`` =
    let parser = PassageBody.Parser.parser
    testList "PassageBody.Parser.parser" [
        testCase "empty blanks only" <| fun () ->
            Expect.equal
                (FParsec.runResult parser (
                    String.concat "\n" [
                        ""
                        ""
                    ]
                ))
                (Ok [])
                ""
        testCase "with next passage" <| fun () ->
            Expect.equal
                (FParsec.runResult parser (
                    String.concat "\n" [
                        "lorem"
                        ""
                        "ipsum dei"
                        ""
                        ":: Next passage"
                    ]
                ))
                (Ok [
                    "lorem"
                    ""
                    "ipsum dei"
                ])
                ""
    ]

[<Tests>]
let ``PassageBody.Printer.shows`` =
    let shows = PassageBody.Printer.shows
    testList "PassageBody.Printer.shows" [
        testCase "1" <| fun () ->
            Expect.equal
                (ShowList.show <| shows NewlineType.Lf [
                    "lorem"
                    ""
                    "ipsum dei"
                    ""
                ])
                (String.concat "\n" [
                    "lorem"
                    ""
                    "ipsum dei"
                    ""
                ])
                ""
    ]

[<Tests>]
let ``Passage.Parser.parser`` =
    let parser = Passage.Parser.parser
    testList "Passage.Parser.parser" [
        testCase "1" <| fun () ->
            Expect.equal
                (FParsec.runResult parser (
                    String.concat "\n" [
                        ":: Passage"
                        "Body\n"
                    ]
                ))
                (Ok {
                    Header = {
                        Name = "Passage"
                        Tags = None
                        Metadata = None
                    }
                    Body = ["Body"]
                })
                ""
    ]

[<Tests>]
let ``Passage.Printer.shows`` =
    let shows = Passage.Printer.shows
    testList "Passage.Printer.shows" [
        testCase "1" <| fun () ->
            Expect.equal
                (ShowList.show <| shows NewlineType.Lf {
                    Header = {
                        Name = "Passage"
                        Tags = None
                        Metadata = None
                    }
                    Body = ["Body"]
                })
                (String.concat (NewlineType.toString NewlineType.Lf) [
                    ":: Passage"
                    "Body"
                ])
                ""
    ]

[<Tests>]
let ``Document.Printer.shows`` =
    let shows = Document.Printer.shows
    testList "Document.Printer.shows" [
        testCase "1" <| fun () ->
            Expect.equal
                (ShowList.show <| shows NewlineType.Lf [
                    {
                        Header = {
                            Name = "Passage1"
                            Tags = None
                            Metadata = None
                        }
                        Body = ["Body1"]
                    }
                    {
                        Header = {
                            Name = "Passage2"
                            Tags = None
                            Metadata = None
                        }
                        Body = ["Body2"]
                    }
                ])
                (String.concat (NewlineType.toString NewlineType.Lf) [
                    ":: Passage1"
                    "Body1"
                    ""
                    ""
                    ":: Passage2"
                    "Body2"
                ])
                ""
    ]
