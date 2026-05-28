module Twine.Twee.FSharp.Tests
open FsharpMyExtension.Serialization.Deserializers
open FsharpMyExtension.Serialization.Serializers
open Expecto

open Twine.Twee.FSharp.Parser
open Twine.Twee.FSharp.Printer

[<Tests>]
let ``Parser.PassageName.parser`` =
    let parser = PassageName.parser
    testList "Parser.PassageName.parser" [
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
let ``Printer.PassageName.shows`` =
    let shows = Printer.PassageName.shows
    testList "Printer.PassageName.shows" [
        testCase "1" <| fun () ->
            Expect.equal
                (shows "Passage" |> ShowList.show)
                "Passage"
                ""
    ]

[<Tests>]
let ``PassageTags.parser`` =
    let parser = PassageTags.parser
    testList "PassageTags.parser" [
        testCase "1" <| fun () ->
            Expect.equal
                ("[tag1   tag2]" |> FParsec.runResult parser)
                (Ok (Set.ofList ["tag1"; "tag2"]))
                ""
    ]

[<Tests>]
let ``PassageTags.shows`` =
    let shows = PassageTags.shows
    testList "PassageTags.shows" [
        testCase "1" <| fun () ->
            Expect.equal
                (["tag1"; "tag2"] |> Set.ofList |> shows |> ShowList.show)
                "[tag1 tag2]"
                ""
    ]

[<Tests>]
let ``PassageMetadata.parser`` =
    let parser = PassageMetadata.parser
    testList "PassageMetadata.parser" [
        testCase "1" <| fun () ->
            Expect.equal
                ("{\"position\":\"800,5700\",\"size\":\"100,100\"}" |> FParsec.runResult parser)
                ("\"position\":\"800,5700\",\"size\":\"100,100\"" |> Ok)
                ""
    ]

[<Tests>]
let ``PassageMetadata.shows`` =
    let shows = PassageMetadata.shows
    testList "PassageMetadata.shows" [
        testCase "1" <| fun () ->
            Expect.equal
                ("\"position\":\"800,5700\",\"size\":\"100,100\"" |> shows |> ShowList.show)
                "{\"position\":\"800,5700\",\"size\":\"100,100\"}"
                ""
    ]

[<Tests>]
let ``PassageHeader.parser`` =
    let parser = PassageHeader.parser
    testList "PassageHeader.parser" [
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
let ``PassageHeader.shows`` =
    let shows = PassageHeader.shows
    testList "PassageHeader.shows" [
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
let ``PassageBody.parser`` =
    let parser = PassageBody.parser
    testList "PassageBody.parser" [
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
let ``PassageBody.shows`` =
    let shows = PassageBody.shows
    testList "PassageBody.shows" [
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
    let parse =
        FParsec.runResult (
            Passage.Parser.parser PassageBody.parser
        )
    testList "Passage.Parser.parser" [
        testCase "1" <| fun () ->
            Expect.equal
                (parse (
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
    let show newlineType =
        Passage.Printer.shows PassageBody.shows newlineType
        >> ShowList.show
    testList "Passage.Printer.shows" [
        testCase "1" <| fun () ->
            Expect.equal
                (show NewlineType.Lf {
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
let ``Document.Parser.parser`` =
    let parse =
        FParsec.runResult (
            Document.Parser.parser PassageBody.parser
        )
    testList "Document.Parser.parser" [
        testCase "1" <| fun () ->
            Expect.equal
                (parse (
                    String.concat (NewlineType.toString NewlineType.Lf) [
                        ":: Passage1"
                        "Body1"
                        ""
                        ""
                        ":: Passage2"
                        "Body2"
                        ""
                        ""
                    ]
                ))
                (Ok [
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
                ""
    ]

[<Tests>]
let ``Document.Printer.shows`` =
    let show newlineType =
        Document.Printer.shows PassageBody.shows newlineType
        >> ShowList.show
    testList "Document.Printer.shows" [
        testCase "1" <| fun () ->
            Expect.equal
                (show NewlineType.Lf [
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
