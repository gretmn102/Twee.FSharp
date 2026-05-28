namespace Twine.Twee.FSharp

type PassageName = string

type PassageTag = string

type PassageTags = PassageTag Set

type PassageMetadata = string

type PassageHeader =
    {
        Name: PassageName
        Tags: PassageTags option
        Metadata: PassageMetadata option
    }

type PassageBody = string list

type Passage<'Body> = {
    Header: PassageHeader
    Body: 'Body
}

type Document<'PassageBody> = Passage<'PassageBody> list

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Document =
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
