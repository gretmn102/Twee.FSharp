namespace Twine.Twee.FSharp

type PassageName = string

type PassageBody = string list

type PassageTag = string

type PassageTags = PassageTag Set

type PassageMetadata = string

type PassageHeader =
    {
        Name: PassageName
        Tags: PassageTags option
        Metadata: PassageMetadata option
    }

type Passage<'Body> = {
    Header: PassageHeader
    Body: 'Body
}

type Document<'PassageBody> = Passage<'PassageBody> list
