namespace Twine.Twee.FSharp

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
