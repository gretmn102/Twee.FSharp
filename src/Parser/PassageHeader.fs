[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Twine.Twee.FSharp.Parser.PassageHeader
open FParsec

open Twine.Twee.FSharp
open Common

let parser: PassageHeader Parser =
    skipString "::" >>. spaces
    >>. pipe3
        PassageName.parser
        (opt (PassageTags.parser .>> whitespaces))
        (opt PassageMetadata.parser)
        (fun name tags metadata ->
            {
                Name = name
                Tags = tags
                Metadata = metadata
            }
        )
