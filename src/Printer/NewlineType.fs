namespace Twine.Twee.FSharp.Printer

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
