module Twine.Twee.FSharp.Parser.Common

open FParsec

type 'a Parser = Parser<'a, unit>

let whitespaces : _ Parser =
    skipManySatisfy (fun c -> c = ' ' || c = '\t' )
