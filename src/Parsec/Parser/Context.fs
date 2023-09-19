module Parsec.Parser.Context

open Parsec.SourceFileReader
open Type

type ParserContext(source: string) =
    let sourceFileReader = SourceFileReader(source)

    member _.Run(parse: Parser<'State, 'T>, initialState: 'State) : Result<'T, Failure> =
        parse (initialState, sourceFileReader)
        |> Result.map (fun success -> success.ParsedValue)

    member _.ResetReader() = sourceFileReader.Reset()
