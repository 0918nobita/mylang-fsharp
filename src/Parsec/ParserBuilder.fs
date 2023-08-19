[<AutoOpen>]
module Parsec.ParserBuilder

type ParserBuilder() =
    member inline _.Return(parsedValue) = Parser.succeed parsedValue

    member inline _.ReturnFrom(parser) = parser

    member inline _.BindReturn(parser, mapping) = Parser.map mapping parser

    member inline _.Bind(parser, binder) = Parser.bind binder parser

let parser = ParserBuilder()
