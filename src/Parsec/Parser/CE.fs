module Parsec.Parser.CE

type ParserBuilder() =
    member inline _.Return(parsedValue) = Prim.succeed parsedValue

    member inline _.ReturnFrom(parser) = parser

    member inline _.BindReturn(parser, [<InlineIfLambda>] mapping) = Prim.map mapping parser

    member inline _.Bind(parser, [<InlineIfLambda>] binder) = Prim.bind binder parser

let parser = ParserBuilder()
