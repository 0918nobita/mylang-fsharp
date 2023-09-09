/// コンピュテーション式でパーサを構成するためのビルダークラスとそのインスタンス
[<AutoOpen>]
module Parsec.ParserBuilder

type ParserBuilder() =
    member inline _.Return(parsedValue) = Parser.succeed parsedValue

    member inline _.ReturnFrom(parser) = parser

    member inline _.BindReturn(parser, [<InlineIfLambda>] mapping) = Parser.map mapping parser

    member inline _.Bind(parser, [<InlineIfLambda>] binder) = Parser.bind binder parser

let parser = ParserBuilder()
