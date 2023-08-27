[<AutoOpen>]
module Parsec.ParserContext

open SourceFile
open Parser

type ParserContext(sourceFile: SourceFile) =
    static let mutable nextContextId = 0

    let contextId = nextContextId

    do nextContextId <- nextContextId + 1

    member _.Run
        (
            Parser parse: Parser<_, 'State, 'T, 'E>,
            initialState: 'State,
            fromIndex: CursorIndex
        ) : ParseResult<'State, 'T, 'E> =
        parse contextId (initialState, sourceFile, fromIndex)
