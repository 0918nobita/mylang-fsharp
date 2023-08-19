[<AutoOpen>]
module Parsec.ParserContext

open System.Collections.Generic

open SourceFile
open Parser

exception ParserContextMismatch

type ParserContext(sourceFile: SourceFile) =
    static let mutable nextContextId = 0

    let contextId = nextContextId

    do nextContextId <- nextContextId + 1

    member _.Memorize(Parser parse: Parser<Unmemorized, 'State, 'T, 'E>) : Parser<Memorized, 'State, 'T, 'E> =
        let memo = Dictionary<int * 'State, ParseResult<'State, 'T, 'E>>()

        Parser
        <| fun contextId' ->
            if contextId <> contextId' then
                raise ParserContextMismatch

            let parseFn = parse contextId

            fun (state, sourceFile, cursorIndex) ->
                if memo.ContainsKey((cursorIndex, state)) then
                    memo.[cursorIndex, state]
                else
                    let result = parseFn (state, sourceFile, cursorIndex)
                    memo.[(cursorIndex, state)] <- result
                    result

    /// <exception cref="ParserContextMismatch"></exception>
    member _.Run
        (
            Parser parse: Parser<_, 'State, 'T, 'E>,
            initialState: 'State,
            fromIndex: CursorIndex
        ) : ParseResult<'State, 'T, 'E> =
        parse contextId (initialState, sourceFile, fromIndex)
