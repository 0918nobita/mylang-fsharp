/// パーサに入力を与えて実行するためのコンテキスト
[<AutoOpen>]
module Parsec.ParserContext

open System.Collections.Generic

open Parser

type private StreamFromString(source: string) =
    let len = String.length source

    let posMemo = Dictionary<int, SourcePos>()

    do posMemo.Add(-1, SourcePos.origin)

    let mutable currentPos = SourcePos.origin

    let mutable currentIndex = -1

    interface System.IDisposable with
        member _.Dispose() = ()

    interface ISourceCharStream with
        member _.CurrentIndex
            with get () = currentIndex
            and set (index: int) = currentIndex <- index

        member _.CurrentPos = posMemo.[currentIndex]

        member _.ReadNextChar() =
            currentIndex <- currentIndex + 1

            if currentIndex < len then
                if posMemo.ContainsKey currentIndex then
                    let pos = posMemo.[currentIndex]
                    let c = source.[currentIndex]
                    (pos, Some c)
                else
                    let c = source.[currentIndex]

                    currentPos <-
                        if c = '\n' then
                            SourcePos.nextLine currentPos
                        else
                            SourcePos.nextColumn currentPos

                    posMemo.[currentIndex] <- currentPos

                    (currentPos, Some c)
            else
                (currentPos, None)

        member _.SeekFromBegin(absoluteIndex: int) = currentIndex <- absoluteIndex - 1

        member _.SeekFromCurrentPos(offset: int) = currentIndex <- currentIndex + offset

type ParserContext(source: string) =
    static let mutable nextContextId = 0

    let contextId = nextContextId

    do nextContextId <- nextContextId + 1

    let sourceCharStream = new StreamFromString(source)

    member _.Run(Parser parse: Parser<_, 'State, 'T>, initialState: 'State) : ParseResult<'State, 'T> =
        let parseFn = Reader.run contextId parse
        parseFn (initialState, sourceCharStream)

    interface System.IDisposable with
        member _.Dispose() =
            (sourceCharStream :> ISourceCharStream).Dispose()
