[<AutoOpen>]
module Parsec.Parser

type CursorIndex = int

type ParseSuccess<'ParsedValue> =
    {
        /// 続けてパースする場合の開始位置
        NextIndex: CursorIndex
        ParsedValue: 'ParsedValue
    }

/// <summary>パース結果</summary>
/// <typeparam name="State">遷移後の状態</typeparam>
/// <typeparam name="ParsedValue">パースに成功した場合に得られる、パースされた値</typeparam>
/// <typeparam name="ErrorReason">パースに失敗した場合に得られる、エラー原因</typeparam>
type ParseResult<'State, 'ParsedValue, 'ErrorReason> = Result<'State * ParseSuccess<'ParsedValue>, 'ErrorReason>

/// パーサのコンテキストの識別子
type internal ContextId = int

type ParseFn<'State, 'ParsedValue, 'ErrorReason> =
    'State * SourceFile * CursorIndex -> ParseResult<'State, 'ParsedValue, 'ErrorReason>

type MemorizedOrNot =
    interface
    end

type Unmemorized =
    interface
        inherit MemorizedOrNot
    end

type Memorized =
    interface
        inherit MemorizedOrNot
    end

type Parser<'MemorizedOrNot, 'State, 'ParsedValue, 'ErrorReason when 'MemorizedOrNot :> MemorizedOrNot> =
    internal | Parser of
        (ContextId -> 'State * SourceFile * CursorIndex -> ParseResult<'State, 'ParsedValue, 'ErrorReason>)

module Parser =
    /// メモ化されていない単純なパーサを生成する
    let make (parseFn: ParseFn<'State, 'T, 'E>) : Parser<Unmemorized, 'State, 'T, 'E> =
        Parser <| fun _contextId -> parseFn

    /// 必ず成功して <c>parsedValue</c> を返すパーサを生成する
    let inline succeed (parsedValue: 'T) : Parser<Unmemorized, _, 'T, _> =
        make (fun (state, _sourceFile, cursorIndex) ->
            Ok(
                state,
                { NextIndex = cursorIndex
                  ParsedValue = parsedValue }
            ))

    /// 必ず失敗するパーサを生成する
    let inline fail (reason: 'E) : Parser<Unmemorized, _, _, 'E> =
        make (fun (_state, _sourceFile, _cursorIndex) -> Error reason)

    /// パーサの現在の状態を取得する
    let getState () : Parser<Unmemorized, 'State, 'State, _> =
        make (fun (state, _sourceFile, cursorIndex) ->
            Ok(
                state,
                { NextIndex = cursorIndex
                  ParsedValue = state }
            ))

    /// パーサの状態を指定した値に更新する
    let putState (state: 'State) : Parser<Unmemorized, 'State, unit, _> =
        make (fun (_state, _sourceFile, cursorIndex) ->
            Ok(
                state,
                { NextIndex = cursorIndex
                  ParsedValue = () }
            ))

    /// パーサの状態を指定した関数で更新する
    let modifyState (mapping: 'State -> 'State) : Parser<Unmemorized, 'State, unit, _> =
        make (fun (state, _sourceFile, cursorIndex) ->
            Ok(
                mapping state,
                { NextIndex = cursorIndex
                  ParsedValue = () }
            ))

    /// パースされた値を指定した関数で変換する
    let map (mapping: 'T -> 'U) (Parser parse: Parser<_, 'State, 'T, 'E>) : Parser<Unmemorized, 'State, 'U, 'E> =
        Parser
        <| fun contextId ->
            let parseFn = parse contextId

            fun (prevState, sourceFile, cursorIndex) ->
                parseFn (prevState, sourceFile, cursorIndex)
                |> Result.map (fun (newState, success) ->
                    (newState,
                     { NextIndex = success.NextIndex
                       ParsedValue = mapping success.ParsedValue }))

    /// パース失敗時の原因を指定した関数で変換する
    let mapError
        (mapping: 'E1 -> 'E2)
        (Parser parse: Parser<_, 'State, 'T, 'E1>)
        : Parser<Unmemorized, 'State, 'T, 'E2> =
        Parser
        <| fun contextId ->
            let parseFn = parse contextId

            fun (prevState, sourceFile, cursorIndex) ->
                parseFn (prevState, sourceFile, cursorIndex) |> Result.mapError mapping

    let bind
        (binder: 'T -> Parser<_, 'State, 'U, 'E>)
        (Parser parse: Parser<_, 'State, 'T, _>)
        : Parser<Unmemorized, 'State, 'U, 'E> =
        Parser
        <| fun contextId ->
            let parseFn = parse contextId

            fun (prevState, sourceFile, cursorIndex) ->
                parseFn (prevState, sourceFile, cursorIndex)
                |> Result.bind (fun (newState, success) ->
                    let (Parser parse) = binder success.ParsedValue
                    parse contextId (newState, sourceFile, success.NextIndex))

    /// 1つ目のパーサで失敗した場合、2つ目のパーサを試す
    let alt
        (Parser parse2: Parser<_, 'State, 'T, 'E>)
        (Parser parse1: Parser<_, 'State, 'T, _>)
        : Parser<Unmemorized, 'State, 'T, 'E> =
        Parser
        <| fun contextId ->
            let parseFn1 = parse1 contextId
            let parseFn2 = parse2 contextId

            fun (prevState, sourceFile, cursorIndex) ->
                match parseFn1 (prevState, sourceFile, cursorIndex) with
                | Ok(newState, success) -> Ok(newState, success)
                | Error _ -> parseFn2 (prevState, sourceFile, cursorIndex)

    /// <summary>エラー復帰を表現するコンビネータ</summary>
    /// <remarks>1つ目のパーサで失敗した場合、そのエラー原因をもとに生成した2つ目のパーサを実行する</remarks>
    let recover
        (recovery: 'E1 -> Parser<_, 'State, 'T, 'E2>)
        (Parser parse: Parser<_, 'State, _, 'E1>)
        : Parser<Unmemorized, 'State, 'T, 'E2> =
        Parser
        <| fun contextId ->
            let parseFn = parse contextId

            fun (state, sourceFile, cursorIndex) ->
                match parseFn (state, sourceFile, cursorIndex) with
                | Ok(state', success) -> Ok(state', success)
                | Error reason ->
                    let (Parser parse') = recovery reason
                    parse' contextId (state, sourceFile, cursorIndex)

    let withoutMovingCursor (Parser parser: Parser<_, 'State, 'T, 'E>) : Parser<Unmemorized, 'State, 'T, 'E> =
        Parser
        <| fun contextId ->
            let parseFn = parser contextId

            fun (prevState, sourceFile, cursorIndex) ->
                parseFn (prevState, sourceFile, cursorIndex)
                |> Result.map (fun (newState, { NextIndex = _; ParsedValue = parsedValue }) -> (newState, { NextIndex = cursorIndex; ParsedValue = parsedValue }))

    let opt (Parser parse: Parser<_, 'State, 'T, 'E>) : Parser<Unmemorized, 'State, Option<'T>, _> =
        Parser
        <| fun contextId ->
            let parseFn = parse contextId

            fun (prevState, sourceFile, cursorIndex) ->
                match parseFn (prevState, sourceFile, cursorIndex) with
                | Ok (newState, { NextIndex = nextIndex; ParsedValue = parsedValue }) ->
                    Ok (newState, { NextIndex = nextIndex; ParsedValue = Some parsedValue })
                | Error _reason ->
                    Ok (prevState, { NextIndex = cursorIndex; ParsedValue = None })

    /// 0回以上の繰り返し
    let many (Parser parse: Parser<_, 'State, 'T, _>) : Parser<Unmemorized, 'State, 'T list, _> =
        Parser
        <| fun contextId ->
            let parseFn = parse contextId

            fun (prevState, sourceFile, cursorIndex) ->
                let mutable results: 'T list = []

                let mutable currentState = prevState

                let mutable currentIndex = cursorIndex

                let mutable shouldContinue = true

                while shouldContinue do
                    match parseFn (currentState, sourceFile, currentIndex) with
                    | Ok(newState,
                         { NextIndex = nextIndex
                           ParsedValue = parsedValue }) ->
                        results <- parsedValue :: results
                        currentState <- newState
                        currentIndex <- nextIndex
                    | Error _ -> shouldContinue <- false

                Ok(
                    currentState,
                    { NextIndex = currentIndex
                      ParsedValue = List.rev results }
                )

    /// 1回以上の繰り返し
    let some (Parser parse: Parser<_, 'State, 'T, 'E>) : Parser<Unmemorized, 'State, 'T * 'T list, 'E> =
        Parser
        <| fun contextId ->
            let parseFn = parse contextId

            fun (prevState, sourceFile, cursorIndex) ->
                match parseFn (prevState, sourceFile, cursorIndex) with
                | Ok(newState,
                     { NextIndex = nextIndex
                       ParsedValue = head }) ->
                    let mutable tail: 'T list = []

                    let mutable currentState = newState

                    let mutable currentIndex = nextIndex

                    let mutable shouldContinue = true

                    while shouldContinue do
                        match parseFn (currentState, sourceFile, currentIndex) with
                        | Ok(newState',
                             { NextIndex = nextIndex'
                               ParsedValue = parsedValue }) ->
                            tail <- parsedValue :: tail
                            currentState <- newState'
                            currentIndex <- nextIndex'
                        | Error _ -> shouldContinue <- false

                    Ok(
                        currentState,
                        { NextIndex = currentIndex
                          ParsedValue = (head, List.rev tail) }
                    )
                | Error err -> Error err

    let endOfInput () : Parser<Unmemorized, _, unit, SourcePos> =
        make (fun (state, sourceFile, cursorIndex) ->
            match SourceFile.tryGetChar cursorIndex sourceFile with
            | pos, Some c -> Error pos
            | pos, None ->
                Ok(
                    state,
                    { NextIndex = cursorIndex
                      ParsedValue = () }
                ))

    let skipTill (parser: Parser<_, 'State, 'T, _>) : Parser<Unmemorized, 'State, 'T, unit> =
        let (Parser parse) =
            (endOfInput () |> map (fun () -> None) |> alt (parser |> map Some))

        Parser
        <| fun contextId ->
            let parseFn = parse contextId

            fun (state, sourceFile, cursorIndex) ->
                let mutable result = Unchecked.defaultof<ParseResult<'State, 'T, _>>

                let mutable currentIndex = cursorIndex

                let mutable shouldContinue = true

                while shouldContinue do
                    match parseFn (state, sourceFile, currentIndex) with
                    | Ok(newState,
                         { NextIndex = nextIndex
                           ParsedValue = Some parsedValue }) ->
                        result <-
                            Ok(
                                newState,
                                { NextIndex = nextIndex
                                  ParsedValue = parsedValue }
                            )

                        shouldContinue <- false
                    | Ok(_newState,
                         { NextIndex = nextIndex
                           ParsedValue = None }) ->
                        result <- Error()
                        shouldContinue <- false
                    | Error _ -> currentIndex <- currentIndex + 1

                result

    let forwardRef<'MemorizedOrNot, 'State, 'T, 'E when 'MemorizedOrNot :> MemorizedOrNot>
        ()
        : Parser<'MemorizedOrNot, 'State, 'T, 'E> * (Parser<'MemorizedOrNot, 'State, 'T, 'E> ref) =
        let dummy = Parser <| fun _contextId _input -> failwith "parser not initialized"

        let r = ref dummy

        let proxy =
            Parser
            <| fun contextId ->
                let (Parser parse) = r.Value
                parse contextId

        (proxy, r)
