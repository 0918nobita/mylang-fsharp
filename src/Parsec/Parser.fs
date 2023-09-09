/// パーサの型定義・操作・合成
[<AutoOpen>]
module Parsec.Parser

open System.Collections.Generic
open System.Runtime.CompilerServices

/// <summary>パース成功</summary>
/// <remarks>遷移後の状態と結果値を内包する</remarks>
/// <typeparam name="State">パーサが持ち回る状態</typeparam>
/// <typeparam name="T">結果値</typeparam>
[<IsReadOnly; Struct>]
type ParseSuccess<'State, 'T> = { NextState: 'State; ParsedValue: 'T }

/// <summary>パース失敗</summary>
/// <remarks>パース結果が <c>SoftFailure</c> または <c>HardFailure</c> の場合に得られる「想定していた入力」「実際の入力」「発生位置」に関する情報</remarks>
[<IsReadOnly; Struct>]
type ParseFailure =
    { Actual: string
      Expected: string
      SourcePos: SourcePos }

/// <summary>パース結果</summary>
/// <remarks>失敗した際には、バックトラックを許す場合は <c>SoftFailure</c>、バックトラックを禁止する場合は <c>HardFailure</c> で表現する。</remarks>
/// <typeparam name="State">パーサが持ち回る状態</typeparam>
/// <typeparam name="T">パースに成功した場合の結果値</typeparam>
[<IsReadOnly; Struct>]
type ParseResult<'State, 'T> =
    | Success of success: ParseSuccess<'State, 'T>
    | SoftFailure of softFailure: ParseFailure
    | HardFailure of hardFailure: ParseFailure

/// <summary>ユーザが直接記述するパーサ実装</summary>
/// <remarks><c>Parser.make</c> に渡して、メモ化の設定が可能なパーサの実体に変換する</remarks>
type ParseFn<'State, 'T> = 'State * ISourceCharStream -> ParseResult<'State, 'T>

/// パーサのコンテキストの識別子
type ContextId = int

/// パーサがメモ化されているかどうかを表す幽霊型の制約
type MemorizedOrNot =
    interface
    end

/// パーサがメモ化されていることを表す幽霊型
type Memorized =
    interface
        inherit MemorizedOrNot
    end

/// パーサがメモ化されていないことを表す幽霊型
type Unmemorized =
    interface
        inherit MemorizedOrNot
    end

/// <summary>パーサの実体</summary>
/// <typeparam name="MemorizedOrNot">メモ化されているかどうかを表す幽霊型</typeparam>
/// <typeparam name="State">パーサが持ち回る状態</typeparam>
/// <typeparam name="T">パースに成功した場合の結果値</typeparam>
type Parser<'MemorizedOrNot, 'State, 'T when 'MemorizedOrNot :> MemorizedOrNot> =
    | Parser of Reader<ContextId, ParseFn<'State, 'T>>

/// パーサの実体に対する操作・合成
module Parser =
    /// 新たにパーサの実体をつくる
    let inline make (parseFn: ParseFn<'State, 'T>) : Parser<Unmemorized, 'State, 'T> =
        Parser <| reader { return parseFn }

    /// まだメモ化されていないパーサの実体を引数に取り、メモ化済みのパーサの実体を返す
    let inline memorize (Parser parse: Parser<Unmemorized, 'State, 'T>) : Parser<Memorized, 'State, 'T> =
        let memo = Dictionary<ContextId * int * 'State, ParseResult<'State, 'T>>()

        Parser
        <| reader {
            let! parseFn = parse

            let! contextId = Reader.ask

            return
                fun (state, stream) ->
                    let currentIndex = stream.CurrentIndex

                    if memo.ContainsKey((contextId, currentIndex, state)) then
                        memo.[contextId, currentIndex, state]
                    else
                        let result = parseFn (state, stream)
                        memo.[(contextId, currentIndex, state)] <- result
                        result
        }

    /// <summary><c>succeed parsedValue</c> は、実行されると必ず成功して、状態を遷移せずに結果値 <c>parsedValue</c> を返すパーサの実体をつくる</summary>
    let inline succeed (parsedValue: 'T) : Parser<'Unmemorized, 'State, 'T> =
        Parser
        <| reader {
            return
                fun (state, _stream) ->
                    Success
                        { NextState = state
                          ParsedValue = parsedValue }
        }

    /// <summary>バックトラック可能なパーサに変換する</summary>
    /// <remarks>
    /// <c>opt</c> , <c>many</c> , <c>some</c> , <c>alt</c> パーサの内部で呼び出された際に Hard Failure を発生させるとバックトラックせずにエラー内容が伝播するが、
    /// <c>backtrackable</c> を通じて Hard Failure を一律に Soft Failure に変換することで、バックトラックが発生するようになる
    /// </remarks>
    let inline backtrackable (Parser parse: Parser<_, 'State, 'T>) : Parser<Unmemorized, 'State, 'T> =
        Parser
        <| reader {
            let! parseFn = parse

            return
                fun (state, stream) ->
                    match parseFn (state, stream) with
                    | Success success -> Success success
                    | SoftFailure failure -> SoftFailure failure
                    | HardFailure failure -> SoftFailure failure
        }

    /// <summary>パーサが失敗した際に、入力ストリームの読み取り位置をパーサ実行前の位置に復元する</summary>
    /// <note><c>Parsec.Parser</c> モジュールの外部からは直接利用しないでください</note>
    let inline restorePos (parse: Reader<ContextId, ParseFn<'State, 'T>>) : Reader<ContextId, ParseFn<'State, 'T>> =
        reader {
            let! parseFn = parse

            return
                fun (state, stream) ->
                    let baseIndex = stream.CurrentIndex

                    match parseFn (state, stream) with
                    | Success success -> Success success
                    | SoftFailure failure ->
                        stream.CurrentIndex <- baseIndex
                        SoftFailure failure
                    | HardFailure failure ->
                        stream.CurrentIndex <- baseIndex
                        HardFailure failure
        }

    /// <summary>パーサの現在の状態 <c>'State</c> を結果値として取り出すパーサの実体をつくる</summary>
    let inline getState () : Parser<Unmemorized, 'State, 'State> =
        Parser
        <| reader {
            return
                fun (state, _stream) ->
                    Success
                        { NextState = state
                          ParsedValue = state }
        }

    /// <summary><c>putState state</c> はパーサの状態を <c>state</c> に更新するパーサの実体をつくる</summary>
    let inline putState (state: 'State) : Parser<Unmemorized, 'State, unit> =
        Parser
        <| reader { return fun (_state, _stream) -> Success { NextState = state; ParsedValue = () } }

    /// <summary><c>modifyState mapping</c> は、パーサの状態に対して関数 <c>mapping</c> を適用して更新するパーサの実体をつくる</summary>
    let inline modifyState ([<InlineIfLambda>] mapping: 'State -> 'State) : Parser<Unmemorized, 'State, unit> =
        Parser
        <| reader {
            return
                fun (state, _stream) ->
                    Success
                        { NextState = mapping state
                          ParsedValue = () }
        }

    /// <summary>成功した場合は結果値を関数 <c>mapping</c> で変換したうえで返すパーサの実体をつくる</summary>
    let inline map
        ([<InlineIfLambda>] mapping: 'T -> 'U)
        (Parser parse: Parser<_, 'State, 'T>)
        : Parser<Unmemorized, 'State, 'U> =
        Parser
        <| reader {
            let! parseFn = parse

            return
                fun (state, stream) ->
                    match parseFn (state, stream) with
                    | Success success ->
                        Success
                            { NextState = success.NextState
                              ParsedValue = mapping success.ParsedValue }
                    | SoftFailure failure -> SoftFailure failure
                    | HardFailure failure -> HardFailure failure
        }

    let inline mapError
        ([<InlineIfLambda>] mapping: ParseFailure -> ParseFailure)
        (Parser parse: Parser<_, 'State, 'T>)
        : Parser<Unmemorized, 'State, 'T> =
        Parser
        <| reader {
            let! parseFn = parse

            return
                fun (state, stream) ->
                    match parseFn (state, stream) with
                    | Success success -> Success success
                    | SoftFailure failure -> SoftFailure(mapping failure)
                    | HardFailure failure -> HardFailure(mapping failure)
        }

    /// <summary>パーサの連接</summary>
    /// <remarks>
    /// <c>bind binder parser</c> は、まず <c>parser</c> を実行する。
    /// 成功した場合には <c>binder</c> を適用して得られるパーサを続けて実行し、その結果を返す。
    /// 失敗した場合にはエラー内容を伝播する。
    /// </remarks>
    let inline bind
        ([<InlineIfLambda>] binder: 'T -> Parser<_, 'State, 'U>)
        (Parser parse: Parser<_, 'State, 'T>)
        : Parser<Unmemorized, 'State, 'U> =
        Parser
        <| reader {
            let! parseFn = parse

            let! contextId = Reader.ask

            return
                fun (state, stream) ->
                    match parseFn (state, stream) with
                    | Success success ->
                        let (Parser parse) = binder success.ParsedValue
                        let parseFn = Reader.run contextId parse
                        parseFn (success.NextState, stream)
                    | SoftFailure failure -> SoftFailure failure
                    | HardFailure failure -> HardFailure failure
        }

    let inline opt (Parser parse: Parser<_, 'State, 'T>) : Parser<Unmemorized, 'State, Option<'T>> =
        reader {
            let! parseFn = restorePos parse

            return
                fun (state, stream) ->
                    match parseFn (state, stream) with
                    | Success success ->
                        Success
                            { NextState = success.NextState
                              ParsedValue = Some success.ParsedValue }
                    | SoftFailure _failure ->
                        Success
                            { NextState = state
                              ParsedValue = None }
                    | HardFailure failure -> HardFailure failure
        }
        |> Parser

    /// <summary>１つ目のパーサを試し、失敗した場合には２つ目のパーサを試す</summary>
    /// <remarks>
    /// <c>alt parser2 parser1</c> はまず <c>parser1</c> を実行する。
    /// 成功した場合はその結果を伝播し、Soft Failure として失敗した場合には <c>parser2</c> を実行してその結果を返す。
    /// Hard Failure として失敗した場合は <c>parser2</c> は実行されず、エラー内容を伝播する。
    /// </remarks>
    let inline alt
        (Parser parse2: Parser<_, 'State, 'T>)
        (Parser parse1: Parser<_, 'State, 'T>)
        : Parser<Unmemorized, 'State, 'T> =
        reader {
            let! parseFn1 = restorePos parse1
            let! parseFn2 = parse2

            return
                fun (state, stream) ->
                    match parseFn1 (state, stream) with
                    | Success success -> Success success
                    | SoftFailure _failure -> parseFn2 (state, stream)
                    | HardFailure failure -> HardFailure failure
        }
        |> Parser

    /// <summary>エラー復帰を表現するコンビネータ</summary>
    /// <remarks>1つ目のパーサで失敗した場合、そのエラー原因をもとに生成した2つ目のパーサを実行する</remarks>
    let inline recover
        ([<InlineIfLambda>] recovery: ParseFailure -> Parser<_, 'State, 'T>)
        (Parser parse: Parser<_, 'State, _>)
        : Parser<Unmemorized, 'State, 'T> =
        Parser
        <| reader {
            let! parseFn = restorePos parse

            let! contextId = Reader.ask

            return
                fun (state, stream) ->
                    match parseFn (state, stream) with
                    | Success success -> Success success
                    | SoftFailure failure
                    | HardFailure failure ->
                        let (Parser parse') = recovery failure
                        let parseFn' = Reader.run contextId parse'
                        parseFn' (state, stream)
        }

    let inline forwardRef<'MemorizedOrNot, 'State, 'T when 'MemorizedOrNot :> MemorizedOrNot>
        ()
        : Parser<'MemorizedOrNot, 'State, 'T> * (Parser<'MemorizedOrNot, 'State, 'T> ref) =
        let dummy = Parser <| reader { return fun _ -> failwith "parser not initialized" }

        let r = ref dummy

        let proxy =
            Parser
            <| Reader(fun contextId ->
                let (Parser parse) = r.Value
                Reader.run contextId parse)

        (proxy, r)

    /// 0回以上の繰り返し
    let inline many (Parser parse: Parser<_, 'State, 'T>) : Parser<Unmemorized, 'State, 'T list> =
        Parser
        <| reader {
            let! parseFn = parse

            return
                fun (prevState, stream) ->
                    let mutable hardFailureOpt: Option<ParseFailure> = None

                    let mutable results: 'T list = []

                    let mutable currentState = prevState

                    let mutable shouldContinue = true

                    while shouldContinue do
                        let baseIndex = stream.CurrentIndex

                        match parseFn (currentState, stream) with
                        | Success success ->
                            results <- success.ParsedValue :: results
                            currentState <- success.NextState
                        | SoftFailure _failure ->
                            stream.CurrentIndex <- baseIndex
                            shouldContinue <- false
                        | HardFailure failure ->
                            stream.CurrentIndex <- baseIndex
                            hardFailureOpt <- Some failure
                            shouldContinue <- false

                    match hardFailureOpt with
                    | Some failure -> HardFailure failure
                    | None ->
                        Success
                            { NextState = currentState
                              ParsedValue = List.rev results }
        }

    /// 1回以上の繰り返し
    let inline some (parser: Parser<_, 'State, 'T>) : Parser<Unmemorized, 'State, 'T * 'T list> =
        parser |> bind (fun head -> many parser |> map (fun tail -> (head, tail)))
