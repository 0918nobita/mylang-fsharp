module Parsec.Parser.Prim

open System.Collections.Generic

open Type

/// メモ化済みのパーサの実体を返す
let inline memorize (parse: Parser<'State, 'T>) : Parser<'State, 'T> =
    let memo = Dictionary<int * 'State, ParseResult<'State, 'T>>()

    fun (state, reader) ->
        let currentIndex = reader.CurrentIndex

        if memo.ContainsKey((currentIndex, state)) then
            memo.[currentIndex, state]
        else
            let result = parse (state, reader)
            memo.[(currentIndex, state)] <- result
            result

/// <summary><c>succeed parsedValue</c> は、実行されると必ず成功して、状態を遷移せずに結果値 <c>parsedValue</c> を返すパーサの実体をつくる</summary>
let inline succeed (parsedValue: 'T) : Parser<'State, 'T> =
    fun (state, _reader) ->
        Ok
            { NewState = state
              ParsedValue = parsedValue }

/// <summary>バックトラック可能なパーサに変換する</summary>
/// <remarks>
/// <c>opt</c> , <c>many</c> , <c>some</c> , <c>alt</c> パーサの内部で呼び出された際に Hard Failure を発生させるとバックトラックせずにエラー内容が伝播するが、
/// <c>backtrackable</c> を通じて Hard Failure を Soft Failure に変換することで、バックトラックを有効化できる
/// </remarks>
let inline backtrackable (parse: Parser<'State, 'T>) : Parser<'State, 'T> =
    fun (state, reader) ->
        parse (state, reader)
        |> Result.mapError (fun failure -> { failure with Kind = Soft })

/// <summary>成功した場合は結果値を関数 <c>mapping</c> で変換したうえで返すパーサの実体をつくる</summary>
let inline map ([<InlineIfLambda>] mapping: 'T -> 'U) (parse: Parser<'State, 'T>) : Parser<'State, 'U> =
    fun (state, reader) ->
        parse (state, reader)
        |> Result.map (fun success ->
            { NewState = success.NewState
              ParsedValue = mapping success.ParsedValue })

/// <summary>パーサが失敗した際に、入力ストリームの読み取り位置をパーサ実行前の位置に復元する</summary>
/// <note>ライブラリの外部からは直接利用しないでください</note>
let inline restorePos (parse: Parser<'State, 'T>) : Parser<'State, 'T> =
    fun (state, reader) ->
        let baseIndex = reader.CurrentIndex
        let res = parse (state, reader)

        if Result.isError res then
            reader.CurrentIndex <- baseIndex

        res

/// <summary>パーサの連接</summary>
/// <remarks>
/// <c>bind binder parser</c> は、まず <c>parser</c> を実行する。
/// 成功した場合には <c>binder</c> を適用して得られるパーサを続けて実行し、その結果を返す。
/// 失敗した場合にはエラー内容を伝播する。
/// </remarks>
let inline bind ([<InlineIfLambda>] binder: 'T -> Parser<'State, 'U>) (parse: Parser<'State, 'T>) : Parser<'State, 'U> =
    fun (state, reader) ->
        parse (state, reader)
        |> Result.bind (fun success ->
            let parse' = binder success.ParsedValue
            parse' (success.NewState, reader))

let forwardRef<'State, 'T> () : Parser<'State, 'T> * ref<Parser<'State, 'T>> =
    let dummy = fun _ -> failwith "parser not initialized"
    let r: ref<Parser<'State, 'T>> = ref dummy
    let proxy: Parser<'State, 'T> = fun (state, reader) -> r.Value(state, reader)
    (proxy, r)
