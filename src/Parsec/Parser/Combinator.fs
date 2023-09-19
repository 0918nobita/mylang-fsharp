module Parsec.Parser.Combinator

open Type
open CE

/// <summary>指定したパーサを省略可能なものに変換し、省略されなかった場合結果は <c>Some</c> に包んで返す</summary>
let inline opt (parse: Parser<'State, 'T>) : Parser<'State, Option<'T>> =
    fun (state, reader) ->
        match (Prim.restorePos parse) (state, reader) with
        | Ok success ->
            Ok
                { NewState = success.NewState
                  ParsedValue = Some success.ParsedValue }
        | Error { Kind = Soft } -> Ok { NewState = state; ParsedValue = None }
        | Error failure -> Error failure

/// 指定したパーサを省略可能なものに変換し、結果値を無視する
let inline optional (parse: Parser<'State, 'T>) : Parser<'State, unit> =
    fun (state, reader) ->
        match (Prim.restorePos parse) (state, reader) with
        | Ok success ->
            Ok
                { NewState = success.NewState
                  ParsedValue = () }
        | Error { Kind = Soft } -> Ok { NewState = state; ParsedValue = () }
        | Error failure -> Error failure

/// 0 回以上の繰り返し
let inline many (parse: Parser<'State, 'T>) : Parser<'State, list<'T>> =
    let parse' = Prim.restorePos parse

    let rec inner reader state list =
        match parse' (state, reader) with
        | Ok success -> inner reader success.NewState (success.ParsedValue :: list)
        | Error { Kind = Soft } ->
            Ok
                { NewState = state
                  ParsedValue = List.rev list }
        | Error failure -> Error failure

    fun (state, reader) -> inner reader state []

/// 1 回以上の繰り返し
let inline many1 (parse: Parser<'State, 'T>) : Parser<'State, 'T * list<'T>> =
    parser {
        let! head = parse
        let! tail = many parse
        return head, tail
    }

let skipMany (parse: Parser<'State, 'T>) : Parser<'State, unit> =
    let parse' = Prim.restorePos parse

    let rec inner reader state =
        match parse' (state, reader) with
        | Ok success -> inner reader success.NewState
        | Error { Kind = Soft } -> Ok { NewState = state; ParsedValue = () }
        | Error failure -> Error failure

    fun (state, reader) -> inner reader state

let skipMany1 (parse: Parser<'State, 'T>) : Parser<'State, unit> =
    parser {
        let! _ = parse
        return! skipMany parse
    }
