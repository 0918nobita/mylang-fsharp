/// 基本的なパーサ
module Parsec.BasicParser

open SourcePos

let inline endOfFile () : Parser<Memorized, 'State, unit> =
    Parser.make (fun (state, stream) ->
        match stream.ReadNextChar() with
        | _, None -> Success { NextState = state; ParsedValue = () }
        | pos, Some c ->
            HardFailure
                { Actual = $"'%c{c}'"
                  Expected = "[EOF]"
                  SourcePos = pos })
    |> Parser.memorize

/// <summary>指定された1文字を受理して、その文字と位置を返すパーサ</summary>
/// <param name="c">想定する文字</param>
let inline pchar (c: char) : Parser<_, 'State, char * SourcePos> =
    Parser.make (fun (state, stream) ->
        match stream.ReadNextChar() with
        | pos, Some c' when c = c' ->
            Success
                { NextState = state
                  ParsedValue = (c, pos) }
        | pos, Some c' ->
            HardFailure
                { Actual = $"'%c{c'}'"
                  Expected = $"'%c{c}'"
                  SourcePos = pos }
        | pos, None ->
            HardFailure
                { Actual = "[Reached EOF]"
                  Expected = $"'%c{c}'"
                  SourcePos = pos })

/// <summary>条件を満たす1文字を受理して、その文字と開始位置を返すパーサ</summary>
/// <param name="helpMsg">失敗した場合に「どんな文字を想定していたのか」を伝えるメッセージ</param>
/// <param name="guard">読み取った文字が想定通りかチェックする条件</param>
let inline satisfy
    (helpMsg: string)
    ([<InlineIfLambda>] guard: char -> bool)
    : Parser<Unmemorized, 'State, char * SourcePos> =
    Parser.make (fun (state, stream) ->
        match stream.ReadNextChar() with
        | pos, Some c when guard c ->
            Success
                { NextState = state
                  ParsedValue = (c, pos) }
        | pos, Some c ->
            HardFailure
                { Actual = $"'%c{c}'"
                  Expected = helpMsg
                  SourcePos = pos }
        | pos, None ->
            HardFailure
                { Actual = "[Reached EOF]"
                  Expected = helpMsg
                  SourcePos = pos })

let inline anyChar () : Parser<Memorized, 'State, char * SourcePos> =
    satisfy "any char" (fun _ -> true) |> Parser.memorize

let inline whiteSpaces () : Parser<Memorized, 'State, unit> =
    satisfy "white space" System.Char.IsWhiteSpace
    |> Parser.backtrackable
    |> Parser.many
    |> Parser.map ignore
    |> Parser.memorize

let inline whiteSpaces1 () : Parser<Memorized, 'State, unit> =
    satisfy "white space" System.Char.IsWhiteSpace
    |> Parser.backtrackable
    |> Parser.some
    |> Parser.map ignore
    |> Parser.memorize

/// <summary>指定された文字列を受理して、その文字列と開始位置を返すパーサ</summary>
/// <param name="str">想定する文字列</param>
let inline pstring (str: string) : Parser<Unmemorized, 'State, string * SourcePos> =
    Parser.make (fun (state, stream) ->
        let len = String.length str

        let startPos = stream.CurrentPos

        let mutable error: Option<ParseFailure> = None

        let mutable shouldContinue = true

        let mutable strIndex = 0

        while shouldContinue && (strIndex < len) do
            match stream.ReadNextChar() with
            | _pos, Some c when c = str.[strIndex] -> strIndex <- strIndex + 1
            | pos, Some c ->
                error <-
                    Some
                        { Actual = $"'%c{c}'"
                          Expected = $"'%c{str.[strIndex]}'"
                          SourcePos = pos }

                shouldContinue <- false
            | pos, None ->
                error <-
                    Some
                        { Actual = "[Reached EOF]"
                          Expected = $"'%c{str.[strIndex]}'"
                          SourcePos = pos }

                shouldContinue <- false

        match error with
        | Some err -> HardFailure err
        | None ->
            Success
                { NextState = state
                  ParsedValue = (str, startPos) })
