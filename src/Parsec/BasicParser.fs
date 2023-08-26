module Parsec.BasicParser

open SourcePos

[<RequireQualifiedAccess>]
type CharParserError =
    | UnexpectedChar of
        {| Expected: char
           Found: char
           SourcePos: SourcePos |}
    | UnexpectedEndOfInput of
        {| Expected: char
           SourcePos: SourcePos |}

    member inline this.SourcePos =
        match this with
        | UnexpectedChar payload -> payload.SourcePos
        | UnexpectedEndOfInput payload -> payload.SourcePos

/// 指定された1文字を受理して、その文字と位置を返すパーサ
let inline pchar (c: char) : Parser<_, _, char * SourcePos, CharParserError> =
    Parser.make (fun (state, sourceFile, cursorIndex) ->
        match SourceFile.tryGetChar cursorIndex sourceFile with
        | pos, Some c' when c = c' ->
            Ok(
                state,
                { NextIndex = cursorIndex + 1
                  ParsedValue = (c, pos) }
            )
        | pos, Some c' ->
            Error(
                CharParserError.UnexpectedChar
                    {| Expected = c
                       Found = c'
                       SourcePos = pos |}
            )
        | pos, None -> Error(CharParserError.UnexpectedEndOfInput {| Expected = c; SourcePos = pos |}))

[<RequireQualifiedAccess>]
type SatisfyParserError =
    | UnexpectedChar of {| Found: char; SourcePos: SourcePos |}
    | UnexpectedEndOfInput of {| SourcePos: SourcePos |}

    member inline this.SourcePos =
        match this with
        | UnexpectedChar payload -> payload.SourcePos
        | UnexpectedEndOfInput payload -> payload.SourcePos

let inline satisfy
    ([<InlineIfLambda>] guard: char -> bool)
    : Parser<Unmemorized, 'state, char * SourcePos, SatisfyParserError> =
    Parser.make (fun (state, sourceFile, cursorIndex) ->
        match SourceFile.tryGetChar cursorIndex sourceFile with
        | pos, Some c when guard c ->
            Ok(
                state,
                { NextIndex = cursorIndex + 1
                  ParsedValue = (c, pos) }
            )
        | pos, Some c -> Error(SatisfyParserError.UnexpectedChar {| Found = c; SourcePos = pos |})
        | pos, None -> Error(SatisfyParserError.UnexpectedEndOfInput {| SourcePos = pos |}))

[<RequireQualifiedAccess>]
type StringParserError =
    | UnexpectedChar of
        {| Expected: char
           Found: char
           SourcePos: SourcePos |}
    | UnexpectedEndOfInput of {| SourcePos: SourcePos |}

    member inline this.SourcePos =
        match this with
        | UnexpectedChar payload -> payload.SourcePos
        | UnexpectedEndOfInput payload -> payload.SourcePos

let inline pstring (str: string) : Parser<Unmemorized, unit, string * SourcePos, StringParserError> =
    Parser.make (fun (state, sourceFile, cursorIndex) ->
        let len = String.length str

        let mutable error: Option<StringParserError> = None

        let mutable shouldContinue = true

        let mutable strIndex = 0

        while shouldContinue && (strIndex < len) do
            match SourceFile.tryGetChar (cursorIndex + strIndex) sourceFile with
            | pos, Some c when c = str.[strIndex] -> strIndex <- strIndex + 1
            | pos, Some c ->
                error <-
                    Some(
                        StringParserError.UnexpectedChar
                            {| Expected = str.[strIndex]
                               Found = c
                               SourcePos = pos |}
                    )

                shouldContinue <- false
            | pos, None ->
                error <- Some(StringParserError.UnexpectedEndOfInput {| SourcePos = pos |})
                shouldContinue <- false

        match error with
        | Some err -> Error err
        | None ->
            Ok(
                state,
                { NextIndex = cursorIndex + len
                  ParsedValue = (str, SourceFile.position cursorIndex sourceFile) }
            ))
