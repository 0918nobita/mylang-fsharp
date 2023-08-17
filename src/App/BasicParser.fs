module BasicParser

open Reader
open ParserCombinator

[<RequireQualifiedAccess>]
type CharParserError =
    | UnexpectedChar of char
    | UnexpectedEndOfInput

let inline pchar (c: char) : Parser<char, CharParserError> =
    Parser.make
    <| reader {
        let! (stream, index) = Reader.ask
        stream.Seek(index)

        return
            match stream.Next() with
            | Some c' when c = c' -> Ok(c, stream.Index)
            | Some c' -> Error(CharParserError.UnexpectedChar c')
            | None -> Error CharParserError.UnexpectedEndOfInput
    }

[<RequireQualifiedAccess>]
type SatisfyParserError =
    | UnexpectedChar of char
    | UnexpectedEndOfInput

let inline satisfy ([<InlineIfLambda>] guard: char -> bool) : Parser<char, SatisfyParserError> =
    Parser.make
    <| reader {
        let! (stream, index) = Reader.ask
        stream.Seek(index)

        return
            match stream.Next() with
            | Some c when guard c -> Ok(c, stream.Index)
            | Some c -> Error(SatisfyParserError.UnexpectedChar c)
            | None -> Error SatisfyParserError.UnexpectedEndOfInput
    }

[<RequireQualifiedAccess>]
type StringParserError =
    | UnexpectedChar of char
    | UnexpectedEndOfInput

let inline pstring (str: string) : Parser<string, StringParserError> =
    Parser.make
    <| reader {
        let! (stream, index) = Reader.ask
        stream.Seek(index)

        let len = String.length str

        let mutable error: Option<StringParserError> = None

        let mutable shouldContinue = true

        let mutable strIndex = 0

        while shouldContinue && (strIndex < len) do
            match stream.Next() with
            | Some c when c = str.[strIndex] -> strIndex <- strIndex + 1
            | Some c ->
                error <- Some(StringParserError.UnexpectedChar c)
                shouldContinue <- false
            | None -> shouldContinue <- false

        return
            match error with
            | Some err -> Error err
            | None -> Ok(str, stream.Index)
    }
