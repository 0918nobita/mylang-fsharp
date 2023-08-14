module BasicParser

open Reader
open ParserCombinator

[<RequireQualifiedAccess>]
type CharParserError =
    | UnexpectedChar of char
    | UnexpectedEndOfInput

let pchar (c: char) : Parser<char, CharParserError> =
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

let satisfy (guard: char -> bool) : Parser<char, SatisfyParserError> =
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
