module ParserLib

open SourcePos
open SourceFilePos

type ParserInput<'C> = {
    Context: 'C
    Target: string
}

type ParseSuccess<'C, 'T> = {
    Context: 'C
    ParsedValue: 'T
    Rest: string
}

type ParseError<'C, 'E> = {
    Context: 'C
    Reason: 'E
    /// 飛ばして続きをパースする場合に入力となる文字列
    Rest: string
}

type ParseResult<'C, 'T, 'E> = Result<ParseSuccess<'C, 'T>, ParseError<'C, 'E>>

type Parser<'C, 'T, 'E> = ParserInput<'C> -> ParseResult<'C, 'T, 'E>

module Parser =
    let succeed (parsedValue: 'T) : Parser<'C, 'T, 'E> =
        fun { Context = context; Target = target } ->
            Ok {
                Context = context
                ParsedValue = parsedValue
                Rest = target
            }

    let map (mapping: 'T -> 'U) (parse: Parser<'C, 'T, 'E>) : Parser<'C, 'U, 'E> =
        fun parserInput ->
            parse parserInput
            |> Result.map (fun parseSuccess ->
                {
                    Context = parseSuccess.Context
                    ParsedValue = mapping parseSuccess.ParsedValue
                    Rest = parseSuccess.Rest
                })

    let bind (binder: 'T -> Parser<'C, 'U, 'E>) (parse: Parser<'C, 'T, 'E>) : Parser<'C, 'U, 'E> =
        fun parserInput ->
            parse parserInput
            |> Result.bind (fun parseSuccess ->
                binder parseSuccess.ParsedValue {
                    Context = parseSuccess.Context
                    Target = parseSuccess.Rest
                })

    let getContext : Parser<'C, 'C, 'E> =
        fun { Context = context; Target = target } ->
            Ok {
                Context = context
                ParsedValue = context
                Rest = target
            }

    let optional (parse: Parser<'C, 'T, 'E>) : Parser<'C, Option<'T>, 'E> =
        fun { Context = context; Target = target } ->
            match parse { Context = context; Target = target } with
            | Ok { Context = context'; ParsedValue = parsedValue; Rest = rest } ->
                Ok {
                    Context = context'
                    ParsedValue = Some parsedValue
                    Rest = rest
                }
            | Error _ ->
                Ok {
                    Context = context
                    ParsedValue = None
                    Rest = target
                }

type ParserBuilder() =
    member _.Bind(parser: Parser<'C, 'T, 'E>, binder: 'T -> Parser<'C, 'U, 'E>) : Parser<'C, 'U, 'E> = Parser.bind binder parser
    member _.Return(parsedValue: 'T) : Parser<'C, 'T, 'E> = Parser.succeed parsedValue
    member _.ReturnFrom(parser: Parser<'C, 'T, 'E>) : Parser<'C, 'T, 'E> = parser

let parser = ParserBuilder()

type LineBreak = Crlf | Lf

let charParser (c : char) : Parser<LineBreak * SourceFilePos, char, string> =
    fun { Context = (lineBreak, location); Target = target } ->
        match (lineBreak, Seq.tryHead target) with
        | (Crlf, Some '\r') ->
            Ok {
                Context = (Crlf, SourcePos.carriageReturn location)
                ParsedValue = '\r'
                Rest = target[1..]
            }
        | (Crlf, Some '\n') ->
            Ok {
                Context = (Crlf, SourcePos.lineFeed location)
                ParsedValue = '\n'
                Rest = target[1..]
            }
        | (Lf, Some '\n') ->
            Ok {
                Context = (Crlf, SourcePos.nextLine location)
                ParsedValue = '\n'
                Rest = target[1..]
            }
        | (_, Some c') when c' = c ->
            Ok {
                Context = (lineBreak, SourcePos.nextColumn location)
                ParsedValue = c
                Rest = target[1..]
            }
        | (_, Some c') ->
            Error {
                Context = (lineBreak, SourcePos.nextColumn location)
                Reason = $"char '%c{c}' expected, but char '%c{c'}' found"
                Rest = target[1..]
            }
        | (_, None) ->
            Error {
                Context = (lineBreak, SourcePos.nextColumn location)
                Reason = $"char '%c{c}' expected, but reached end of input"
                Rest = target[1..]
            }

let withLocation (parse: Parser<LineBreak * SourceFilePos, 'T, 'E>) : Parser<LineBreak * SourceFilePos, SourceFilePos * 'T, 'E> =
    parser {
        let! (_, location) = Parser.getContext
        let! res = parse
        return (location, res)
    }
