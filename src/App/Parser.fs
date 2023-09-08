module Mylang.Parser

open System
open Parsec

open Ast

module P = BasicParser

[<RequireQualifiedAccess>]
type IdentSyntaxError =
    | InvalidHeadChar of {| Found: char; SourcePos: SourcePos |}
    | UnexpectedEndOfInput of {| SourcePos: SourcePos |}

let private identHeadChar c = Char.IsAsciiLetter c || c = '_'

let private identTailChar c = Char.IsAsciiLetterOrDigit c || c = '_'

let private identParser =
    parser {
        let! (head, headPos) =
            P.satisfy identHeadChar
            |> Parser.mapError (fun err ->
                match err with
                | P.SatisfyParserError.UnexpectedChar payload ->
                    IdentSyntaxError.InvalidHeadChar
                        {| Found = payload.Found
                           SourcePos = payload.SourcePos |}
                | P.SatisfyParserError.UnexpectedEndOfInput payload ->
                    IdentSyntaxError.UnexpectedEndOfInput {| SourcePos = payload.SourcePos |})

        let! tail = P.satisfy identTailChar |> Parser.map fst |> Parser.many
        let raw = head :: tail |> List.toArray |> String
        return ({ SourcePos = headPos; Raw = raw }: Identifier)
    }
    |> Parser.memorize

[<RequireQualifiedAccess>]
type private IntLiteralSyntaxError =
    | InvalidHeadChar of {| Found: char; SourcePos: SourcePos |}
    | UnexpectedEndOfInput of {| SourcePos: SourcePos |}

    member inline this.SourcePos =
        match this with
        | InvalidHeadChar payload -> payload.SourcePos
        | UnexpectedEndOfInput payload -> payload.SourcePos

let private intLiteralParser: Parser<Memorized, unit, IntLiteral, IntLiteralSyntaxError> =
    parser {
        let! (firstDigit, headPos) =
            P.satisfy Char.IsAsciiDigit
            |> Parser.mapError (fun err ->
                match err with
                | P.SatisfyParserError.UnexpectedChar payload ->
                    IntLiteralSyntaxError.InvalidHeadChar
                        {| Found = payload.Found
                           SourcePos = payload.SourcePos |}
                | P.SatisfyParserError.UnexpectedEndOfInput payload ->
                    IntLiteralSyntaxError.UnexpectedEndOfInput {| SourcePos = payload.SourcePos |})

        let! succeedingDigits = P.satisfy Char.IsAsciiDigit |> Parser.map fst |> Parser.many
        let raw = firstDigit :: succeedingDigits |> List.toArray |> String
        return ({ SourcePos = headPos; Raw = raw }: IntLiteral)
    }
    |> Parser.memorize

[<RequireQualifiedAccess>]
type ExpressionSyntaxError =
    | ExpressionNotFound of SourcePos
    | FuncallLeftParenthesisNotFound of SourcePos
    | FuncallRightParenthesisNotFound of SourcePos

let private exprParser, private exprParserRef =
    Parser.forwardRef<Memorized, unit, Expression, ExpressionSyntaxError> ()

let private factorParser =
    (identParser |> Parser.map Identifier)
    |> Parser.alt (intLiteralParser |> Parser.map IntLiteral)
    |> Parser.mapError (fun err -> ExpressionSyntaxError.ExpressionNotFound err.SourcePos)
    |> Parser.memorize

let private argumentsParser =
    parser {
        let! _ =
            P.pchar '('
            |> Parser.mapError (fun err -> ExpressionSyntaxError.FuncallLeftParenthesisNotFound err.SourcePos)

        let! firstArgument = exprParser

        let! _ =
            P.pchar ')'
            |> Parser.mapError (fun err -> ExpressionSyntaxError.FuncallRightParenthesisNotFound err.SourcePos)

        return [| firstArgument |]
    }

exprParserRef.Value <-
    parser {
        let! factor = factorParser

        let! argumentArrays = argumentsParser |> Parser.many

        let folder (state: Expression) (item: Expression[]) : Expression =
            Funcall { Callee = state; Arguments = item }

        return argumentArrays |> List.fold folder factor
    }
    |> Parser.memorize

[<RequireQualifiedAccess>]
type LetStmtSyntaxError =
    | LetNotFound of SourcePos
    | SpacesBeforeIdentnotFound of SourcePos
    | Ident of IdentSyntaxError
    | EqualNotFound of SourcePos
    | Initializer of ExpressionSyntaxError

let private letStmtParser =
    parser {
        let! _ =
            P.pstring "let"
            |> Parser.mapError (fun err -> LetStmtSyntaxError.LetNotFound err.SourcePos)

        let! _ =
            P.pchar ' '
            |> Parser.some
            |> Parser.mapError (fun err -> LetStmtSyntaxError.SpacesBeforeIdentnotFound err.SourcePos)

        let! ident = identParser |> Parser.mapError LetStmtSyntaxError.Ident
        let! _ = P.pchar ' ' |> Parser.many

        let! _ =
            P.pchar '='
            |> Parser.mapError (fun err -> LetStmtSyntaxError.EqualNotFound err.SourcePos)

        let! _ = P.pchar ' ' |> Parser.many
        let! init = exprParser |> Parser.mapError LetStmtSyntaxError.Initializer
        return (ident, init)
    }
    |> Parser.memorize

let private myParser: Parser<Memorized, unit, Option<Identifier * Expression> * LetStmtSyntaxError list, unit> =
    letStmtParser
    |> Parser.map (fun letStmt -> (Some letStmt, []))
    |> Parser.recover (fun err ->
        Parser.skipTill (P.pchar ';')
        |> Parser.map (fun _ -> (None, [ err ]))
        |> Parser.mapError ignore)
    |> Parser.memorize

let parse (source: string) =
    let src = SourceFile.fromString source

    let parserContext = ParserContext src

    parserContext.Run(myParser, initialState = (), fromIndex = 0) |> Result.map snd
