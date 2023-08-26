module Mylang.Program

open System
open Parsec

open Ast

module P = BasicParser

let src = SourceFile.fromString "let foo = func(12)(13);"

let parserContext = ParserContext src

[<RequireQualifiedAccess>]
type IdentSyntaxError =
    | InvalidHeadChar of {| Found: char; SourcePos: SourcePos |}
    | UnexpectedEndOfInput of {| SourcePos: SourcePos |}

let identHeadChar c = Char.IsAsciiLetter c || c = '_'

let identTailChar c = Char.IsAsciiLetterOrDigit c || c = '_'

let identParser =
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
    |> parserContext.Memorize

[<RequireQualifiedAccess>]
type IntLiteralSyntaxError =
    | InvalidHeadChar of {| Found: char; SourcePos: SourcePos |}
    | UnexpectedEndOfInput of {| SourcePos: SourcePos |}

    member inline this.SourcePos =
        match this with
        | InvalidHeadChar payload -> payload.SourcePos
        | UnexpectedEndOfInput payload -> payload.SourcePos

let intLiteralParser: Parser<Memorized, unit, IntLiteral, IntLiteralSyntaxError> =
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
        return { SourcePos = headPos; Raw = raw }
    }
    |> parserContext.Memorize

[<RequireQualifiedAccess>]
type ExpressionSyntaxError =
    | ExpressionNotFound of SourcePos
    | FuncallLeftParenthesisNotFound of SourcePos
    | FuncallRightParenthesisNotFound of SourcePos

let exprParser, exprParserRef =
    Parser.forwardRef<Memorized, unit, Expression, ExpressionSyntaxError> ()

let factorParser =
    (identParser |> Parser.map Identifier)
    |> Parser.alt (intLiteralParser |> Parser.map IntLiteral)
    |> Parser.mapError (fun err -> ExpressionSyntaxError.ExpressionNotFound err.SourcePos)
    |> parserContext.Memorize

let argumentsParser =
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
    |> parserContext.Memorize

[<RequireQualifiedAccess>]
type LetStmtSyntaxError =
    | LetNotFound of SourcePos
    | SpacesBeforeIdentnotFound of SourcePos
    | Ident of IdentSyntaxError
    | EqualNotFound of SourcePos
    | Initializer of ExpressionSyntaxError

let letStmtParser =
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
    |> parserContext.Memorize

let myParser: Parser<Memorized, unit, Option<Identifier * Expression> * LetStmtSyntaxError list, unit> =
    letStmtParser
    |> Parser.map (fun letStmt -> (Some letStmt, []))
    |> Parser.recover (fun err ->
        Parser.skipTill (P.pchar ';')
        |> Parser.map (fun _ -> (None, [ err ]))
        |> Parser.mapError ignore)
    |> parserContext.Memorize

parserContext.Run(myParser, initialState = (), fromIndex = 0) |> printfn "%A"
