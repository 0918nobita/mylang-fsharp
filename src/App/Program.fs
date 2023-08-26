module Program

open System

open Parsec

module P = BasicParser

let src = SourceFile.fromString "let foo = func(12)(13);"

let parserContext = ParserContext src

type Identifier = { SourcePos: SourcePos; Raw: string }

let identParser: Parser<Memorized, unit, Identifier, SourcePos> =
    parser {
        let! (head, headPos) = P.satisfy Char.IsAsciiLetter |> Parser.mapError (fun err -> err.SourcePos)
        let! tail = P.satisfy Char.IsAsciiLetterOrDigit |> Parser.map fst |> Parser.many
        let raw = head :: tail |> List.toArray |> String
        return { SourcePos = headPos; Raw = raw }
    }
    |> parserContext.Memorize

type IntLiteral = { SourcePos: SourcePos; Raw: string }

let intLiteralParser: Parser<Memorized, unit, IntLiteral, SourcePos> =
    parser {
        let! (firstDigit, headPos) = P.satisfy Char.IsAsciiDigit |> Parser.mapError (fun err -> err.SourcePos)
        let! succeedingDigits = P.satisfy Char.IsAsciiDigit |> Parser.map fst |> Parser.many
        let raw = firstDigit :: succeedingDigits |> List.toArray |> String
        return { SourcePos = headPos; Raw = raw }
    }
    |> parserContext.Memorize

type Expression =
    | Identifier of Identifier
    | IntLiteral of IntLiteral
    | Funcall of Funcall

and Funcall =
    { Callee: Expression
      Arguments: Expression[] }

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
    |> Parser.mapError ExpressionSyntaxError.ExpressionNotFound
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
    | IdentNotFound of SourcePos
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

        let! ident = identParser |> Parser.mapError LetStmtSyntaxError.IdentNotFound
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

parserContext.Run(myParser, initialState = (), fromIndex = 0)
|> printfn "\n--- exprParser ---\n%A"
