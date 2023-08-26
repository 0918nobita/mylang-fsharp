module Program

open System

open Parsec

module P = BasicParser

let src = SourceFile.fromString "let foo = _; print foo"

let parserContext = ParserContext src

type SyntaxError =
    | LetStmtLetNotFound of SourcePos
    | LetStmtSpacesBeforeIdentnotFound of SourcePos
    | LetStmtIdentNotFound of SourcePos
    | LetStmtEqualNotFound of SourcePos
    | LetStmtInitializerNotFound of SourcePos

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

let exprParser: Parser<Memorized, unit, Expression, SourcePos> =
    (identParser |> Parser.map Identifier)
    |> Parser.alt (intLiteralParser |> Parser.map IntLiteral)
    |> parserContext.Memorize

let letStmtParser =
    parser {
        let! _ = P.pstring "let" |> Parser.mapError (fun err -> LetStmtLetNotFound err.SourcePos)

        let! _ =
            P.pchar ' '
            |> Parser.some
            |> Parser.mapError (fun err -> LetStmtSpacesBeforeIdentnotFound err.SourcePos)

        let! ident = identParser |> Parser.mapError LetStmtIdentNotFound
        let! _ = P.pchar ' ' |> Parser.many
        let! _ = P.pchar '=' |> Parser.mapError (fun err -> LetStmtEqualNotFound err.SourcePos)
        let! _ = P.pchar ' ' |> Parser.many
        let! init = exprParser |> Parser.mapError LetStmtInitializerNotFound
        return (ident, init)
    }
    |> parserContext.Memorize

let myParser: Parser<Memorized, unit, Option<Identifier * Expression> * SyntaxError list, unit> =
    letStmtParser
    |> Parser.map (fun letStmt -> (Some letStmt, []))
    |> Parser.recover (fun err ->
        Parser.skipTill (P.pchar ';')
        |> Parser.map (fun _ -> (None, [ err ]))
        |> Parser.mapError ignore)
    |> parserContext.Memorize

parserContext.Run(myParser, initialState = (), fromIndex = 0)
|> printfn "\n--- exprParser ---\n%A"
