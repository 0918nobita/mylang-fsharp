module Program

open System

open SourcePos
open SourceFile
open ParserCombinator

module P = BasicParser

type SyntaxError =
    | LetStmtLetNotFound
    | LetStmtSpacesBeforeIdentNotFound
    | LetStmtIdentNotFound
    | LetStmtEqualNotFound
    | LetStmtInitializerNotFound

type Identifier = { SourcePos: SourcePos; Raw: string }

let identParser: Parser<Identifier, unit> =
    parser {
        let! (head, headPos) = P.satisfy Char.IsAsciiLetter |> Parser.mapError ignore
        let! tail = P.satisfy Char.IsAsciiLetterOrDigit |> Parser.many
        let raw = head :: (tail |> List.map fst) |> List.toArray |> String
        return { SourcePos = headPos; Raw = raw }
    }

type IntLiteral = { SourcePos: SourcePos; Raw: string }

let intLiteralParser: Parser<IntLiteral, unit> =
    parser {
        let! (firstDigit, headPos) = P.satisfy Char.IsAsciiDigit |> Parser.mapError ignore
        let! succeedingDigits = P.satisfy Char.IsAsciiDigit |> Parser.many
        let raw = firstDigit :: (succeedingDigits |> List.map fst) |> List.toArray |> String
        return { SourcePos = headPos; Raw = raw }
    }

type Expression =
    | Identifier of Identifier
    | IntLiteral of IntLiteral

let exprParser: Parser<Expression, unit> =
    (identParser |> Parser.map Identifier)
    |> Parser.alt (intLiteralParser |> Parser.map IntLiteral)

let letStmtParser =
    parser {
        let! _ = P.pstring "let" |> Parser.mapError (fun _ -> LetStmtLetNotFound)

        let! _ =
            P.pchar ' '
            |> Parser.some
            |> Parser.mapError (fun _ -> LetStmtSpacesBeforeIdentNotFound)

        let! ident = identParser |> Parser.mapError (fun _ -> LetStmtIdentNotFound)
        let! _ = P.pchar ' ' |> Parser.many
        let! _ = P.pchar '=' |> Parser.mapError (fun _ -> LetStmtEqualNotFound)
        let! _ = P.pchar ' ' |> Parser.many
        let! init = exprParser |> Parser.mapError (fun _ -> LetStmtInitializerNotFound)
        return (ident, init)
    }

let myParser: Parser<Option<Identifier * Expression> * SyntaxError list, unit> =
    letStmtParser
    |> Parser.map (fun letStmt -> (Some letStmt, []))
    |> Parser.recover (fun err -> Parser.succeed (None, [ err ]))

let sourceFile = SourceFile.fromString "let"

let stream = SourceFileStream sourceFile

Parser.run stream myParser |> printfn "%A"
