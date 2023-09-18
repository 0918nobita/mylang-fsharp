module Mylang.MyParser

open System
open Parsec

module P = BasicParser

open Ast

let inline private identHeadChar c = Char.IsAsciiLetter c || c = '_'

let inline private identTailChar c = Char.IsAsciiLetterOrDigit c || c = '_'

let private identParser: Parser<Memorized, unit, Identifier> =
    parser {
        let! (head, headPos) = P.satisfy "ASCII letter or '_'" identHeadChar

        let! tail =
            P.satisfy "ASCII letter, digit or '_'" identTailChar
            |> Parser.map fst
            |> Parser.backtrackable
            |> Parser.many

        let raw = head :: tail |> List.toArray |> String

        return ({ SourcePos = headPos; Raw = raw }: Identifier)
    }
    |> Parser.memorize

let private intLiteralParser: Parser<Memorized, unit, IntLiteral> =
    parser {
        let! (firstDigit, headPos) = P.satisfy "ASCII digit" Char.IsAsciiDigit

        let! succeedingDigits =
            P.satisfy "ASCII digit" Char.IsAsciiDigit
            |> Parser.map fst
            |> Parser.backtrackable
            |> Parser.many

        let raw = firstDigit :: succeedingDigits |> List.toArray |> String
        return ({ SourcePos = headPos; Raw = raw }: IntLiteral)
    }
    |> Parser.memorize

let private charLiteralParser: Parser<Memorized, unit, CharLiteral> =
    parser {
        let! (_, pos) = P.pchar '\'' |> Parser.backtrackable
        let! (c, _) = P.anyChar ()
        let! _ = P.pchar '\''
        return ({ SourcePos = pos; Raw = c }: CharLiteral)
    }
    |> Parser.memorize

let inline private stringLiteralChar c = c <> '"'

let private stringLiteralParser: Parser<Memorized, unit, StringLiteral> =
    parser {
        let! (_, pos) = P.pchar '"' |> Parser.backtrackable

        let! content =
            P.satisfy "a character which is not double quote" stringLiteralChar
            |> Parser.map fst
            |> Parser.backtrackable
            |> Parser.many

        let! _ = P.pchar '"'

        return
            { SourcePos = pos
              Raw = content |> List.toArray |> String }
    }
    |> Parser.memorize

let private exprParser, private exprParserRef =
    Parser.forwardRef<Memorized, unit, Expression> ()

let private expr1Parser =
    parser {
        let! _ = P.pchar '(' |> Parser.backtrackable
        let! expr = exprParser
        let! _ = P.pchar ')'
        return expr
    }
    |> Parser.alt (identParser |> Parser.map Identifier)
    |> Parser.backtrackable
    |> Parser.alt (intLiteralParser |> Parser.map IntLiteral)
    |> Parser.backtrackable
    |> Parser.alt (charLiteralParser |> Parser.map CharLiteral)
    |> Parser.backtrackable
    |> Parser.alt (stringLiteralParser |> Parser.map StringLiteral)
    |> Parser.memorize

let private argumentsParser =
    parser {
        let! _ = P.pchar '(' |> Parser.backtrackable
        let! firstArgument = exprParser
        let! _ = P.pchar ')'
        return [| firstArgument |]
    }
    |> Parser.memorize

let private expr2Parser =
    parser {
        let! expr1 = expr1Parser

        let! argumentArrays = argumentsParser |> Parser.many

        let folder (state: Expression) (item: Expression[]) : Expression =
            Funcall { Callee = state; Arguments = item }

        return argumentArrays |> List.fold folder expr1
    }
    |> Parser.memorize

type MulOrDivOp =
    | MulOp
    | DivOp

let private mulOp: Parser<Memorized, unit, MulOrDivOp> =
    P.pchar '*' |> Parser.map (fun _ -> MulOp) |> Parser.memorize

let private divOp: Parser<Memorized, unit, MulOrDivOp> =
    P.pchar '/' |> Parser.map (fun _ -> DivOp) |> Parser.memorize

let private expr3Parser =
    parser {
        let! expr2 = expr2Parser

        do! P.whiteSpaces ()

        let! mulOrDiv =
            parser {
                let! op = mulOp |> Parser.backtrackable |> Parser.alt divOp |> Parser.backtrackable
                do! P.whiteSpaces ()
                let! rhs = expr2Parser
                do! P.whiteSpaces ()
                return (op, rhs)
            }
            |> Parser.many

        return
            mulOrDiv
            |> List.fold
                (fun lhs (op, rhs) ->
                    match op with
                    | MulOp -> Mul { Lhs = lhs; Rhs = rhs }
                    | DivOp -> Div { Lhs = lhs; Rhs = rhs })
                expr2
    }
    |> Parser.memorize

type AddOrSubOp =
    | AddOp
    | SubOp

let private addOp: Parser<Memorized, unit, AddOrSubOp> =
    P.pchar '+' |> Parser.map (fun _ -> AddOp) |> Parser.memorize

let private subOp: Parser<Memorized, unit, AddOrSubOp> =
    P.pchar '-' |> Parser.map (fun _ -> SubOp) |> Parser.memorize

exprParserRef.Value <-
    parser {
        let! expr3 = expr3Parser
        do! P.whiteSpaces ()

        let! addOrSub =
            parser {
                let! op = addOp |> Parser.backtrackable |> Parser.alt subOp |> Parser.backtrackable
                do! P.whiteSpaces ()
                let! rhs = expr3Parser
                return (op, rhs)
            }
            |> Parser.many

        return
            addOrSub
            |> List.fold
                (fun lhs (op, rhs) ->
                    match op with
                    | AddOp -> Add { Lhs = lhs; Rhs = rhs }
                    | SubOp -> Sub { Lhs = lhs; Rhs = rhs })
                expr3
    }
    |> Parser.memorize

let typeParser: Parser<Memorized, unit, TypeAnnotation> =
    P.pstring "char"
    |> Parser.map (fun (_, pos) -> CharKeywordType pos)
    |> Parser.backtrackable
    |> Parser.alt (P.pstring "number" |> Parser.map (fun (_, pos) -> NumberKeywordType pos))
    |> Parser.backtrackable
    |> Parser.alt (P.pstring "string" |> Parser.map (fun (_, pos) -> StringKeywordType pos))
    |> Parser.memorize

let private letStmtParser =
    parser {
        let! (_, pos) = P.pstring "let" |> Parser.backtrackable
        do! P.whiteSpaces1 ()
        let! ident = identParser
        do! P.whiteSpaces ()
        let! _ = P.pchar ':'
        do! P.whiteSpaces ()
        let! ty = typeParser
        do! P.whiteSpaces ()
        let! _ = P.pchar '='
        do! P.whiteSpaces ()
        let! init = exprParser

        return
            { Identifier = ident
              Type = ty
              Initializer = init
              SourcePos = pos }
    }
    |> Parser.memorize

let private topLevelLetStmtParser =
    parser {
        let! letStmt = letStmtParser
        do! P.whiteSpaces ()
        let! _ = P.pchar ';'
        return letStmt
    }
    |> Parser.memorize

let programParser =
    parser {
        let! stmts =
            parser {
                do! P.whiteSpaces () |> Parser.backtrackable
                let! stmt = topLevelLetStmtParser
                do! P.whiteSpaces ()
                return stmt
            }
            |> Parser.many

        do! P.endOfFile ()
        return stmts
    }
