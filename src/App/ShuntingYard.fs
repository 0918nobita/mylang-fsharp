/// 操車場アルゴリズムに関するサンプル
module Mylang.ShuntingYard

open System
open Parsec

module P = BasicParser

open Ast

type Associativity =
    | LeftAssociative
    | RightAssociative

type IOperator =
    abstract member Priority: int
    abstract member Associativity: Associativity

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

[<StructuredFormatDisplay("{Display}")>]
type PlusOperator =
    | PlusOperator of SourcePos

    member _.Display = "+"

    interface IOperator with
        member _.Priority = 1
        member _.Associativity = LeftAssociative

let private plusOperatorParser: Parser<Memorized, unit, PlusOperator> =
    parser {
        let! _, pos = P.pchar '+'
        return PlusOperator pos
    }
    |> Parser.memorize

[<StructuredFormatDisplay("{Display}")>]
type TimesOperator =
    | TimesOperator of SourcePos

    member _.Display = "*"

    interface IOperator with
        member _.Priority = 2
        member _.Associativity = LeftAssociative

let private timesOperatorParser: Parser<Memorized, unit, TimesOperator> =
    parser {
        let! _, pos = P.pchar '*'
        return TimesOperator pos
    }
    |> Parser.memorize

[<StructuredFormatDisplay("**")>]
type PowOperator =
    | PowOperator of SourcePos

    interface IOperator with
        member _.Priority = 3
        member _.Associativity = RightAssociative

let private powOperatorParser: Parser<Memorized, unit, PowOperator> =
    parser {
        let! _, pos = P.pstring "**"
        return PowOperator pos
    }
    |> Parser.memorize

let private binaryOperatorParser: Parser<Memorized, unit, IOperator> =
    plusOperatorParser
    |> Parser.map (fun plusOperator -> plusOperator :> IOperator)
    |> Parser.backtrackable
    |> Parser.alt (powOperatorParser |> Parser.map (fun powOperator -> powOperator :> IOperator))
    |> Parser.backtrackable
    |> Parser.alt (
        timesOperatorParser
        |> Parser.map (fun timesOperator -> timesOperator :> IOperator)
    )
    |> Parser.memorize

type Token =
    | Value of IntLiteral
    | Op of IOperator

    override this.ToString() =
        match this with
        | Value intLiteral -> string intLiteral
        | Op op -> string op

let private tokenParser: Parser<Memorized, unit, Token> =
    intLiteralParser
    |> Parser.map Value
    |> Parser.backtrackable
    |> Parser.alt (binaryOperatorParser |> Parser.map Op)
    |> Parser.memorize

let private exprParser: Parser<Memorized, unit, list<Token>> =
    parser {
        do! P.whiteSpaces ()
        let! token = tokenParser
        do! P.whiteSpaces ()
        return token
    }
    |> Parser.backtrackable
    |> Parser.many
    |> Parser.memorize

let inline separateWhile (predicate: 'T -> bool) (list: list<'T>) : list<'T> * list<'T> =
    list
    |> List.tryFindIndex (predicate >> not)
    |> Option.map (fun index -> list |> List.splitAt index)
    |> Option.defaultWith (fun () -> (list, []))

let inline infixNotationToRPN (tokens: list<Token>) : list<Token> =
    let rec inner (tokens: list<Token>) (opStack: list<IOperator>) (outputQueue: list<Token>) : list<Token> =
        match tokens with
        | [] ->
            match opStack with
            | [] -> outputQueue
            | top :: opStack' -> inner [] opStack' (Op top :: outputQueue)
        | (Value intValue) :: tail -> inner tail opStack (Value intValue :: outputQueue)
        | (Op op) :: tail ->
            match opStack with
            | top :: _ ->
                if
                    op.Priority > top.Priority
                    || (op.Associativity = RightAssociative && op.Priority >= top.Priority)
                then
                    inner tail (op :: opStack) outputQueue
                else
                    let (front, back) =
                        opStack |> separateWhile (fun op' -> op'.Priority >= op.Priority)

                    let opStack' = op :: back
                    let outputQueue' = (List.rev front |> List.map Op) @ outputQueue

                    inner tail opStack' outputQueue'
            | [] -> inner tail (op :: opStack) outputQueue

    inner tokens [] [] |> List.rev

let inline showTokenList (tokens: list<Token>) =
    tokens |> List.map string |> String.concat " "

let example () =
    use context = new ParserContext "1 + 2 * 3 ** 4 ** 5 * 6 + 7"

    match context.Run(exprParser, initialState = ()) with
    | Success success ->
        success.ParsedValue |> showTokenList |> printfn "infix notation: %s"

        success.ParsedValue |> infixNotationToRPN |> showTokenList |> printfn "RPN: %s"
    | SoftFailure failure
    | HardFailure failure -> eprintfn "%A" failure
