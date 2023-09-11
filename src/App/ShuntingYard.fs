/// 操車場アルゴリズムに関するサンプル
module Mylang.ShuntingYard

open System
open System.Collections.Generic
open Parsec

module P = BasicParser

open Ast

type Associativity =
    | LeftAssociative
    | RightAssociative

type Op =
    { Associativity: Associativity
      Name: string
      Priority: int }

type Token =
    | Value of IntLiteral
    | Op of Op

let infixNotationToRpn (rpnTokens: list<Token>) : list<Token> =
    let opStack = Stack<Op>()
    let outputQueue = Queue<Token>()

    let mutable tokens = rpnTokens
    let mutable shouldContinue = true

    while shouldContinue do
        match tokens with
        | [] ->
            while opStack.Count <> 0 do
                outputQueue.Enqueue(Op(opStack.Pop()))

            shouldContinue <- false
        | Value value :: tokens' ->
            outputQueue.Enqueue(Value value)
            tokens <- tokens'
        | Op o1 :: tokens' ->
            let mutable shouldContinuePoppingOp = true

            while shouldContinuePoppingOp do
                if opStack.Count = 0 then
                    shouldContinuePoppingOp <- false
                else
                    let o2 = opStack.Peek()

                    if
                        o1.Priority > o2.Priority
                        || (o1.Associativity = RightAssociative && o1.Priority >= o2.Priority)
                    then
                        shouldContinuePoppingOp <- false
                    else
                        opStack.Pop() |> ignore
                        outputQueue.Enqueue(Op o2)

            opStack.Push o1
            tokens <- tokens'

    outputQueue.ToArray() |> Array.toList

let showTokenList (tokens: list<Token>) : string =
    tokens
    |> List.map (fun token ->
        match token with
        | Value value -> value.Raw
        | Op op -> op.Name)
    |> String.concat " "

let example () =
    let plus =
        { Associativity = LeftAssociative
          Name = "+"
          Priority = 1 }

    let times =
        { Associativity = LeftAssociative
          Name = "*"
          Priority = 2 }

    let pow =
        { Associativity = RightAssociative
          Name = "**"
          Priority = 3 }

    let intLit (n: int) =
        ({ SourcePos = SourcePos.origin
           Raw = string n }
        : IntLiteral)

    // 1 + 2 * 3 ** 4 ** 5 * 6 + 7
    let infixNotationTokens =
        [ Value(intLit 1)
          Op plus
          Value(intLit 2)
          Op times
          Value(intLit 3)
          Op pow
          Value(intLit 4)
          Op pow
          Value(intLit 5)
          Op times
          Value(intLit 6)
          Op plus
          Value(intLit 7) ]

    infixNotationTokens |> showTokenList |> printfn "infix notation: %s"

    infixNotationToRpn infixNotationTokens |> showTokenList |> printfn "RPN: %s"

(*
type ExprParseFailure =
    | FewOperandsSpecified of
        {| SourcePos: SourcePos
           ActualNumOperands: int
           ExpectedNumOperands: int |}
    | TooManyOperandsSpecified of SourcePos

type IOperator =
    abstract member Priority: int
    abstract member Associativity: Associativity
    abstract member ToAst: list<Expression> -> Result<Expression * list<Expression>, ExprParseFailure>

and Token =
    | Value of IntLiteral
    | Op of IOperator

    override this.ToString() =
        match this with
        | Value intLiteral -> string intLiteral
        | Op op -> string op

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
    { SourcePos: SourcePos }

    member _.Display = "+"

    interface IOperator with
        member _.Priority = 1
        member _.Associativity = LeftAssociative

        member this.ToAst(stack) =
            match stack with
            | [] ->
                FewOperandsSpecified
                    {| SourcePos = this.SourcePos
                       ActualNumOperands = 0
                       ExpectedNumOperands = 2 |}
                |> Error
            | [ _ ] ->
                FewOperandsSpecified
                    {| SourcePos = this.SourcePos
                       ActualNumOperands = 1
                       ExpectedNumOperands = 2 |}
                |> Error
            | rhs :: lhs :: rest -> Ok(Add(lhs, rhs), rest)

let private plusOperatorParser: Parser<Memorized, unit, PlusOperator> =
    parser {
        let! _, pos = P.pchar '+'
        return { SourcePos = pos }
    }
    |> Parser.memorize

[<StructuredFormatDisplay("{Display}")>]
type TimesOperator =
    { SourcePos: SourcePos }

    member _.Display = "*"

    interface IOperator with
        member _.Priority = 2
        member _.Associativity = LeftAssociative

        member this.ToAst(stack) =
            match stack with
            | [] ->
                FewOperandsSpecified
                    {| SourcePos = this.SourcePos
                       ActualNumOperands = 0
                       ExpectedNumOperands = 2 |}
                |> Error
            | [ _ ] ->
                FewOperandsSpecified
                    {| SourcePos = this.SourcePos
                       ActualNumOperands = 1
                       ExpectedNumOperands = 2 |}
                |> Error
            | rhs :: lhs :: rest -> Ok(Mul(lhs, rhs), rest)

let private timesOperatorParser: Parser<Memorized, unit, TimesOperator> =
    parser {
        let! _, pos = P.pchar '*'
        return { SourcePos = pos }
    }
    |> Parser.memorize

[<StructuredFormatDisplay("**")>]
type PowOperator =
    { SourcePos: SourcePos }

    interface IOperator with
        member _.Priority = 3
        member _.Associativity = RightAssociative

        member this.ToAst(stack) =
            match stack with
            | [] ->
                FewOperandsSpecified
                    {| SourcePos = this.SourcePos
                       ActualNumOperands = 0
                       ExpectedNumOperands = 2 |}
                |> Error
            | [ _ ] ->
                FewOperandsSpecified
                    {| SourcePos = this.SourcePos
                       ActualNumOperands = 1
                       ExpectedNumOperands = 2 |}
                |> Error
            | rhs :: lhs :: rest -> Ok(Pow(lhs, rhs), rest)

let private powOperatorParser: Parser<Memorized, unit, PowOperator> =
    parser {
        let! _, pos = P.pstring "**"
        return { SourcePos = pos }
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

let private tokenParser: Parser<Memorized, unit, Token> =
    intLiteralParser
    |> Parser.map Value
    |> Parser.backtrackable
    |> Parser.alt (binaryOperatorParser |> Parser.map Op)
    |> Parser.memorize

let inline private separateWhile ([<InlineIfLambda>] predicate: 'T -> bool) (list: list<'T>) : list<'T> * list<'T> =
    list
    |> List.tryFindIndex (predicate >> not)
    |> Option.map (fun index -> list |> List.splitAt index)
    |> Option.defaultWith (fun () -> (list, []))

let inline private infixNotationToRpn (infixNotationTokens: Token * list<Token>) : Token * list<Token> =
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

    match inner (fst infixNotationTokens :: snd infixNotationTokens) [] [] with
    | [] -> failwith "unreachable"
    | lastToken :: formerTokens -> (List.last formerTokens, List.tail (List.rev formerTokens) @ [ lastToken ])

let rec rpnToExpression (tokens: Token * list<Token>) (stack: list<Expression>) : Result<Expression, ExprParseFailure> =
    match tokens with
    | Value value, [] ->
        match stack with
        | [] -> Ok(IntLiteral value)
        | top :: _ -> Error(TooManyOperandsSpecified top.SourcePos)
    | Op op, [] ->
        match op.ToAst stack with
        | Ok(expr, []) -> Ok expr
        | Ok(_, expr :: _) -> Error(TooManyOperandsSpecified expr.SourcePos)
        | Error error -> Error error
    | Value value, token :: rest -> rpnToExpression (token, rest) (IntLiteral value :: stack)
    | Op op, token :: rest ->
        match op.ToAst stack with
        | Ok(top, exprs) -> rpnToExpression (token, rest) (top :: exprs)
        | Error error -> Error error

let private exprParser: Parser<Memorized, unit, Expression> =
    parser {
        do! P.whiteSpaces ()
        let! token = tokenParser
        do! P.whiteSpaces ()
        return token
    }
    |> Parser.backtrackable
    |> Parser.some
    |> Parser.bind (fun tokens ->
        let rpn = infixNotationToRpn tokens

        match rpnToExpression rpn [] with
        | Ok expr -> Parser.succeed expr
        | Error(FewOperandsSpecified payload) ->
            Parser.hardFailure
                { SourcePos = payload.SourcePos
                  Actual = $"%i{payload.ActualNumOperands} operands"
                  Expected = $"%i{payload.ExpectedNumOperands} operands" }
        | Error(TooManyOperandsSpecified sourcePos) ->
            Parser.hardFailure
                { SourcePos = sourcePos
                  Actual = "Too many operands specified"
                  Expected = "There are no surplus operands" })
    |> Parser.memorize

let example () =
    let source = "1 + 2 * 3 ** 4 ** 5 * 6 + 7"
    printfn "source: %s" source

    use context = new ParserContext(source)

    match context.Run(exprParser, initialState = ()) with
    | Success success -> success.ParsedValue |> printfn "result:\n%A"
    | SoftFailure failure
    | HardFailure failure -> eprintfn "%A" failure
*)
