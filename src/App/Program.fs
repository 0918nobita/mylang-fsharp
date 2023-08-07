module Program

open SourceFilePos
open ParserLib
open Ast

let origin = SourceFilePos.origin Anon

let myParser =
    charParser 'd'
    |> Parser.bind (fun c ->
        charParser 'e'
        |> Parser.map (fun c' -> $"%c{c}%c{c'}")
        |> withLocation)
    |> Parser.optional

myParser { Context = (Lf, origin); Target = "def" }
|> printfn "%A"

let myParser2 =
    parser {
        let! c = charParser 'd'
        let! (_, location) = Parser.getContext
        let! c' = charParser 'e'
        return (location, $"%c{c}%c{c'}")
    }
    |> Parser.optional

myParser2 { Context = (Lf, origin); Target = "def" }
|> printfn "%A"

let compileExpression (expression: Expression) =
    match expression with
    | I32Literal { Value = value } ->
        string value

let compileStatement (statement: Statement) =
    match statement with
    | VariableDeclaration { Identifier = identifier; Expression = expression } ->
        $"const %s{identifier.Raw} = %s{compileExpression(expression)};\n"

let statement = VariableDeclaration {
    LetKeyword = { SourceFilePos = origin }
    Identifier = {
        SourceFilePos = origin
        Raw = "foo"
    }
    Equal = { SourceFilePos = origin }
    Expression = I32Literal {
        SourceFilePos = origin
        Value = 42
    }
}

printfn "%s" (compileStatement statement)
