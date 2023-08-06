module Program

type Location = {
    Line: int
    Column: int
}

module Location =
    let nextColumn (location: Location) : Location =
        {
            Line = location.Line
            Column = location.Column + 1
        }

type LetKeyword = {
    Location: Location
}

type Identifier = {
    Location: Location
    Raw: string
}

type Equal = {
    Location: Location
}

type I32Literal = {
    Location: Location
    Value: int
}

type Expression =
    // | Identifier of Identifier
    | I32Literal of I32Literal

type VariableDeclaration = {
    LetKeyword: LetKeyword
    Identifier: Identifier
    Equal: Equal
    Expression: Expression
}

type Statement =
    | VariableDeclaration of VariableDeclaration

type ParserInput = {
    Location: Location
    Target: string
}

type ParseSuccess<'T> = {
    Location: Location
    ParsedValue: 'T
    Rest: string
}

type ParseError<'E> = {
    Location: Location
    Message: string
    Rest: string
}

type ParseResult<'T, 'E> = Result<ParseSuccess<'T>, ParseError<'E>>

type Parser<'T, 'E> = ParserInput -> ParseResult<'T, 'E>

let compileExpression (expression: Expression) =
    match expression with
    | I32Literal { Value = value } ->
        string value

let compileStatement (statement: Statement) =
    match statement with
    | VariableDeclaration { Identifier = identifier; Expression = expression } ->
        $"const %s{identifier.Raw} = %s{compileExpression(expression)};\n"

let location: Location = {
    Line = 0
    Column = 0
}

let aParser: Parser<char, string> =
    fun { Location = location; Target = target } ->
        match Seq.tryHead target with
        | Some 'a' ->
            Ok {
                Location = Location.nextColumn location
                ParsedValue = 'a'
                Rest = target[1..]
            }
        | Some c ->
            Error {
                Location = Location.nextColumn location
                Message = $"char 'a' expected, but '%c{c}' found"
                Rest = target[1..]
            }
        | None ->
            Error {
                Location = location
                Message = "char 'a' expected, but reached end of input"
                Rest = target
            }

aParser { Location = location; Target = "def" } |> printfn "%A"

let statement = VariableDeclaration {
    LetKeyword = { Location = location }
    Identifier = { Location = location; Raw = "foo" }
    Equal = { Location = location }
    Expression = I32Literal { Location = location; Value = 42 }
}

printfn "%s" (compileStatement statement)
