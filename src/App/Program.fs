module Program

type Location = {
    Line: int
    Column: int
}

module Location =
    let root = {
        Line = 0
        Column = 0
    }

    let nextColumn (location: Location) : Location =
        {
            Line = location.Line
            Column = location.Column + 1
        }
    
    let nextLine (location: Location) : Location =
        {
            Line = location.Line + 1
            Column = 0
        }

    let carriageReturn (location: Location) =
        {
            Line = location.Line
            Column = 0
        }

    let lineFeed (location: Location) =
        {
            Line = location.Line + 1
            Column = location.Column
        }

/// `let`
type LetKeyword = {
    Location: Location
}

type Identifier = {
    Location: Location
    Raw: string
}

/// `=`
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

type ParserInput<'C> = {
    Context: 'C
    Target: string
}

type ParseSuccess<'C, 'T> = {
    Context: 'C
    ParsedValue: 'T
    Rest: string
}

type ParseError<'C, 'E> = {
    Context: 'C
    Reason: 'E
    /// 飛ばして続きをパースする場合に入力となる文字列
    Rest: string
}

type ParseResult<'C, 'T, 'E> = Result<ParseSuccess<'C, 'T>, ParseError<'C, 'E>>

type Parser<'C, 'T, 'E> = ParserInput<'C> -> ParseResult<'C, 'T, 'E>

module Parser =
    let map (mapping: 'T -> 'U) (parse: Parser<'C, 'T, 'E>) : Parser<'C, 'U, 'E> =
        fun parserInput ->
            parse parserInput
            |> Result.map (fun parseSuccess ->
                {
                    Context = parseSuccess.Context
                    ParsedValue = mapping parseSuccess.ParsedValue
                    Rest = parseSuccess.Rest
                })

    let optional (parse: Parser<'C, 'T, 'E>) : Parser<'C, Option<'T>, 'E> =
        fun { Context = context; Target = target } ->
            match parse { Context = context; Target = target } with
            | Ok { Context = context'; ParsedValue = parsedValue; Rest = rest } ->
                Ok {
                    Context = context'
                    ParsedValue = Some parsedValue
                    Rest = rest
                }
            | Error _ ->
                Ok {
                    Context = context
                    ParsedValue = None
                    Rest = target
                }

type LineBreak = Crlf | Lf

let charParser (c : char) : Parser<LineBreak * Location, char, string> =
    fun { Context = (lineBreak, location); Target = target } ->
        match (lineBreak, Seq.tryHead target) with
        | (Crlf, Some '\r') ->
            Ok {
                Context = (Crlf, Location.carriageReturn location)
                ParsedValue = '\r'
                Rest = target[1..]
            }
        | (Crlf, Some '\n') ->
            Ok {
                Context = (Crlf, Location.lineFeed location)
                ParsedValue = '\n'
                Rest = target[1..]
            }
        | (Lf, Some '\n') ->
            Ok {
                Context = (Crlf, Location.nextLine location)
                ParsedValue = '\n'
                Rest = target[1..]
            }
        | (_, Some c') when c' = c ->
            Ok {
                Context = (lineBreak, Location.nextColumn location)
                ParsedValue = c
                Rest = target[1..]
            }
        | (_, Some c') ->
            Error {
                Context = (lineBreak, Location.nextColumn location)
                Reason = $"char '%c{c}' expected, but char '%c{c'}' found"
                Rest = target[1..]
            }
        | (_, None) ->
            Error {
                Context = (lineBreak, Location.nextColumn location)
                Reason = $"char '%c{c}' expected, but reached end of input"
                Rest = target[1..]
            }

let myParser =
    charParser 'd'
    |> Parser.map string
    |> Parser.optional

myParser { Context = (Lf, Location.root); Target = "def" }
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
    LetKeyword = { Location = Location.root }
    Identifier = {
        Location = Location.root
        Raw = "foo"
    }
    Equal = { Location = Location.root }
    Expression = I32Literal {
        Location = Location.root
        Value = 42
    }
}

printfn "%s" (compileStatement statement)
