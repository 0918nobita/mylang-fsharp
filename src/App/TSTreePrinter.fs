module Mylang.TSTreePrinter

open TSTree

let inline private printIdentifier (ast: TSTree.Identifier) = ast.Text

let inline private printTypeNode (ast: TSTree.TypeNode) =
    match ast with
    | BooleanKeywordType -> "boolean"
    | NumberKeywordType -> "number"
    | StringKeywordType -> "string"

let rec private printExpression (ast: TSTree.Expression) =
    match ast with
    | CallExpression callExpression ->
        let callee = printExpression callExpression.Callee

        let arguments =
            callExpression.Arguments |> Array.map printExpression |> String.concat ", "

        $"%s{callee}(%s{arguments})"
    | Identifier identifier -> printIdentifier identifier
    | NumericLiteral numericLiteral -> numericLiteral.Text
    | StringLiteral stringLiteral -> $"\"%s{stringLiteral.Text}\""

let inline private printStmt (ast: TSTree.Statement) =
    match ast with
    | VariableDeclarationList variableDeclarationList ->
        let letOrConst =
            match variableDeclarationList.LetOrConst with
            | Let -> "let"
            | Const -> "const"

        let declarations =
            variableDeclarationList.Declarations
            |> Array.map (fun variableDeclaration ->
                let ident = printIdentifier variableDeclaration.Identifier

                let typeNode =
                    variableDeclaration.Type
                    |> Option.map (fun ty -> $": %s{printTypeNode ty}")
                    |> Option.defaultValue ""

                let initializer = printExpression variableDeclaration.Initializer
                $"%s{ident}%s{typeNode} = %s{initializer}")
            |> String.concat ", "

        $"%s{letOrConst} %s{declarations};"

let print (ast: TSTree.Program) =
    ast.Body |> Array.map printStmt |> String.concat "\n"
