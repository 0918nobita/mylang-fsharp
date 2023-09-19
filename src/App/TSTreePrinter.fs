module Mylang.TSTreePrinter

open TSTree

let inline private printIdentifier (ast: TSTree.Identifier) = ast.Text

let inline private printTypeNode (ast: TSTree.TypeNode) =
    match ast with
    | BooleanKeywordType -> "boolean"
    | NumberKeywordType -> "number"
    | StringKeywordType -> "string"

type Priority =
    | Ident = 20
    | NumLit = 20
    | StrLit = 20
    | ArrowFuncExpr = 20
    | Funcall = 18
    | Mul = 13
    | Div = 13
    | Add = 12
    | Sub = 12

let inline private insertParen (outerPriority: Priority) ((str, innerPriority): string * Priority) =
    if outerPriority > innerPriority then $"(%s{str})" else str

let rec private printExpression (ast: TSTree.Expression) : string * Priority =
    match ast with
    | ArrowFunctionExpression arrowFunctionExpression ->
        let paramDecls =
            arrowFunctionExpression.Params |> List.map printIdentifier |> String.concat ", "

        let (body, _) = printExpression arrowFunctionExpression.Body

        $"((%s{paramDecls}) => %s{body})", Priority.ArrowFuncExpr
    | CallExpression callExpression ->
        let callee = printExpression callExpression.Callee |> insertParen Priority.Funcall

        let arguments =
            callExpression.Arguments
            |> Array.map (printExpression >> fst)
            |> String.concat ", "

        $"%s{callee}(%s{arguments})", Priority.Funcall
    | Identifier identifier -> printIdentifier identifier, Priority.Ident
    | NumericLiteral numericLiteral -> numericLiteral.Text, Priority.NumLit
    | StringLiteral stringLiteral -> $"\"%s{stringLiteral.Text}\"", Priority.StrLit
    | Mul mul ->
        let lhs = printExpression mul.Lhs |> insertParen Priority.Mul
        let rhs = printExpression mul.Rhs |> insertParen Priority.Mul
        $"%s{lhs} * %s{rhs}", Priority.Mul
    | Div div ->
        let lhs = printExpression div.Lhs |> insertParen Priority.Div
        let rhs = printExpression div.Rhs |> insertParen Priority.Div
        $"%s{lhs} / %s{rhs}", Priority.Div
    | Add add ->
        let lhs = printExpression add.Lhs |> insertParen Priority.Add
        let rhs = printExpression add.Rhs |> insertParen Priority.Add
        $"%s{lhs} + %s{rhs}", Priority.Add
    | Sub sub ->
        let lhs = printExpression sub.Lhs |> insertParen Priority.Sub
        let rhs = printExpression sub.Rhs |> insertParen Priority.Sub
        $"%s{lhs} - %s{rhs}", Priority.Sub

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

                let (initializer, _) = printExpression variableDeclaration.Initializer
                $"%s{ident}%s{typeNode} = %s{initializer}")
            |> String.concat ", "

        $"%s{letOrConst} %s{declarations};"

let print (ast: TSTree.Program) =
    ast.Body |> List.map printStmt |> String.concat "\n"
