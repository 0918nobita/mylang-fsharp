module Mylang.TSTreePrinter

open TSTree

type private TSTreeVisitor() =
    interface ITSTreeVisitor<string> with
        member _.Visit(identifier: Identifier) = identifier.Text

        member _.Visit(numericLiteral: NumericLiteral) = numericLiteral.Text

        member _.Visit(stringLiteral: StringLiteral) = stringLiteral.Text

        member this.Visit(expression: Expression) =
            match expression with
            | Identifier identifier -> identifier.Accept(this)
            | NumericLiteral numericLiteral -> numericLiteral.Accept(this)
            | StringLiteral stringLiteral -> stringLiteral.Accept(this)

        member _.Visit(typeNode: TypeNode) =
            match typeNode with
            | BooleanKeywordType -> "boolean"
            | NumberKeywordType -> "number"
            | StringKeywordType -> "string"

        member this.Visit(variableDeclarationList: VariableDeclarationList) =
            let letOrConst =
                match variableDeclarationList.LetOrConst with
                | Let -> "let"
                | Const -> "const"

            let declarations =
                variableDeclarationList.Declarations
                |> Array.map (fun variableDeclaration ->
                    let ident = variableDeclaration.Identifier.Accept(this)

                    let typeNode =
                        variableDeclaration.Type
                        |> Option.map (fun typeNode ->
                            let typeNode = typeNode.Accept(this)
                            $": %s{typeNode}")
                        |> Option.defaultValue ""

                    let initializer = variableDeclaration.Initializer.Accept(this)
                    $"%s{ident}%s{typeNode} = %s{initializer}")
                |> String.concat ", "

            $"%s{letOrConst} %s{declarations};"

        member this.Visit(statement: Statement) =
            match statement with
            | VariableDeclarationList variableDeclarationList -> variableDeclarationList.Accept(this)

        member this.Visit(program: Program) =
            program.Body
            |> Array.map (fun statement -> statement.Accept(this))
            |> String.concat "\n"

let print (tsTree: Program) = tsTree.Accept(TSTreeVisitor())
