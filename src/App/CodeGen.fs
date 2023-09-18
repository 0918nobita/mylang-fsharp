module Mylang.CodeGen

let rec private codeGenExpression (ast: Ast.Expression) : TSTree.Expression =
    match ast with
    | Ast.Identifier identifier -> TSTree.Identifier { Text = identifier.Raw }
    | Ast.IntLiteral intLiteral -> TSTree.NumericLiteral { Text = intLiteral.Raw }
    | Ast.CharLiteral charLiteral -> TSTree.StringLiteral { Text = string charLiteral.Raw }
    | Ast.StringLiteral stringLiteral -> TSTree.StringLiteral { Text = stringLiteral.Raw }
    | Ast.Lambda lambda ->
        TSTree.ArrowFunctionExpression
            { Params = lambda.Params |> List.map (fun (ident, _ty) -> { Text = ident.Raw })
              Body = codeGenExpression lambda.Body }
    | Ast.Funcall funcall ->
        TSTree.CallExpression
            { Callee = codeGenExpression funcall.Callee
              Arguments = funcall.Arguments |> Array.map codeGenExpression }
    | Ast.Mul mul ->
        TSTree.Mul
            { Lhs = codeGenExpression mul.Lhs
              Rhs = codeGenExpression mul.Rhs }
    | Ast.Div div ->
        TSTree.Div
            { Lhs = codeGenExpression div.Lhs
              Rhs = codeGenExpression div.Rhs }
    | Ast.Add add ->
        TSTree.Add
            { Lhs = codeGenExpression add.Lhs
              Rhs = codeGenExpression add.Rhs }
    | Ast.Sub sub ->
        TSTree.Sub
            { Lhs = codeGenExpression sub.Lhs
              Rhs = codeGenExpression sub.Rhs }

let codegen (ast: list<Ast.LetStmt>) : TSTree.Program =
    let stmts =
        ast
        |> List.map (fun stmt ->
            TSTree.VariableDeclarationList
                { LetOrConst = TSTree.Const
                  Declarations =
                    [| { Identifier = { Text = stmt.Identifier.Raw }
                         Type = None
                         Initializer = codeGenExpression stmt.Initializer } |] })

    { Body = stmts }
