module Mylang.CodeGen

let rec private codeGenExpression (ast: Ast.Expression) : TSTree.Expression =
    match ast with
    | Ast.Identifier identifier -> TSTree.Identifier { Text = identifier.Raw }
    | Ast.IntLiteral intLiteral -> TSTree.NumericLiteral { Text = intLiteral.Raw }
    | Ast.CharLiteral charLiteral -> TSTree.StringLiteral { Text = string charLiteral.Raw }
    | Ast.StringLiteral stringLiteral -> TSTree.StringLiteral { Text = stringLiteral.Raw }
    | Ast.Add _ -> failwith "not implemented"
    | Ast.Mul _ -> failwith "not implemented"
    | Ast.Pow _ -> failwith "not implemented"
    | Ast.Funcall funcall ->
        TSTree.CallExpression
            { Callee = codeGenExpression funcall.Callee
              Arguments = funcall.Arguments |> Array.map codeGenExpression }

let codegen (ast: Ast.LetStmt[]) : TSTree.Program =
    let stmts =
        ast
        |> Array.map (fun stmt ->
            TSTree.VariableDeclarationList
                { LetOrConst = TSTree.Const
                  Declarations =
                    [| { Identifier = { Text = stmt.Identifier.Raw }
                         Type = None
                         Initializer = codeGenExpression stmt.Initializer } |] })

    { Body = stmts }
