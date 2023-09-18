module Mylang.TypeCheck

open Parsec

open Ast

type Type =
    | Char
    | Number
    | String

type TypeError =
    | TypeMismatch of
        {| Expected: Type
           Actual: Type
           SourcePos: SourcePos |}
    | UnresolvedName of {| Name: string; SourcePos: SourcePos |}

let rec private typeCheckExpr
    (typeEnv: Map<string, Type>)
    (expr: Expression)
    (expectedType: Type)
    : Result<unit, TypeError> =
    match expr with
    | CharLiteral charLiteral ->
        match expectedType with
        | Char -> Ok()
        | _ ->
            Error(
                TypeMismatch
                    {| Expected = expectedType
                       Actual = Char
                       SourcePos = charLiteral.SourcePos |}
            )
    | IntLiteral intLiteral ->
        match expectedType with
        | Number -> Ok()
        | _ ->
            Error(
                TypeMismatch
                    {| Expected = expectedType
                       Actual = Number
                       SourcePos = intLiteral.SourcePos |}
            )
    | StringLiteral stringLiteral ->
        match expectedType with
        | String -> Ok()
        | _ ->
            Error(
                TypeMismatch
                    {| Expected = expectedType
                       Actual = String
                       SourcePos = stringLiteral.SourcePos |}
            )
    | Identifier ident ->
        if Map.containsKey ident.Raw typeEnv then
            if typeEnv.[ident.Raw] = expectedType then
                Ok()
            else
                Error(
                    TypeMismatch
                        {| Expected = expectedType
                           Actual = typeEnv.[ident.Raw]
                           SourcePos = ident.SourcePos |}
                )
        else
            Error(
                UnresolvedName
                    {| Name = ident.Raw
                       SourcePos = ident.SourcePos |}
            )
    | Funcall _ -> failwith "not implemented"
    | Mul mul ->
        if expectedType <> Number then
            Error(
                TypeMismatch
                    {| Expected = expectedType
                       Actual = Number
                       SourcePos = mul.SourcePos |}
            )
        else
            typeCheckExpr typeEnv mul.Lhs Number
            |> Result.bind (fun _ -> typeCheckExpr typeEnv mul.Rhs Number)
    | Div div ->
        if expectedType <> Number then
            Error(
                TypeMismatch
                    {| Expected = expectedType
                       Actual = Number
                       SourcePos = div.SourcePos |}
            )
        else
            typeCheckExpr typeEnv div.Lhs Number
            |> Result.bind (fun _ -> typeCheckExpr typeEnv div.Rhs Number)
    | Add add ->
        if expectedType <> Number then
            Error(
                TypeMismatch
                    {| Expected = expectedType
                       Actual = Number
                       SourcePos = add.SourcePos |}
            )
        else
            typeCheckExpr typeEnv add.Lhs Number
            |> Result.bind (fun _ -> typeCheckExpr typeEnv add.Rhs Number)
    | Sub sub ->
        if expectedType <> Number then
            Error(
                TypeMismatch
                    {| Expected = expectedType
                       Actual = Number
                       SourcePos = sub.SourcePos |}
            )
        else
            typeCheckExpr typeEnv sub.Lhs Number
            |> Result.bind (fun _ -> typeCheckExpr typeEnv sub.Rhs Number)

let typeCheckLetStmt (typeEnv: Map<string, Type>) (letStmt: LetStmt) : Result<Map<string, Type>, TypeError> =
    match letStmt.Type with
    | CharKeywordType _ ->
        typeCheckExpr typeEnv letStmt.Initializer Char
        |> Result.map (fun _ -> Map.add letStmt.Identifier.Raw Char typeEnv)
    | NumberKeywordType _ ->
        typeCheckExpr typeEnv letStmt.Initializer Number
        |> Result.map (fun _ -> Map.add letStmt.Identifier.Raw Number typeEnv)
    | StringKeywordType _ ->
        typeCheckExpr typeEnv letStmt.Initializer String
        |> Result.map (fun _ -> Map.add letStmt.Identifier.Raw String typeEnv)

let typeCheck (letStmts: list<LetStmt>) : Result<unit, TypeError> =
    letStmts
    |> List.fold
        (fun (proc: Result<Map<string, Type>, TypeError>) letStmt ->
            proc |> Result.bind (fun typeEnv -> typeCheckLetStmt typeEnv letStmt))
        (Ok Map.empty)
    |> Result.map ignore
