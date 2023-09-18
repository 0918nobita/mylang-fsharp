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

let rec private typeCheckExpr (typeEnv: Map<string, Type>) (expr: Expression) (expectedType: Type) : list<TypeError> =
    match expr with
    | CharLiteral charLiteral ->
        match expectedType with
        | Char -> []
        | _ ->
            [ TypeMismatch
                  {| Expected = expectedType
                     Actual = Char
                     SourcePos = charLiteral.SourcePos |} ]
    | IntLiteral intLiteral ->
        match expectedType with
        | Number -> []
        | _ ->
            [ TypeMismatch
                  {| Expected = expectedType
                     Actual = Number
                     SourcePos = intLiteral.SourcePos |} ]
    | StringLiteral stringLiteral ->
        match expectedType with
        | String -> []
        | _ ->
            [ TypeMismatch
                  {| Expected = expectedType
                     Actual = String
                     SourcePos = stringLiteral.SourcePos |} ]
    | Identifier ident ->
        if Map.containsKey ident.Raw typeEnv then
            if typeEnv.[ident.Raw] = expectedType then
                []
            else
                [ TypeMismatch
                      {| Expected = expectedType
                         Actual = typeEnv.[ident.Raw]
                         SourcePos = ident.SourcePos |} ]
        else
            [ UnresolvedName
                  {| Name = ident.Raw
                     SourcePos = ident.SourcePos |} ]
    | Funcall _ -> failwith "not implemented"
    | Mul mul ->
        let lhsErrors = typeCheckExpr typeEnv mul.Lhs Number
        let rhsErrors = typeCheckExpr typeEnv mul.Rhs Number

        if expectedType <> Number then
            TypeMismatch
                {| Expected = expectedType
                   Actual = Number
                   SourcePos = mul.SourcePos |}
            :: lhsErrors
            @ rhsErrors
        else
            lhsErrors @ rhsErrors
    | Div div ->
        let lhsErrors = typeCheckExpr typeEnv div.Lhs Number
        let rhsErrors = typeCheckExpr typeEnv div.Rhs Number

        if expectedType <> Number then
            TypeMismatch
                {| Expected = expectedType
                   Actual = Number
                   SourcePos = div.SourcePos |}
            :: lhsErrors
            @ rhsErrors
        else
            lhsErrors @ rhsErrors
    | Add add ->
        let lhsErrors = typeCheckExpr typeEnv add.Lhs Number
        let rhsErrors = typeCheckExpr typeEnv add.Rhs Number

        if expectedType <> Number then
            TypeMismatch
                {| Expected = expectedType
                   Actual = Number
                   SourcePos = add.SourcePos |}
            :: lhsErrors
            @ rhsErrors
        else
            lhsErrors @ rhsErrors
    | Sub sub ->
        let lhsErrors = typeCheckExpr typeEnv sub.Lhs Number
        let rhsErrors = typeCheckExpr typeEnv sub.Rhs Number

        if expectedType <> Number then
            TypeMismatch
                {| Expected = expectedType
                   Actual = Number
                   SourcePos = sub.SourcePos |}
            :: lhsErrors
            @ rhsErrors
        else
            lhsErrors @ rhsErrors

let private typeCheckLetStmt (typeEnv: Map<string, Type>) (letStmt: LetStmt) : Map<string, Type> * list<TypeError> =
    match letStmt.Type with
    | CharKeywordType _ ->
        let initializerErrors = typeCheckExpr typeEnv letStmt.Initializer Char
        let newTypeEnv = typeEnv |> Map.add letStmt.Identifier.Raw Char
        (newTypeEnv, initializerErrors)
    | NumberKeywordType _ ->
        let initializerErrors = typeCheckExpr typeEnv letStmt.Initializer Number
        let newTypeEnv = typeEnv |> Map.add letStmt.Identifier.Raw Number
        (newTypeEnv, initializerErrors)
    | StringKeywordType _ ->
        let initializerErrors = typeCheckExpr typeEnv letStmt.Initializer String
        let newTypeEnv = typeEnv |> Map.add letStmt.Identifier.Raw String
        (newTypeEnv, initializerErrors)

let typeCheck (letStmts: list<LetStmt>) : list<TypeError> =
    letStmts
    |> List.fold
        (fun ((typeEnv: Map<string, Type>, errors: list<TypeError>)) letStmt ->
            let (newTypeEnv, errors') = typeCheckLetStmt typeEnv letStmt
            (newTypeEnv, errors @ errors'))
        (Map.empty, [])
    |> snd
