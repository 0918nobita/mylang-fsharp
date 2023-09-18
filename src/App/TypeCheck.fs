module Mylang.TypeCheck

open Parsec

open Ast

type Type =
    | Char
    | Number
    | String
    | Func of
        {| Params: list<Type>
           ReturnType: Type |}

type TypeError =
    | TypeMismatch of
        {| Expected: Type
           Actual: Type
           SourcePos: SourcePos |}
    | UnresolvedName of {| Name: string; SourcePos: SourcePos |}

let rec private typeAnnotationToType (typeAnnotation: TypeAnnotation) : Type =
    match typeAnnotation with
    | CharKeywordType _ -> Char
    | NumberKeywordType _ -> Number
    | StringKeywordType _ -> String
    | FuncType funcType ->
        let paramTypes = funcType.Params |> List.map typeAnnotationToType
        let returnType = typeAnnotationToType funcType.ReturnType

        Func
            {| Params = paramTypes
               ReturnType = returnType |}

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
    | Lambda lambda ->
        let newTypeEnv =
            lambda.Params
            |> List.fold (fun state (ident, ty) -> state |> Map.add ident.Raw (typeAnnotationToType ty)) typeEnv

        let paramTypes = lambda.Params |> List.map (snd >> typeAnnotationToType)

        let returnTy = typeAnnotationToType lambda.ReturnType

        let funcTy =
            Func
                {| Params = paramTypes
                   ReturnType = returnTy |}

        let bodyErrors = typeCheckExpr newTypeEnv lambda.Body returnTy

        if expectedType <> funcTy then
            TypeMismatch
                {| Expected = expectedType
                   Actual = funcTy
                   SourcePos = lambda.SourcePos |}
            :: bodyErrors
        else
            bodyErrors
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
    let expectedType = typeAnnotationToType letStmt.Type
    let initializerErrors = typeCheckExpr typeEnv letStmt.Initializer expectedType
    let newTypeEnv = typeEnv |> Map.add letStmt.Identifier.Raw expectedType
    (newTypeEnv, initializerErrors)

let typeCheck (letStmts: list<LetStmt>) : list<TypeError> =
    letStmts
    |> List.fold
        (fun ((typeEnv: Map<string, Type>, errors: list<TypeError>)) letStmt ->
            let (newTypeEnv, errors') = typeCheckLetStmt typeEnv letStmt
            (newTypeEnv, errors @ errors'))
        (Map.empty, [])
    |> snd
