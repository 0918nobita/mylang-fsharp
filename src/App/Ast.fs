module Mylang.Ast

open Parsec

type Identifier = { SourcePos: SourcePos; Raw: string }

type IntLiteral = { SourcePos: SourcePos; Raw: string }

type CharLiteral = { SourcePos: SourcePos; Raw: char }

type StringLiteral = { SourcePos: SourcePos; Raw: string }

type Expression =
    | Identifier of Identifier
    | IntLiteral of IntLiteral
    | CharLiteral of CharLiteral
    | StringLiteral of StringLiteral
    | Funcall of Funcall
    | Mul of Mul
    | Div of Div
    | Add of Add
    | Sub of Sub

    member this.SourcePos =
        match this with
        | Identifier ident -> ident.SourcePos
        | IntLiteral intLit -> intLit.SourcePos
        | CharLiteral charLit -> charLit.SourcePos
        | StringLiteral strLit -> strLit.SourcePos
        | Funcall funcall -> funcall.SourcePos
        | Mul mul -> mul.SourcePos
        | Div div -> div.SourcePos
        | Add add -> add.SourcePos
        | Sub sub -> sub.SourcePos

and Funcall =
    { Callee: Expression
      Arguments: Expression[] }

    member this.SourcePos = this.Callee.SourcePos

and Mul =
    { Lhs: Expression
      Rhs: Expression }

    member this.SourcePos = this.Lhs.SourcePos

and Div =
    { Lhs: Expression
      Rhs: Expression }

    member this.SourcePos = this.Lhs.SourcePos

and Add =
    { Lhs: Expression
      Rhs: Expression }

    member this.SourcePos = this.Lhs.SourcePos

and Sub =
    { Lhs: Expression
      Rhs: Expression }

    member this.SourcePos = this.Lhs.SourcePos

type TypeAnnotation =
    | CharKeywordType of SourcePos
    | NumberKeywordType of SourcePos
    | StringKeywordType of SourcePos

type LetStmt =
    { Identifier: Identifier
      Type: TypeAnnotation
      Initializer: Expression
      SourcePos: SourcePos }
