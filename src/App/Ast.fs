module Mylang.Ast

open Parsec

type Identifier = { SourcePos: SourcePos; Raw: string }

[<StructuredFormatDisplay("{Raw}")>]
type IntLiteral = { SourcePos: SourcePos; Raw: string }

type CharLiteral = { SourcePos: SourcePos; Raw: char }

type StringLiteral = { SourcePos: SourcePos; Raw: string }

type Expression =
    | Identifier of Identifier
    | IntLiteral of IntLiteral
    | CharLiteral of CharLiteral
    | StringLiteral of StringLiteral
    | Add of lhs: Expression * rhs: Expression
    | Mul of lhs: Expression * rhs: Expression
    | Pow of lhs: Expression * rhs: Expression
    | Funcall of Funcall

    member this.SourcePos =
        match this with
        | Identifier identifier -> identifier.SourcePos
        | IntLiteral intLiteral -> intLiteral.SourcePos
        | CharLiteral charLiteral -> charLiteral.SourcePos
        | StringLiteral stringLiteral -> stringLiteral.SourcePos
        | Add(lhs, _)
        | Mul(lhs, _)
        | Pow(lhs, _) -> lhs.SourcePos
        | Funcall funcall -> funcall.SourcePos

and Funcall =
    { Callee: Expression
      Arguments: Expression[] }

    member this.SourcePos = this.Callee.SourcePos

type LetStmt =
    { Identifier: Identifier
      Initializer: Expression
      SourcePos: SourcePos }
