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

and Funcall =
    { Callee: Expression
      Arguments: Expression[] }

and Mul = { Lhs: Expression; Rhs: Expression }

and Div = { Lhs: Expression; Rhs: Expression }

and Add = { Lhs: Expression; Rhs: Expression }

and Sub = { Lhs: Expression; Rhs: Expression }

type LetStmt =
    { Identifier: Identifier
      Initializer: Expression
      SourcePos: SourcePos }
