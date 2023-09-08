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

and Funcall =
    { Callee: Expression
      Arguments: Expression[] }

type LetStmt =
    { Identifier: Identifier
      Initializer: Expression
      SourcePos: SourcePos }
