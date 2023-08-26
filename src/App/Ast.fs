module Mylang.Ast

open Parsec

type Identifier = { SourcePos: SourcePos; Raw: string }

type IntLiteral = { SourcePos: SourcePos; Raw: string }

type Expression =
    | Identifier of Identifier
    | IntLiteral of IntLiteral
    | Funcall of Funcall

and Funcall =
    { Callee: Expression
      Arguments: Expression[] }
