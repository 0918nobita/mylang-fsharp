module Ast

open Token

type Expression =
    // | Identifier of Identifier
    | I32Literal of I32Literal

type VariableDeclaration =
    { LetKeyword: LetKeyword
      Identifier: Identifier
      Equal: Equal
      Expression: Expression }

type Statement = VariableDeclaration of VariableDeclaration
