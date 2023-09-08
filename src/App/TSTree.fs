module Mylang.TSTree

type CallExpression =
    { Callee: Expression
      Arguments: Expression[] }

and Identifier = { Text: string }

and NumericLiteral = { Text: string }

and StringLiteral = { Text: string }

and Expression =
    | CallExpression of CallExpression
    | Identifier of Identifier
    | NumericLiteral of NumericLiteral
    | StringLiteral of StringLiteral

type TypeNode =
    | BooleanKeywordType
    | NumberKeywordType
    | StringKeywordType

type VariableDeclaration =
    { Identifier: Identifier
      Type: Option<TypeNode>
      Initializer: Expression }

type LetOrConst =
    | Let
    | Const

type VariableDeclarationList =
    { LetOrConst: LetOrConst
      Declarations: VariableDeclaration[] }

type Statement = VariableDeclarationList of VariableDeclarationList

type Program = { Body: Statement[] }
