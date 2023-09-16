module Mylang.TSTree

type Expression =
    | CallExpression of CallExpression
    | Identifier of Identifier
    | NumericLiteral of NumericLiteral
    | StringLiteral of StringLiteral
    | Mul of Mul
    | Div of Div
    | Add of Add
    | Sub of Sub

and CallExpression =
    { Callee: Expression
      Arguments: Expression[] }

and Identifier = { Text: string }

and NumericLiteral = { Text: string }

and StringLiteral = { Text: string }

and Mul = { Lhs: Expression; Rhs: Expression }

and Div = { Lhs: Expression; Rhs: Expression }

and Add = { Lhs: Expression; Rhs: Expression }

and Sub = { Lhs: Expression; Rhs: Expression }

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
