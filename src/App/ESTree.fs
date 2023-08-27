module Mylang.ESTree

type Identifier = { Raw: string }

type Literal =
    | NullLiteral
    | BooleanLiteral of bool
    | NumericLiteral of string
    | StringLiteral of string

type Expression =
    | This
    | IdentifierReference of Identifier
    | Literal of Literal

type ExpressionStatement = { Expression: Expression }

type LetOrConst =
    | Let
    | Const

type LexicalBinding =
    { BindingIdentifier: Identifier
      Initializer: Expression }

type LexicalDeclaration =
    { LetOrConst: LetOrConst
      BindingList: LexicalBinding[] }

type Declaration = LexicalDeclaration of LexicalDeclaration

type Statement =
    | ExpressionStatement of ExpressionStatement
    | Declaration of Declaration

type Program = { Body: Statement[] }
