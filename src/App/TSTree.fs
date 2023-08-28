module Mylang.TSTree

type ITSTreeVisitor<'T> =
    abstract member Visit: Identifier -> 'T
    abstract member Visit: NumericLiteral -> 'T
    abstract member Visit: StringLiteral -> 'T
    abstract member Visit: Expression -> 'T
    abstract member Visit: TypeNode -> 'T
    abstract member Visit: VariableDeclarationList -> 'T
    abstract member Visit: Statement -> 'T
    abstract member Visit: Program -> 'T

and Identifier =
    { Text: string }

    member inline this.Accept(visitor: ITSTreeVisitor<_>) = visitor.Visit(this)

and NumericLiteral =
    { Text: string }

    member inline this.Accept(visitor: ITSTreeVisitor<_>) = visitor.Visit(this)

and StringLiteral =
    { Text: string }

    member inline this.Accept(visitor: ITSTreeVisitor<_>) = visitor.Visit(this)

and Expression =
    | Identifier of Identifier
    | NumericLiteral of NumericLiteral
    | StringLiteral of StringLiteral

    member inline this.Accept(visitor: ITSTreeVisitor<_>) = visitor.Visit(this)

and TypeNode =
    | BooleanKeywordType
    | NumberKeywordType
    | StringKeywordType

    member inline this.Accept(visitor: ITSTreeVisitor<_>) = visitor.Visit(this)

and VariableDeclaration =
    { Identifier: Identifier
      Type: Option<TypeNode>
      Initializer: Expression }

and LetOrConst =
    | Let
    | Const

and VariableDeclarationList =
    { LetOrConst: LetOrConst
      Declarations: VariableDeclaration[] }

    member inline this.Accept(visitor: ITSTreeVisitor<_>) = visitor.Visit(this)

and Statement =
    | VariableDeclarationList of VariableDeclarationList

    member inline this.Accept(visitor: ITSTreeVisitor<_>) = visitor.Visit(this)

and Program =
    { Body: Statement[] }

    member inline this.Accept(visitor: ITSTreeVisitor<_>) = visitor.Visit(this)
