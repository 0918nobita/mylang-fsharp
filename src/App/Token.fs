module Token

open SourcePos

/// `let`
type LetKeyword = { SourceFilePos: SourcePos }

type Identifier =
    { SourceFilePos: SourcePos
      Raw: string }

/// `=`
type Equal = { SourceFilePos: SourcePos }

type I32Literal =
    { SourceFilePos: SourcePos; Value: int }
