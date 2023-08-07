module Token

open SourceFilePos

/// `let`
type LetKeyword = {
    SourceFilePos: SourceFilePos
}

type Identifier = {
    SourceFilePos: SourceFilePos
    Raw: string
}

/// `=`
type Equal = {
    SourceFilePos: SourceFilePos
}

type I32Literal = {
    SourceFilePos: SourceFilePos
    Value: int
}
