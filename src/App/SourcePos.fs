module SourcePos

type SourcePos = { Line: int; Column: int }

module SourcePos =
    let origin: SourcePos = { Line = 0; Column = 0 }

    let inline nextColumn (sourcePos: SourcePos) : SourcePos =
        { sourcePos with
            Column = sourcePos.Column + 1 }

    let inline nextLine (sourcePos: SourcePos) : SourcePos =
        { sourcePos with
            Line = sourcePos.Line + 1
            Column = 0 }
