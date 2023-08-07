module SourcePos

type SourcePos<'S> = {
    Source: 'S
    Line: int
    Column: int
}

module SourcePos =
    let origin (source: 'S) : SourcePos<'S> = {
        Source = source
        Line = 0
        Column = 0
    }

    let nextColumn (sourcePos: SourcePos<'S>) : SourcePos<'S> =
        { sourcePos with Column = sourcePos.Column + 1 }

    let nextLine (sourcePos: SourcePos<'S>) : SourcePos<'S> =
        { sourcePos with Line = sourcePos.Line + 1; Column = 0 }

    let carriageReturn (sourcePos: SourcePos<'S>) : SourcePos<'S> =
        { sourcePos with Column = 0 }
    
    let lineFeed (sourcePos: SourcePos<'S>) : SourcePos<'S> =
        { sourcePos with Line = sourcePos.Line + 1 }
