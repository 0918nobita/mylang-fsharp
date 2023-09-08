/// パーサに対する入力上の位置を表すデータ
[<AutoOpen>]
module Parsec.SourcePos

open System.Runtime.CompilerServices

[<IsReadOnly; Struct; StructuredFormatDisplay("[Ln {OneBasedLine}, Col {Column}]")>]
type SourcePos =
    { Line: int
      Column: int }

    member inline this.OneBasedLine = this.Line + 1

module SourcePos =
    let origin: SourcePos = { Line = 0; Column = 0 }

    let inline nextColumn (sourcePos: SourcePos) : SourcePos =
        { sourcePos with
            Column = sourcePos.Column + 1 }

    let inline nextLine (sourcePos: SourcePos) : SourcePos =
        { sourcePos with
            Line = sourcePos.Line + 1
            Column = 0 }
