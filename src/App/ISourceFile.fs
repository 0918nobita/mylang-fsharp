module ISourceFile

open SourcePos

type ISourceFile =
    abstract member TryGetChar: SourcePos -> Option<char>
    abstract member Position: int -> SourcePos
