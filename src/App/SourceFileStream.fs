module SourceFileStream

open SourcePos

type ISourceFileStream =
    abstract member Next: unit -> Option<char>
    abstract member Peek: unit -> Option<char>
    abstract member Seek: int -> unit
    abstract member Index: int
    abstract member Position: SourcePos
