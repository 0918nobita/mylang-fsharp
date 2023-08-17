module SourceFileStream

open SourcePos

type ISourceFileStream =
    abstract member Next: unit -> Option<char>
    abstract member Peek: unit -> Option<char>
    abstract member Seek: int -> unit
    abstract member Position: int -> SourcePos
    abstract member Index: int
