/// パーサに対する入力
[<AutoOpen>]
module Parsec.SourceCharStream

type ISourceCharStream =
    inherit System.IDisposable
    abstract member CurrentIndex: int with get, set
    abstract member CurrentPos: SourcePos
    abstract member ReadNextChar: unit -> SourcePos * Option<char>
    abstract member SeekFromBegin: int -> unit
    abstract member SeekFromCurrentPos: int -> unit
