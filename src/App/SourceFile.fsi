module SourceFile

open SourcePos
open ISourceFile

type SourceFile

module SourceFile =
    val fromString: string -> SourceFile

    /// `tryGetChar sourcePos sourceFile` は
    /// `sourceFile` の指定した位置 `sourcePos` に文字 `c` が存在する場合 `Some c` を返し、
    /// 存在しない場合 `None` を返す
    val tryGetChar: SourcePos -> SourceFile -> Option<char>

    /// `position n sourceFile` は `sourceFile` の １行目の行頭から数えて `n` 文字目に来る文字の位置を返す
    val position: int -> SourceFile -> SourcePos

type SourceFileStream =
    new: SourceFile -> SourceFileStream

    interface ISourceFile
