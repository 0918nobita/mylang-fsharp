module SourceFilePos

open SourcePos

type SourceFile =
    | Anon
    | File of string

type SourceFilePos = SourcePos<SourceFile>

module SourceFilePos =
    let origin (sourceFile: SourceFile) : SourceFilePos = SourcePos.origin sourceFile
