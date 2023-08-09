module SourceFileStream

open SourceFile

type SourceFileStream(sourceFile: SourceFile) =
    let mutable currentIndex = 0

    member _.Next() =
        currentIndex <- currentIndex + 1
        SourceFile.tryGetChar (SourceFile.position currentIndex sourceFile) sourceFile

    member _.Peek() =
        SourceFile.tryGetChar (SourceFile.position (currentIndex + 1) sourceFile) sourceFile

    member _.Seek(position: int) = currentIndex <- position

    member _.Position = SourceFile.position currentIndex sourceFile

    member _.Index = currentIndex
