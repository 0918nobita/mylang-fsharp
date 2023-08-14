module SourceFile

open SourcePos
open SourceFileStream

type Line = { Content: char[]; CursorIndex: int }

type SourceFile = SourceFile of Line[]

module SourceFile =
    let fromString (source: string) : SourceFile =
        let folder ((cursorIndex, lines): int * Line list) (line: string) : int * Line list =
            (cursorIndex + String.length line,
             { CursorIndex = cursorIndex
               Content = Seq.toArray line }
             :: lines)

        let initialState = (0, [])

        source.Split([| "\r\n"; "\n" |], System.StringSplitOptions.None)
        |> Array.fold folder initialState
        |> snd
        |> List.rev
        |> List.toArray
        |> SourceFile

    let tryGetChar (sourcePos: SourcePos) (SourceFile lines) : Option<char> =
        Array.tryItem sourcePos.Line lines
        |> Option.bind (fun line -> line.Content |> Array.tryItem sourcePos.Column)

    let position (cursorIndex: int) (SourceFile lines) : SourcePos =
        lines
        |> Array.indexed
        |> Array.tryFindBack (fun (_, line) -> line.CursorIndex < cursorIndex)
        |> Option.map (fun (lineNum, line) ->
            let column = cursorIndex - line.CursorIndex - 1
            { Line = lineNum; Column = column })
        |> Option.defaultValue { Line = 0; Column = cursorIndex }

type SourceFileStream(sourceFile: SourceFile) =
    let mutable currentIndex = 0

    interface ISourceFileStream with
        member _.Next() =
            currentIndex <- currentIndex + 1
            SourceFile.tryGetChar (SourceFile.position currentIndex sourceFile) sourceFile

        member _.Peek() =
            SourceFile.tryGetChar (SourceFile.position (currentIndex + 1) sourceFile) sourceFile

        member _.Seek(position: int) = currentIndex <- position

        member _.Index = currentIndex

        member _.Position = SourceFile.position currentIndex sourceFile
