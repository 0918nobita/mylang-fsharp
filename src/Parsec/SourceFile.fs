[<AutoOpen>]
module Parsec.SourceFile

type private SourceChar = { Position: SourcePos; Char: char }

type SourceFile = private SourceFile of SourceChar[]

module SourceFile =
    let fromString (source: string) : SourceFile =
        let folder
            ((cursorIndex, pos, lines): int * SourcePos * list<list<SourceChar>>)
            (line: string)
            : int * SourcePos * list<list<SourceChar>> =
            let newCursorIndex = cursorIndex + String.length line
            let newPos = SourcePos.nextLine pos

            let lineChars =
                line + "\n"
                |> Seq.mapi (fun i c ->
                    { Position = { Line = pos.Line; Column = i }
                      Char = c })
                |> Seq.toList

            (newCursorIndex, newPos, lineChars :: lines)

        let initialState = (0, SourcePos.origin, [])

        let (_, _, lines) =
            source.Split([| "\n" |], System.StringSplitOptions.None)
            |> Array.fold folder initialState

        lines |> List.rev |> List.concat |> List.toArray |> SourceFile

    let position (cursorIndex: int) (SourceFile chars) : SourcePos =
        let len = Array.length chars

        if len = 0 then
            { Line = 0; Column = cursorIndex }
        else
            Array.tryItem cursorIndex chars
            |> Option.map (fun { Position = pos } -> pos)
            |> Option.defaultWith (fun () ->
                let lastCharPos = chars.[len - 1].Position
                let line = lastCharPos.Line
                let column = lastCharPos.Column + cursorIndex - len + 1
                { Line = line; Column = column })

    let tryGetChar (cursorIndex: int) (SourceFile chars) : SourcePos * Option<char> =
        let len = Array.length chars

        if len = 0 then
            ({ Line = 0; Column = cursorIndex }, None)
        else
            let sourceChar = Array.tryItem cursorIndex chars

            let pos =
                sourceChar
                |> Option.map (fun { Position = pos } -> pos)
                |> Option.defaultWith (fun () ->
                    let lastCharPos = chars.[len - 1].Position
                    let line = lastCharPos.Line
                    let column = lastCharPos.Column + cursorIndex - len + 1
                    { Line = line; Column = column })

            let c = sourceChar |> Option.map (fun { Char = c } -> c)
            (pos, c)
