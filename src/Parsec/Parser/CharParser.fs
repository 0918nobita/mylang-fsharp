module Parsec.Parser.CharParser

open Parsec.SourceFileReader
open Parsec.SourcePos
open Type

let inline endOfFile () : Parser<'State, unit> =
    fun (state, reader: SourceFileReader) ->
        match reader.Read() with
        | _, None -> Ok { NewState = state; ParsedValue = () }
        | pos, Some c ->
            Error
                { Kind = Hard
                  Actual = $"'%c{c}'"
                  Expected = "[EOF]"
                  SourceRange = { Start = pos; End = pos } }

let inline anyChar () : Parser<'State, char * SourcePos> =
    fun (state, reader) ->
        match reader.Read() with
        | pos, Some c ->
            Ok
                { NewState = state
                  ParsedValue = (c, pos) }
        | pos, None ->
            Error
                { Kind = Hard
                  Actual = "[EOF]"
                  Expected = "[any char]"
                  SourceRange = { Start = pos; End = pos } }

let inline satisfy (helpMsg: string) ([<InlineIfLambda>] guard: char -> bool) : Parser<'State, char * SourcePos> =
    fun (state, reader) ->
        match reader.Read() with
        | pos, Some c when guard c ->
            Ok
                { NewState = state
                  ParsedValue = (c, pos) }
        | pos, Some c ->
            Error
                { Kind = Hard
                  Actual = $"'%c{c}'"
                  Expected = helpMsg
                  SourceRange = { Start = pos; End = pos } }
        | pos, None ->
            Error
                { Kind = Hard
                  Actual = "[EOF]"
                  Expected = helpMsg
                  SourceRange = { Start = pos; End = pos } }

let inline pchar (c: char) : Parser<'State, char * SourcePos> =
    fun (state, reader) ->
        match reader.Read() with
        | pos, Some c' when c = c' ->
            Ok
                { NewState = state
                  ParsedValue = (c, pos) }
        | pos, Some c' ->
            Error
                { Kind = Hard
                  Actual = $"'%c{c'}'"
                  Expected = $"'%c{c}'"
                  SourceRange = { Start = pos; End = pos } }
        | pos, None ->
            Error
                { Kind = Hard
                  Actual = "[EOF]"
                  Expected = $"'%c{c}'"
                  SourceRange = { Start = pos; End = pos } }
