module ParsecTest.Parser.Internal

open Expecto

open Parsec.SourceFileReader
open Parsec.SourcePos
open Parsec.Parser.Type
open Parsec.Parser.Context

module Prim = Parsec.Parser.Prim

[<Tests>]
let restorePos =
    test "restorePos" {
        let failure =
            { Kind = Soft
              Actual = "actual"
              Expected = "'b'"
              SourceRange =
                { Start = SourcePos.origin
                  End = SourcePos.origin } }

        let myParser: Parser<char, int> =
            fun (c, reader: SourceFileReader) ->
                match reader.Read() |> snd with
                | Some c' when c' = c ->
                    Ok
                        { NewState = c
                          ParsedValue = reader.CurrentIndex }
                | _ -> Error failure
            |> Prim.restorePos

        let context = ParserContext "abc"

        Expect.equal (context.Run(myParser, initialState = 'b')) (Error failure) "1 文字目は 'b' ではなく 'a' なので失敗する"

        Expect.equal
            (context.Run(myParser, initialState = 'a'))
            (Ok 0)
            "restorePos によって SourceFileReader の currentIndex が復元されている"
    }
