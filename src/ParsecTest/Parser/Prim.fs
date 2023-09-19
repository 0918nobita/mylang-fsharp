module ParsecTest.Parser.Prim

open Expecto

open Parsec.SourceFileReader
open Parsec.SourcePos
open Parsec.Parser.Type
open Parsec.Parser.Context

module Prim = Parsec.Parser.Prim
module CharParser = Parsec.Parser.CharParser

let sourceRange: SourceRange =
    { Start = SourcePos.origin
      End = SourcePos.origin }

let softFailure: Failure =
    { Kind = Soft
      Actual = "actual"
      Expected = "expected"
      SourceRange = sourceRange }

let hardFailure: Failure =
    { Kind = Hard
      Actual = "actual"
      Expected = "expected"
      SourceRange = sourceRange }

[<Tests>]
let make =
    test "make" {
        let context = ParserContext "abc"

        let myParser: Parser<unit, Option<char>> =
            fun (_state, reader) ->
                Ok
                    { NewState = ()
                      ParsedValue = reader.Read() |> snd }

        let actual = context.Run(myParser, initialState = ())
        Expect.equal actual (Ok(Some 'a')) "パーサが実行される"
    }

[<Tests>]
let memorize =
    testList
        "memorize"
        [ test "stateless" {
              // パーサの内部処理が走った回数
              let mutable count = 0

              let parser: Parser<unit, Option<char>> =
                  fun (_state, reader: SourceFileReader) ->
                      count <- count + 1

                      Ok
                          { NewState = ()
                            ParsedValue = reader.Read() |> snd }
                  |> Prim.memorize

              let context = ParserContext "abc"
              Expect.equal count 0 "パーサの内部処理は１度も走っていない"
              Expect.equal (context.Run(parser, initialState = ()), count) (Ok(Some 'a'), 1) "パーサの内部処理が走る"
              context.ResetReader()

              Expect.equal (context.Run(parser, initialState = ()), count) (Ok(Some 'a'), 1) "保存済みの値が返り、パーサの内部処理は走らない"
          }

          test "stateful" {
              // パーサの内部処理が走った回数
              let mutable count = 0

              let myParser: Parser<int, int * Option<char>> =
                  fun (state, reader: SourceFileReader) ->
                      count <- count + 1

                      Ok
                          { NewState = state + 1
                            ParsedValue = (state, reader.Read() |> snd) }
                  |> Prim.memorize

              let context = ParserContext "abc"
              Expect.equal count 0 "パーサの内部処理は１度も走っていない"
              Expect.equal (context.Run(myParser, initialState = 2), count) (Ok(2, Some 'a'), 1) "パーサの内部処理が走る"
              context.ResetReader()

              Expect.equal
                  (context.Run(myParser, initialState = 6), count)
                  (Ok(6, Some 'a'), 2)
                  "状態が異なるため、新規にパーサの内部処理が走る"

              context.ResetReader()

              Expect.equal
                  (context.Run(myParser, initialState = 2), count)
                  (Ok(2, Some 'a'), 2)
                  "同じ状態で１度実行されたので、内部処理は走らず保存された値が返る"
          } ]

[<Tests>]
let succeed =
    test "succeed" {
        let myParser: Parser<unit, string> = Prim.succeed "hello"
        let context = ParserContext "abc"
        Expect.equal (context.Run(myParser, initialState = ())) (Ok "hello") "いつも固定値を返す"
    }

[<Tests>]
let backtrackable =
    test "backtrackable" {
        let myParser: Parser<bool, int> =
            fun (state, _reader) ->
                if state then
                    Ok { NewState = state; ParsedValue = 12 }
                else
                    Error hardFailure
            |> Prim.backtrackable

        let context = ParserContext "abc"
        Expect.equal (context.Run(myParser, initialState = true)) (Ok 12) "内側のパーサが成功した場合、`Parser.backtrackable` は何もしない"

        Expect.equal
            (context.Run(myParser, initialState = false))
            (Error softFailure)
            "内側のパーサで発生した Hard failure は Soft failure に変換される"
    }

[<Tests>]
let map =
    test "map" {
        let myParser: Parser<unit, bool> =
            CharParser.anyChar () |> Prim.map (fst >> System.Char.IsWhiteSpace)

        let context1 = ParserContext " "
        Expect.equal (context1.Run(myParser, initialState = ())) (Ok true) "内側のパーサが成功した場合は、その結果値に関数を適用する"

        let context2 = ParserContext ""

        let failure =
            { Kind = Hard
              Actual = "[EOF]"
              Expected = "[any char]"
              SourceRange =
                { Start = SourcePos.origin
                  End = SourcePos.origin } }

        Expect.equal (context2.Run(myParser, initialState = ())) (Error failure) "内側のパーサが失敗した場合はエラー内容を伝播する"
    }

[<Tests>]
let bind =
    test "bind" {
        // binder が呼び出された回数
        let mutable count = 0

        let binder (c: char) =
            count <- count + 1
            Prim.succeed $"%c{c}!"

        let myParser1: Parser<unit, string> =
            fun (_state, reader: SourceFileReader) ->
                match reader.Read() |> snd with
                | Some c -> Ok { NewState = (); ParsedValue = c }
                | None -> Error softFailure
            |> Prim.bind binder

        let myParser2: Parser<unit, string> =
            fun (_state, _reader) -> Error hardFailure
            |> Prim.bind binder

        let context = ParserContext "a"

        Expect.equal
            (context.Run(myParser1, initialState = ()), count)
            (Ok "a!", 1)
            "成功した場合、binder を適用して得られたパーサを実行してその結果を返す"

        Expect.equal
            (context.Run(myParser1, initialState = ()), count)
            (Error softFailure, 1)
            "Soft failure が発生した場合、binder は呼び出さずにエラー内容を伝播する"

        Expect.equal
            (context.Run(myParser2, initialState = ()), count)
            (Error hardFailure, 1)
            "Hard failure が発生した場合、binder は呼び出さずにエラー内容を伝播する"
    }
