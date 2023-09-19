module ParsecTest.Parser.State

open Expecto

open Parsec.Parser.Context

module Prim = Parsec.Parser.Prim
module S = Parsec.Parser.State

[<Tests>]
let getState =
    test "getState" {
        let myParser = S.getState ()
        let context = ParserContext "abc"
        Expect.equal (context.Run(myParser, initialState = 12)) (Ok 12) "初期状態を取得し、それを結果値とする"
    }

[<Tests>]
let putState =
    test "putState" {
        let myParser = S.putState 24 |> Prim.bind S.getState
        let context = ParserContext "abc"
        Expect.equal (context.Run(myParser, initialState = 12)) (Ok 24) "状態を上書きする"
    }

[<Tests>]
let modifyState =
    test "modifyState" {
        let myParser = S.modifyState (fun n -> n * 2) |> Prim.bind S.getState
        let context = ParserContext "abc"
        Expect.equal (context.Run(myParser, initialState = 12)) (Ok 24) "指定された関数を状態に適用して更新する"
    }
