module ParsecTest.Parser.Combinator

open Expecto

open Parsec.SourcePos
open Parsec.Parser.Type
open Parsec.Parser.Context
open Parsec.Parser.CE

module Prim = Parsec.Parser.Prim
module CharParser = Parsec.Parser.CharParser
module Combinator = Parsec.Parser.Combinator

[<Tests>]
let opt =
    test "opt" {
        let myParser1: Parser<unit, char> =
            parser {
                let! (c, _pos) = CharParser.pchar 'a'
                return c
            }

        let myParser2: Parser<unit, Option<char>> = Combinator.opt myParser1

        let context1 = ParserContext "a"
        Expect.equal (context1.Run(myParser2, initialState = ())) (Ok(Some 'a')) "元になっているパーサの実行に成功した場合、結果値を Some で包んで返す"

        let context2 = ParserContext "x"

        let failure =
            { Kind = Hard
              Actual = "'x'"
              Expected = "'a'"
              SourceRange =
                { Start = { Line = 0; Column = 1 }
                  End = { Line = 0; Column = 1 } } }

        Expect.equal
            (context2.Run(myParser2, initialState = ()))
            (Error failure)
            "元になっているパーサで Hard failure が発生した場合、それを伝播する"

        let myParser3: Parser<unit, Option<char>> =
            myParser1 |> Prim.backtrackable |> Combinator.opt

        Expect.equal
            (context2.Run(myParser3, initialState = ()))
            (Ok None)
            "元になっているパーサで Soft Failure が発生した場合、None を結果値として成功する"
    }
