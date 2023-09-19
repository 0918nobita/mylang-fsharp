module Mylang.Parser.Expr

open System

open Parsec.SourcePos
open Parsec.Parser.Type
open Parsec.Parser.CE
open Mylang.Ast.Identifier

module Prim = Parsec.Parser.Prim
module Combinator = Parsec.Parser.Combinator
module CharParser = Parsec.Parser.CharParser

let inline private identHeadChar c = Char.IsAsciiLetter c || c = '_'

let inline private identTailChar c = Char.IsAsciiLetterOrDigit c || c = '_'

let identParser: Parser<unit, Identifier> =
    parser {
        let! (head, headPos) = CharParser.satisfy "ASCII letter or '_'" identHeadChar

        let! tail =
            CharParser.satisfy "ASCII letter, digit or '_'" identTailChar
            |> Prim.backtrackable
            |> Combinator.many

        let sourceRange: SourceRange =
            { Start = headPos
              End = tail |> List.tryLast |> Option.map snd |> Option.defaultValue headPos }

        let text = head :: (tail |> List.map fst) |> List.toArray |> String

        return
            { Text = text
              SourceRange = sourceRange }
    }
