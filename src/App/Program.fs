module Program

open SourceFile
open ParserCombinator

module P = BasicParser

let src = SourceFile.fromString "01013" |> SourceFileStream

let zeroParser = P.pchar '0'

let oneParser = P.pchar '1'

let zeroOrOneParser = zeroParser |> Parser.alt oneParser

let myParser = Parser.many zeroOrOneParser

Parser.run src myParser |> printfn "%A"
