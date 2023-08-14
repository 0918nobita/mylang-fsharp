module Program

open SourceFile
open ParserCombinator

let src = SourceFile.fromString "01013" |> SourceFileStream

let zeroParser = Parser.pchar '0'

let oneParser = Parser.pchar '1'

let zeroOrOneParser = zeroParser |> Parser.alt oneParser

let myParser = Parser.many zeroOrOneParser

Parser.run src myParser |> printfn "%A"
