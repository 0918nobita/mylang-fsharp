module Program

open SourceFile
open ParserCombinator

module P = BasicParser

let src = SourceFile.fromString "010010011" |> SourceFileStream

let myParser = P.pstring "010" |> Parser.some

Parser.run src myParser |> printfn "%A"
