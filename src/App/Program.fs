module Program

open SourceFile
open ParserCombinator

module P = BasicParser

let src = SourceFile.fromString "01013" |> SourceFileStream

let zeroOrOneParser = P.satisfy (fun c -> c = '0' || c = '1')

let myParser = Parser.some zeroOrOneParser

Parser.run src myParser |> printfn "%A"
