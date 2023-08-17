module Program

open SourceFile
open ParserCombinator

module P = BasicParser

let src = SourceFile.fromString "010010011" |> SourceFileStream

let myParser =
    parser {
        let! hoge = P.pstring "010" |> Parser.some
        let! fuga = P.pstring "011"
        return (fst hoge) :: ((snd hoge) @ [ fuga ])
    }

Parser.run src myParser |> printfn "%A"
