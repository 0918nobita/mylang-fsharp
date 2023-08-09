module Program

open SourceFile
open SourceFileStream
open ParserCombinator

let src = SourceFile.fromString "Hello,\nworld!" |> SourceFileStream

let hParser =
    parser {
        let! c = Parser.pchar 'H'
        printfn "hParser executed"
        return c
    }

let myParser1 =
    parser {
        let! firstChar = hParser
        let! secondChar = Parser.pchar 'i'
        return $"%c{firstChar}%c{secondChar}"
    }

let myParser2 =
    parser {
        let! firstChar = hParser
        let! secondChar = Parser.pchar 'e'
        return $"%c{firstChar}%c{secondChar}"
    }

let myParser3 = myParser1 |> Parser.alt myParser2

Parser.run src myParser3 |> printfn "%A"
