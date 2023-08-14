module BasicParser

open Reader
open ParserCombinator

let pchar (c: char) : Parser<char, string> =
    Parser.make
    <| reader {
        let! (stream, index) = Reader.ask
        stream.Seek(index)

        return
            match stream.Next() with
            | Some c' when c = c' -> Ok(c, stream.Index)
            | Some c' -> Error $"Expected '%c{c}', but got '%c{c'}'"
            | None -> Error $"Expected '%c{c}', but reached end of input"
    }
