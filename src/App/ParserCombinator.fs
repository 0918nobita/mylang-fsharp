module ParserCombinator

open System.Collections.Generic

open Reader
open SourceFileStream

type Parser<'T, 'Error> = private Parser of Reader<SourceFileStream * int, Result<'T * int, 'Error>>

module Parser =
    let make (parse: Reader<SourceFileStream * int, Result<'T * int, 'E>>) : Parser<'T, 'E> =
        let memo = new Dictionary<int, Result<'T * int, 'E>>()

        Parser
        <| reader {
            let! index = Reader.asks snd

            if memo.ContainsKey(index) then
                return memo.[index]
            else
                let! result = parse
                memo.Add(index, result)
                return result
        }

    let run (stream: SourceFileStream) (Parser parse: Parser<'T, 'E>) = Reader.run (stream, 0) parse

    let succeed (parsedValue: 'T) : Parser<'T, 'E> =
        make
        <| reader {
            let! index = Reader.asks snd
            return Ok(parsedValue, index)
        }

    let map (mapping: 'T -> 'U) (Parser parse: Parser<'T, 'E>) : Parser<'U, 'E> =
        make
        <| reader {
            let! result = parse
            return result |> Result.map (fun (parsedValue, index) -> (mapping parsedValue, index))
        }

    let bind (binder: 'T -> Parser<'U, 'E>) (Parser parse: Parser<'T, 'E>) : Parser<'U, 'E> =
        make
        <| reader {
            let! result = parse

            match result with
            | Ok(parsedValue, newIndex) ->
                let (Parser parse') = binder parsedValue
                return! parse' |> Reader.local (fun (stream, _) -> (stream, newIndex))
            | Error err -> return Error err
        }

    let alt (Parser succeedingParser: Parser<'T, 'E>) (Parser precedingParser: Parser<'T, 'E>) : Parser<'T, 'E> =
        make
        <| reader {
            let! result = precedingParser

            match result with
            | Ok _ -> return result
            | Error _ -> return! succeedingParser
        }

    let pchar (c: char) : Parser<char, string> =
        make
        <| reader {
            let! (stream, index) = Reader.ask
            stream.Seek(index)

            return
                match stream.Next() with
                | Some c' when c = c' -> Ok(c, stream.Index)
                | Some c' -> Error $"Expected '%c{c}', but got '%c{c'}'"
                | None -> Error $"Expected '%c{c}', but reached end of input"
        }

type ParserBuilder() =
    member _.Return(parsedValue: 'T) : Parser<'T, 'E> = Parser.succeed parsedValue

    member _.ReturnFrom(parser: Parser<'T, 'E>) : Parser<'T, 'E> = parser

    member _.Bind(parser: Parser<'T, 'E>, binder: 'T -> Parser<'U, 'E>) : Parser<'U, 'E> = Parser.bind binder parser

let parser = ParserBuilder()
