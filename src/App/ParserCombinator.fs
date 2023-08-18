module ParserCombinator

open System.Collections.Generic

open Reader
open SourceFileStream

type Parser<'T, 'Error> = private Parser of Reader<ISourceFileStream * int, Result<'T * int, 'Error>>

module Parser =
    let make (parse: Reader<ISourceFileStream * int, Result<'T * int, 'E>>) : Parser<'T, 'E> =
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

    let run (stream: ISourceFileStream) (Parser parse: Parser<'T, 'E>) = Reader.run (stream, 0) parse

    let inline succeed (parsedValue: 'T) : Parser<'T, 'E> =
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

    let mapError (mapping: 'E1 -> 'E2) (Parser parse: Parser<'T, 'E1>) : Parser<'T, 'E2> =
        make
        <| reader {
            let! result = parse
            return result |> Result.mapError mapping
        }

    let bind (binder: 'T -> Parser<'U, 'E>) (Parser parse: Parser<'T, 'E>) : Parser<'U, 'E> =
        make
        <| reader {
            match! parse with
            | Ok(parsedValue, newIndex) ->
                let (Parser parse') = binder parsedValue
                return! parse' |> Reader.local (fun (stream, _) -> (stream, newIndex))
            | Error err -> return Error err
        }

    let alt (Parser succeedingParser: Parser<'T, 'E2>) (Parser precedingParser: Parser<'T, 'E1>) : Parser<'T, 'E2> =
        make
        <| reader {
            match! precedingParser with
            | Ok success -> return Ok success
            | Error _ -> return! succeedingParser
        }

    let recover (recovery: 'E1 -> Parser<'T, 'E2>) (Parser parse: Parser<'T, 'E1>) : Parser<'T, 'E2> =
        make
        <| reader {
            match! parse with
            | Ok success -> return Ok success
            | Error err ->
                let (Parser parse') = recovery err
                return! parse'
        }

    let many (Parser parse: Parser<'T, 'E1>) : Parser<'T list, 'E2> =
        make
        <| reader {
            let! (stream, index) = Reader.ask

            let mutable results: 'T list = []

            let mutable currentIndex = index

            let mutable shouldContinue = true

            while shouldContinue do
                match! parse |> Reader.local (fun _ -> (stream, currentIndex)) with
                | Ok(parsedValue, newIndex) ->
                    results <- parsedValue :: results
                    currentIndex <- newIndex
                | Error _ -> shouldContinue <- false

            return Ok(List.rev results, currentIndex)
        }

    let some (Parser parse: Parser<'T, 'E>) : Parser<'T * 'T list, 'E> =
        make
        <| reader {
            let! stream = Reader.asks fst

            match! parse with
            | Ok(head, newIndex) ->
                let mutable tail: 'T list = []

                let mutable currentIndex = newIndex

                let mutable shouldContinue = true

                while shouldContinue do
                    match! parse |> Reader.local (fun _ -> (stream, currentIndex)) with
                    | Ok(parsedValue, newIndex') ->
                        tail <- parsedValue :: tail
                        currentIndex <- newIndex'
                    | Error _ -> shouldContinue <- false

                return Ok((head, List.rev tail), currentIndex)
            | Error err -> return Error err
        }

type ParserBuilder() =
    member inline _.Return(parsedValue: 'T) : Parser<'T, 'E> = Parser.succeed parsedValue

    member inline _.ReturnFrom(parser: Parser<'T, 'E>) : Parser<'T, 'E> = parser

    member inline _.Bind(parser: Parser<'T, 'E>, [<InlineIfLambda>] binder: 'T -> Parser<'U, 'E>) : Parser<'U, 'E> =
        Parser.bind binder parser

let parser = ParserBuilder()
