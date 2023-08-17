module ParserCombinator

open Reader
open SourceFileStream

type Parser<'T, 'Error>

module Parser =
    val make: Reader<ISourceFileStream * int, Result<'T * int, 'E>> -> Parser<'T, 'E>

    val run: ISourceFileStream -> Parser<'T, 'E> -> Result<'T * int, 'E>

    val inline succeed: 'T -> Parser<'T, 'E>

    val map: ('T -> 'U) -> Parser<'T, 'E> -> Parser<'U, 'E>

    val bind: ('T -> Parser<'U, 'E>) -> Parser<'T, 'E> -> Parser<'U, 'E>

    val alt: Parser<'T, 'E> -> Parser<'T, 'E> -> Parser<'T, 'E>

    val many: Parser<'T, 'E> -> Parser<'T list, unit>

    val some: Parser<'T, 'E> -> Parser<'T * 'T list, 'E>

[<Class>]
type ParserBuilder =
    member inline Return: 'T -> Parser<'T, 'E>

    member inline ReturnFrom: Parser<'T, 'E> -> Parser<'T, 'E>

    member inline Bind: Parser<'T, 'E> * ('T -> Parser<'U, 'E>) -> Parser<'U, 'E>

val parser: ParserBuilder
