module ParserCombinator

open Reader
open SourceFileStream

type Parser<'T, 'Error>

module Parser =
    val make: Reader<ISourceFileStream * int, Result<'T * int, 'E>> -> Parser<'T, 'E>

    val run: ISourceFileStream -> Parser<'T, 'E> -> Result<'T * int, 'E>

    val succeed: 'T -> Parser<'T, 'E>

    val map: ('T -> 'U) -> Parser<'T, 'E> -> Parser<'U, 'E>

    val bind: ('T -> Parser<'U, 'E>) -> Parser<'T, 'E> -> Parser<'U, 'E>

    val alt: Parser<'T, 'E> -> Parser<'T, 'E> -> Parser<'T, 'E>

    val many: Parser<'T, 'E> -> Parser<'T list, unit>

[<Class>]
type ParserBuilder =
    member Return: 'T -> Parser<'T, 'E>

    member ReturnFrom: Parser<'T, 'E> -> Parser<'T, 'E>

    member Bind: Parser<'T, 'E> * ('T -> Parser<'U, 'E>) -> Parser<'U, 'E>

val parser: ParserBuilder
