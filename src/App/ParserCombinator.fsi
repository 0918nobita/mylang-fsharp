module ParserCombinator

open Reader
open ISourceFile

type Parser<'T, 'Error>

module Parser =
    val make: Reader<ISourceFile * int, Result<'T * int, 'E>> -> Parser<'T, 'E>

    val run: ISourceFile -> Parser<'T, 'E> -> Result<'T * int, 'E>

    val inline succeed: 'T -> Parser<'T, 'E>

    val map: ('T -> 'U) -> Parser<'T, 'E> -> Parser<'U, 'E>

    val mapError: ('E1 -> 'E2) -> Parser<'T, 'E1> -> Parser<'T, 'E2>

    val bind: ('T -> Parser<'U, 'E>) -> Parser<'T, 'E> -> Parser<'U, 'E>

    val alt: Parser<'T, 'E2> -> Parser<'T, 'E1> -> Parser<'T, 'E2>

    val recover: ('E1 -> Parser<'T, 'E2>) -> Parser<'T, 'E1> -> Parser<'T, 'E2>

    val many: Parser<'T, 'E1> -> Parser<'T list, 'E2>

    val some: Parser<'T, 'E> -> Parser<'T * 'T list, 'E>

[<Class>]
type ParserBuilder =
    member inline Return: 'T -> Parser<'T, 'E>

    member inline ReturnFrom: Parser<'T, 'E> -> Parser<'T, 'E>

    member inline Bind: Parser<'T, 'E> * ('T -> Parser<'U, 'E>) -> Parser<'U, 'E>

val parser: ParserBuilder
