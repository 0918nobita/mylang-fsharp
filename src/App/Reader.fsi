module Reader

type Reader<'Env, 'T>

module Reader =
    val run: 'Env -> Reader<'Env, 'T> -> 'T

    val ask: Reader<'Env, 'Env>

    val asks: ('Env -> 'T) -> Reader<'Env, 'T>

    val map: ('T -> 'U) -> Reader<'Env, 'T> -> Reader<'Env, 'U>

    val bind: ('T -> Reader<'Env, 'U>) -> Reader<'Env, 'T> -> Reader<'Env, 'U>

    val local: ('Env1 -> 'Env2) -> Reader<'Env2, 'T> -> Reader<'Env1, 'T>

[<Class>]
type ReaderBuilder =
    member inline Bind: Reader<'Env, 'T> * ('T -> Reader<'Env, 'U>) -> Reader<'Env, 'U>

    member inline Combine: Reader<'Env, unit> * (unit -> Reader<'Env, 'T>) -> Reader<'Env, 'T>

    member inline Delay: (unit -> Reader<'Env, 'T>) -> (unit -> Reader<'Env, 'T>)

    member Return: 'T -> Reader<'Env, 'T>

    member inline ReturnFrom: Reader<'Env, 'T> -> Reader<'Env, 'T>

    member inline Run: (unit -> Reader<'Env, 'T>) -> Reader<'Env, 'T>

    member While: (unit -> bool) * (unit -> Reader<'Env, unit>) -> Reader<'Env, unit>

    member Zero: unit -> Reader<'Env, unit>

val reader: ReaderBuilder
