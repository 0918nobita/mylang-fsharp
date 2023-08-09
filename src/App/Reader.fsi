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
    member Return: 'T -> Reader<'Env, 'T>

    member ReturnFrom: Reader<'Env, 'T> -> Reader<'Env, 'T>

    member Bind: Reader<'Env, 'T> * ('T -> Reader<'Env, 'U>) -> Reader<'Env, 'U>

val reader: ReaderBuilder
