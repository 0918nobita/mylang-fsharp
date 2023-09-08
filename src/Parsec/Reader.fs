/// Reader モナド
[<AutoOpen>]
module Parsec.Reader

type Reader<'Env, 'T> = Reader of ('Env -> 'T)

module Reader =
    let inline run (env: 'Env) (Reader action: Reader<'Env, 'T>) : 'T = action env

    let ask: Reader<'Env, 'Env> = Reader(fun env -> env)

    let inline asks ([<InlineIfLambda>] f: 'Env -> 'U) : Reader<'Env, 'U> = Reader(fun env -> f env)

    let inline map ([<InlineIfLambda>] mapping: 'T -> 'U) (reader: Reader<'Env, 'T>) : Reader<'Env, 'U> =
        let newAction env = mapping (run env reader)
        Reader newAction

    let inline bind ([<InlineIfLambda>] binder: 'T -> Reader<'Env, 'U>) (reader: Reader<'Env, 'T>) : Reader<'Env, 'U> =
        let newAction env =
            let newReader = binder (run env reader)
            run env newReader

        Reader newAction

type ReaderBuilder() =
    member inline _.Return(x) = Reader(fun _ -> x)

    member inline _.ReturnFrom(x) = x

    member inline _.BindReturn(x, [<InlineIfLambda>] f) = Reader.map f x

    member inline _.Bind(x, [<InlineIfLambda>] f) = Reader.bind f x

let reader = ReaderBuilder()
