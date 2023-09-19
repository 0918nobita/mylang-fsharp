/// Reader モナド
module Parsec.Reader

type Reader<'Env, 'T> = 'Env -> 'T

module Reader =
    let inline run (env: 'Env) (action: Reader<'Env, 'T>) : 'T = action env

    let ask: Reader<'Env, 'Env> = id

    let inline asks ([<InlineIfLambda>] f: 'Env -> 'U) : Reader<'Env, 'U> = f

    let inline map ([<InlineIfLambda>] mapping: 'T -> 'U) (reader: Reader<'Env, 'T>) : Reader<'Env, 'U> =
        let newAction env = mapping (run env reader)
        newAction

    let inline bind ([<InlineIfLambda>] binder: 'T -> Reader<'Env, 'U>) (reader: Reader<'Env, 'T>) : Reader<'Env, 'U> =
        let newAction env =
            let newReader = binder (run env reader)
            run env newReader

        newAction

type ReaderBuilder() =
    member inline _.Return(x) = fun _ -> x

    member inline _.ReturnFrom(x) = x

    member inline _.BindReturn(x, [<InlineIfLambda>] f) = Reader.map f x

    member inline _.Bind(x, [<InlineIfLambda>] f) = Reader.bind f x

let reader = ReaderBuilder()
