module Reader

type Reader<'Env, 'T> = Reader of ('Env -> 'T)

module Reader =
    let run (env: 'Env) (Reader action: Reader<'Env, 'T>) : 'T = action env

    let ask: Reader<'Env, 'Env> = Reader id

    let asks (f: 'Env -> 'T) : Reader<'Env, 'T> = Reader f

    let map (mapping: 'T -> 'U) (reader: Reader<'Env, 'T>) : Reader<'Env, 'U> =
        Reader <| fun env -> mapping (run env reader)

    let bind (binder: 'T -> Reader<'Env, 'U>) (reader: Reader<'Env, 'T>) : Reader<'Env, 'U> =
        Reader
        <| fun env ->
            let newAction = binder (run env reader)
            run env newAction

    let local (f: 'Env1 -> 'Env2) (Reader action: Reader<'Env2, 'T>) : Reader<'Env1, 'T> =
        Reader
        <| fun env ->
            let newEnv = f env
            let result = action newEnv
            result

type ReaderBuilder() =
    member _.Return(x: 'T) : Reader<'Env, 'T> = Reader(fun _ -> x)

    member _.ReturnFrom(reader: Reader<'Env, 'T>) : Reader<'Env, 'T> = reader

    member _.Bind(x: Reader<'Env, 'T>, f: 'T -> Reader<'Env, 'U>) : Reader<'Env, 'U> = Reader.bind f x

let reader = ReaderBuilder()
