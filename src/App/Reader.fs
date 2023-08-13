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
    member _.Bind(x: Reader<'Env, 'T>, f: 'T -> Reader<'Env, 'U>) : Reader<'Env, 'U> = Reader.bind f x

    member this.Combine(reader1: Reader<'Env, unit>, reader2: unit -> Reader<'Env, 'T>) : Reader<'Env, 'T> =
        this.Bind(reader1, reader2)

    member _.Delay(delayedReader: unit -> Reader<'Env, 'T>) : unit -> Reader<'Env, 'T> = delayedReader

    member _.Return(x: 'T) : Reader<'Env, 'T> = Reader(fun _ -> x)

    member _.ReturnFrom(reader: Reader<'Env, 'T>) : Reader<'Env, 'T> = reader

    member _.Run(delayedReader: unit -> Reader<'Env, 'T>) : Reader<'Env, 'T> = delayedReader ()

    member this.While(guard, body) =
        if guard () then
            this.Bind(body (), (fun () -> this.While(guard, body)))
        else
            this.Zero()

    member _.Zero() : Reader<'Env, unit> = Reader(fun _ -> ())

let reader = ReaderBuilder()
