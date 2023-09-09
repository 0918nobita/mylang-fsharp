module Mylang.Program

open System.IO
open Parsec

[<EntryPoint>]
let main argv =
    if Array.isEmpty argv then
        printfn "ソースファイルを指定してください"
        1
    else
        try
            let source = File.ReadAllText argv.[0]

            use context = new ParserContext(source)

            context.Run(MyParser.programParser, initialState = ()) |> printfn "%A"

            0
        with
        | :? DirectoryNotFoundException
        | :? FileNotFoundException ->
            printfn "指定されたファイルが見つかりませんでした (%s)" argv.[0]
            1
