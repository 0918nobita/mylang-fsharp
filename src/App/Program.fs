module Mylang.Program

open System.IO

[<EntryPoint>]
let main argv =
    if Array.isEmpty argv then
        printfn "ソースファイルを指定してください"
        1
    else
        try
            let source = File.ReadAllText argv.[0]

            Parser.parse source |> printfn "%A"

            0
        with
        | :? DirectoryNotFoundException
        | :? FileNotFoundException ->
            printfn "指定されたファイルが見つかりませんでした (%s)" argv.[0]
            1
