module Mylang.Program

open System
open System.IO
open Parsec

let exit1 errorMsg =
    Console.ForegroundColor <- ConsoleColor.Red
    eprintfn "%s" errorMsg
    Console.ResetColor()
    1

[<EntryPoint>]
let main argv =
    ShuntingYard.example ()

    if Array.isEmpty argv then
        exit1 "ソースファイルを指定してください"
    else
        try
            let sourceFileAttr = File.GetAttributes argv.[0]

            if (sourceFileAttr.HasFlag(FileAttributes.Directory)) then
                exit1 $"ディレクトリが指定されました。ディレクトリではなくファイルを指定してください。(%s{argv.[0]})"
            else
                let sourceFileInfo = FileInfo argv.[0]
                let sourceFile = sourceFileInfo.FullName
                let outFileName = Path.ChangeExtension(sourceFileInfo.Name, ".ts")
                let outFilePath = Path.Combine(sourceFileInfo.Directory.FullName, outFileName)

                Console.ForegroundColor <- ConsoleColor.Green
                printf "[Compiling]"
                Console.ResetColor()
                printfn " %s => %s" sourceFile outFilePath

                let source = File.ReadAllText sourceFile

                use context = new ParserContext(source)

                match context.Run(MyParser.programParser, initialState = ()) with
                | Success success ->
                    let parsed = success.ParsedValue |> List.toArray
                    let tsAst = CodeGen.codegen parsed
                    let compiled = tsAst |> TSTreePrinter.print
                    File.WriteAllText(outFilePath, compiled + "\n")
                    printfn "Done \u2728"
                    0
                | SoftFailure failure
                | HardFailure failure -> exit1 $"%A{failure}"
        with
        | :? DirectoryNotFoundException
        | :? FileNotFoundException -> exit1 $"指定されたファイルが見つかりませんでした (%s{argv.[0]})"
