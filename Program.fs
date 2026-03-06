module Program

open System.IO
open Tokenizer
open Parser
open Interpreter

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        printfn "Использование: dotnet run -- <файл.scm>"
        printfn "Примеры:"
        printfn "  dotnet run -- examples/factorial.scm"
        printfn "  dotnet run -- examples/fibonacci.scm"
        printfn "  dotnet run -- examples/lists.scm"
        printfn "  dotnet run -- examples/closures.scm"
        printfn "  dotnet run -- examples/sort.scm"
        1
    else
        let filename = argv.[0]
        let source = File.ReadAllText(filename)
        try
            let tokens = tokenize source
            let ast = parse tokens
            interpret ast
            0
        with
        | ex ->
            eprintfn "Ошибка: %s" ex.Message
            1
