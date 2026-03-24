open FunLang.Parser
open FunLang.Evaluator
open FunLang.Builtins

let rec repl () =
    System.Console.Write("FunLang> ")
    let input = System.Console.ReadLine()
    if isNull input || input.Trim() = ":q" then
        ()
    else
        try
            match parse input with
            | Ok expr ->
                let value = eval initialEnv expr
                printfn "%A" value
            | Error e ->
                printfn "Parse error: %s" e
        with ex ->
            printfn "Error: %s" ex.Message
        repl ()

[<EntryPoint>]
let main _ =
    printfn "FunLang REPL. Type :q to quit."
    repl ()
    0