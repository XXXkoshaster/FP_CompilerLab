module FunLang.Repl

open System
open FunLang.Parser
open FunLang.Evaluator
open FunLang.Primitive

let rec repl env =
    printf "FunLang> "
    match Console.ReadLine() with
    | null -> ()
    | line ->
        if line.Trim() = "" then repl env
        else
            match parse line with
            | Ok expr ->
                try
                    let result = eval env expr
                    printfn "%s" (valueToString result)
                    repl env
                with
                | ex -> printfn "Error: %s" ex.Message; repl env
            | Error err ->
                printfn "Parse error: %s" err
                repl env