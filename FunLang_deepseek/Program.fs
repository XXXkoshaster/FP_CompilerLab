module FunLang.Program

open FunLang.Repl
open FunLang.Primitive

[<EntryPoint>]
let main argv =
    printfn "Welcome to FunLang interpreter!"
    printfn "Type expressions to evaluate. Press Ctrl+C to exit."
    repl initialEnv
    0