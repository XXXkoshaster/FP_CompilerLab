module FunLang.Builtins

open FunLang.Evaluator

let private expectNumber = function
    | VNumber n -> n
    | v -> failwithf "Expected number, got %A" v

let private numBinOp f args =
    match args with
    | [a; b] -> VNumber (f (expectNumber a) (expectNumber b))
    | _ -> failwith "Expected exactly 2 arguments"

let private cmpBinOp f args =
    match args with
    | [a; b] -> VBoolean (f (expectNumber a) (expectNumber b))
    | _ -> failwith "Expected exactly 2 arguments"

let initialEnv : Env =
    Map.empty
    |> Map.add "+" (VPrimitive (numBinOp (+)))
    |> Map.add "-" (VPrimitive (numBinOp (-)))
    |> Map.add "*" (VPrimitive (numBinOp (*)))
    |> Map.add "/" (VPrimitive (numBinOp (/)))
    |> Map.add "=" (VPrimitive (cmpBinOp (=)))
    |> Map.add "<" (VPrimitive (cmpBinOp (<)))
    |> Map.add ">" (VPrimitive (cmpBinOp (>)))