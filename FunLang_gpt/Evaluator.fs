module FunLang.Evaluator

open FunLang.AST

type Value =
    | VNumber of int
    | VBoolean of bool
    | VClosure of string list * Expr * Env ref
    | VPrimitive of (Value list -> Value)
    | VPair of Value * Value
    | VNil

and Env = Map<string, Value>

let rec eval (env: Env) (expr: Expr) : Value =
    match expr with
    | Number n  -> VNumber n
    | Boolean b -> VBoolean b
    | Symbol s ->
        match Map.tryFind s env with
        | Some v -> v
        | None   -> failwithf "Unbound symbol: %s" s
    | Lambda(pars, body) ->
        VClosure(pars, body, ref env)
    | Apply(funcExpr, args) ->
        let func = eval env funcExpr
        let argVals = List.map (eval env) args
        apply func argVals
    | If(cond, thenExpr, elseExpr) ->
        match eval env cond with
        | VBoolean true  -> eval env thenExpr
        | VBoolean false -> eval env elseExpr
        | v -> failwithf "Condition must be boolean, got %A" v
    | Let(bindings, body) ->
        let newEnv =
            List.fold (fun e (name, ex) -> Map.add name (eval env ex) e) env bindings
        eval newEnv body
    | LetRec(bindings, body) ->
        let evaluated = bindings |> List.map (fun (name, ex) -> name, eval env ex)
        let finalEnv = List.fold (fun e (name, v) -> Map.add name v e) env evaluated
        evaluated |> List.iter (fun (_, v) ->
            match v with
            | VClosure(_, _, envRef) -> envRef.Value <- finalEnv
            | _ -> failwith "LetRec bindings must be lambda expressions")
        eval finalEnv body

and apply func args =
    match func with
    | VPrimitive f -> f args
    | VClosure(pars, body, envRef) ->
        if List.length pars <> List.length args then
            failwithf "Arity mismatch: expected %d arguments, got %d" (List.length pars) (List.length args)
        let callEnv = List.fold2 (fun e p a -> Map.add p a e) envRef.Value pars args
        eval callEnv body
    | _ -> failwithf "Cannot apply non-function: %A" func