module Interpreter

open Lang

let extend env bindings =
    ref (Map.ofList bindings) :: env

let lookup env symbol =
    match List.tryPick (fun (frame: Frame) -> Map.tryFind symbol frame.Value) env with
    | Some v -> v
    | None -> failwithf "Переменная '%s' не найдена" symbol

let rec print expr =
    match expr with
    | Number n -> string n
    | String s -> s
    | Symbol s -> s
    | List items ->
        "(" + (items |> List.map print |> String.concat " ") + ")"
    | Function _ | Special _ -> "Function"
    | Dummy _ -> ""

let malformed name args =
    failwithf "Неверная форма '%s': %s" name (print (List args))

let mathOp op name args =
    match args with
    | [Number a; Number b] -> Number(op a b)
    | _ -> malformed name args

let compareOp pred name args =
    match args with
    | [Number a; Number b] -> if pred a b then Number 1 else Number 0
    | _ -> malformed name args

let rec ifForm env = function
    | [cond; thenExpr; elseExpr] ->
        match eval env cond with
        | Number 0 | List [] -> eval env elseExpr
        | _ -> eval env thenExpr
    | args -> malformed "if" args

and letForm env = function
    | [List bindings; body] ->
        let bind = function
            | List [Symbol s; e] -> (s, ref (eval env e))
            | m -> malformed "let binding" [m]
        let env' = bindings |> List.map bind |> extend env
        eval env' body
    | args -> malformed "let" args

and letrecForm env = function
    | [List bindings; body] ->
        let makeDummy = function
            | List [Symbol s; _] -> (s, ref (Dummy "letrec"))
            | m -> malformed "letrec binding" [m]
        let env' = bindings |> List.map makeDummy |> extend env
        let frame = (List.head env').Value
        bindings |> List.iter (function
            | List [Symbol s; e] ->
                let value = eval env' e
                (Map.find s frame) := value
            | _ -> ())
        eval env' body
    | args -> malformed "letrec" args

and lambdaForm env = function
    | [List parameters; body] ->
        let closure args =
            let bindings =
                List.map2 (fun p a ->
                    match p with
                    | Symbol s -> (s, ref a)
                    | _ -> malformed "lambda param" [p]
                ) parameters args
            eval (extend env bindings) body
        Function closure
    | args -> malformed "lambda" args

and defineForm env = function
    | [Symbol name; expr] ->
        let value = eval env expr
        let frame = List.head env
        frame := Map.add name (ref value) frame.Value
        Dummy (sprintf "Defined %s" name)
    | args -> malformed "define" args

and beginForm env args =
    let rec loop = function
        | [last] -> eval env last
        | h :: t -> eval env h |> ignore; loop t
        | [] -> Dummy "empty begin"
    loop args

and eval env expr =
    match expr with
    | Number _ | String _ | Dummy _ -> expr
    | Symbol s -> (lookup env s).Value
    | List(head :: args) ->
        match eval env head with
        | Function f -> apply env f args
        | Special f -> f env args
        | other -> malformed "вызов" [other]
    | List [] -> List []
    | _ -> failwithf "Невозможно вычислить: %A" expr

and apply env fn args =
    let evaluated = args |> List.map (eval env)
    fn evaluated

let makeGlobalEnv () : Environment =
    [ref (Map.ofList
        [ "+",  ref (Function (mathOp (+) "+"))
          "-",  ref (Function (mathOp (-) "-"))
          "*",  ref (Function (mathOp (*) "*"))
          "/",  ref (Function (mathOp (/) "/"))
          "%",  ref (Function (mathOp (%) "%"))
          "=",  ref (Function (compareOp (=) "="))
          ">",  ref (Function (compareOp (>) ">"))
          "<",  ref (Function (compareOp (<) "<"))
          ">=", ref (Function (compareOp (>=) ">="))
          "<=", ref (Function (compareOp (<=) "<="))

          "if",     ref (Special ifForm)
          "let",    ref (Special letForm)
          "letrec", ref (Special letrecForm)
          "lambda", ref (Special lambdaForm)
          "define", ref (Special defineForm)
          "begin",  ref (Special beginForm)

          "cons", ref (Function (function
              | [h; List t] -> List(h :: t)
              | args -> malformed "cons" args))

          "car", ref (Function (function
              | [List(h :: _)] -> h
              | args -> malformed "car" args))

          "cdr", ref (Function (function
              | [List(_ :: t)] -> List t
              | args -> malformed "cdr" args))

          "list", ref (Function List)

          "null?", ref (Function (function
              | [List []] -> Number 1
              | [_] -> Number 0
              | args -> malformed "null?" args))

          "display", ref (Function (function
              | [e] -> printf "%s" (print e); Dummy "display"
              | args -> malformed "display" args))

          "newline", ref (Function (function
              | [] -> printfn ""; Dummy "newline"
              | args -> malformed "newline" args))

          "read-file", ref (Function (function
              | [String path] -> String(System.IO.File.ReadAllText(path))
              | args -> malformed "read-file" args))

          "write-file", ref (Function (function
              | [String path; String content] ->
                  System.IO.File.WriteAllText(path, content)
                  Dummy "write-file"
              | args -> malformed "write-file" args))

          "append-file", ref (Function (function
              | [String path; String content] ->
                  System.IO.File.AppendAllText(path, content)
                  Dummy "append-file"
              | args -> malformed "append-file" args))

          "file-exists?", ref (Function (function
              | [String path] -> if System.IO.File.Exists(path) then Number 1 else Number 0
              | args -> malformed "file-exists?" args))
        ])]

let interpret (exprs: Expression list) =
    let env = makeGlobalEnv ()
    exprs |> List.iter (fun expr -> eval env expr |> ignore)
