module FunLang.Primitive

open FunLang.Evaluator

// valueToString должна быть определена до её использования в print
let rec valueToString v =
    match v with
    | VNumber n -> string n
    | VBoolean b -> if b then "true" else "false"
    | VClosure(_, _, _) -> "<function>"
    | VPrimitive _ -> "<built-in>"
    | VPair(car, cdr) ->
        let rec listToString v =
            match v with
            | VNil -> ")"
            | VPair(a, VNil) -> (valueToString a) + ")"
            | VPair(a, rest) -> (valueToString a) + " " + listToString rest
            | _ -> "." + valueToString v + ")"
        "(" + listToString v
    | VNil -> "()"

let add args = match args with [VNumber a; VNumber b] -> VNumber (a + b) | _ -> failwith "+ expects two numbers"
let sub args = match args with [VNumber a; VNumber b] -> VNumber (a - b) | _ -> failwith "- expects two numbers"
let mul args = match args with [VNumber a; VNumber b] -> VNumber (a * b) | _ -> failwith "* expects two numbers"
let div args = match args with [VNumber a; VNumber b] -> VNumber (a / b) | _ -> failwith "/ expects two numbers"
let eq args = match args with [VNumber a; VNumber b] -> VBoolean (a = b) | [VBoolean a; VBoolean b] -> VBoolean (a = b) | _ -> failwith "= expects two numbers or booleans"
let lt args = match args with [VNumber a; VNumber b] -> VBoolean (a < b) | _ -> failwith "< expects two numbers"
let gt args = match args with [VNumber a; VNumber b] -> VBoolean (a > b) | _ -> failwith "> expects two numbers"
let not args = match args with [VBoolean b] -> VBoolean (not b) | _ -> failwith "not expects a boolean"
let andArgs args = match args with [VBoolean a; VBoolean b] -> VBoolean (a && b) | _ -> failwith "and expects two booleans"
let orArgs args = match args with [VBoolean a; VBoolean b] -> VBoolean (a || b) | _ -> failwith "or expects two booleans"

let cons args = match args with [a; b] -> VPair(a, b) | _ -> failwith "cons expects two arguments"
let car args = match args with [VPair(a,_)] -> a | [VNil] -> failwith "car of nil" | _ -> failwith "car expects a pair"
let cdr args = match args with [VPair(_,b)] -> b | [VNil] -> failwith "cdr of nil" | _ -> failwith "cdr expects a pair"
let nullQ args = match args with [VNil] -> VBoolean true | [VPair(_,_)] -> VBoolean false | _ -> failwith "null? expects a list (pair or nil)"
let list args = List.foldBack (fun x acc -> VPair(x, acc)) args VNil

let print args =
    match args with
    | [v] ->
        let str = valueToString v
        printfn "%s" str
        v
    | _ -> failwith "print expects one argument"

let initialEnv : Map<string, Value> =
    [ ("+", VPrimitive add)
      ("-", VPrimitive sub)
      ("*", VPrimitive mul)
      ("/", VPrimitive div)
      ("=", VPrimitive eq)
      ("<", VPrimitive lt)
      (">", VPrimitive gt)
      ("not", VPrimitive not)
      ("and", VPrimitive andArgs)
      ("or", VPrimitive orArgs)
      ("cons", VPrimitive cons)
      ("car", VPrimitive car)
      ("cdr", VPrimitive cdr)
      ("null?", VPrimitive nullQ)
      ("list", VPrimitive list)
      ("print", VPrimitive print)
    ] |> Map.ofList