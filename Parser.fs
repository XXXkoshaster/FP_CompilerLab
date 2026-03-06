module Parser

open Tokenizer
open Lang


let mapToken = function
    | TNumber n -> Number n
    | TString s -> String s
    | TSymbol s -> Symbol s
    | _ -> failwith "Ошибка синтаксиса"

let parse (tokens: Token list) =
    let rec parseList acc = function
        | Open :: t ->
            let inner, rest = parseList [] t
            parseList (List(inner) :: acc) rest
        | Close :: t -> List.rev acc, t
        | tok :: t -> parseList (mapToken tok :: acc) t
        | [] -> List.rev acc, []

    let rec parseTop acc = function
        | Open :: t ->
            let inner, rest = parseList [] t
            parseTop (List(inner) :: acc) rest
        | tok :: t -> parseTop (mapToken tok :: acc) t
        | [] -> List.rev acc

    parseTop [] tokens
