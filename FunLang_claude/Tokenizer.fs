module Tokenizer

type Token =
    | Open
    | Close
    | TNumber of int
    | TString of string
    | TSymbol of string

let rec readString acc = function
    | '"' :: t -> acc, t
    | '\\' :: 'n' :: t -> readString (acc + "\n") t
    | '\\' :: '"' :: t -> readString (acc + "\"") t
    | '\\' :: '\\' :: t -> readString (acc + "\\") t
    | c :: t -> readString (acc + string c) t
    | [] -> failwith "Незакрытая строка"

let rec readSymbol acc = function
    | c :: t when c <> '(' && c <> ')' && not (System.Char.IsWhiteSpace c) ->
        readSymbol (acc + string c) t
    | rest -> acc, rest

let tokenize (input: string) =
    let rec loop acc = function
        | [] -> List.rev acc
        | c :: t when System.Char.IsWhiteSpace c -> loop acc t
        | ';' :: t ->
            let rec skip = function
                | '\n' :: rest -> rest
                | _ :: rest -> skip rest
                | [] -> []
            loop acc (skip t)
        | '(' :: t -> loop (Open :: acc) t
        | ')' :: t -> loop (Close :: acc) t
        | '"' :: t ->
            let s, rest = readString "" t
            loop (TString s :: acc) rest
        | '-' :: d :: t when System.Char.IsDigit d ->
            let s, rest = readSymbol ("-" + string d) t
            loop (TNumber(int s) :: acc) rest
        | d :: t when System.Char.IsDigit d ->
            let s, rest = readSymbol (string d) t
            loop (TNumber(int s) :: acc) rest
        | c :: t ->
            let s, rest = readSymbol (string c) t
            loop (TSymbol s :: acc) rest

    loop [] (Seq.toList input)
