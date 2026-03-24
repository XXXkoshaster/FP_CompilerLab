module FunLang.AST

type Expr =
    | Number of int
    | Boolean of bool
    | Symbol of string
    | Lambda of string list * Expr
    | Apply of Expr * Expr list
    | If of Expr * Expr * Expr
    | Let of (string * Expr) list * Expr
    | LetRec of (string * Expr) list * Expr