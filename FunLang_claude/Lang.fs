module Lang

type Expression =
    | Number of int
    | String of string
    | Symbol of string
    | List of Expression list
    | Function of (Expression list -> Expression)
    | Special of (Environment -> Expression list -> Expression)
    | Dummy of string
and Frame = Map<string, Expression ref> ref
and Environment = Frame list
