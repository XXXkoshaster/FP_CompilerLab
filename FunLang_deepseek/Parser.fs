module FunLang.Parser

open FParsec
open FunLang.AST

let ws = spaces
let str s = pstring s .>> ws

let parseNumber = pint32 .>> ws |>> Number

let parseBoolean =
    (stringReturn "true" (Boolean true) <|> stringReturn "false" (Boolean false)) .>> ws

let isOpChar c =
    List.contains c ['+';'-';'*';'/';'=';'<';'>';'?';'!';'&';'|';'~';'$';'%';'^';'@']

let isFirst c = System.Char.IsLetter c || c = '_' || isOpChar c
let isRest  c = System.Char.IsLetterOrDigit c || c = '_' || isOpChar c

let parseSymbolName =
    pipe2
        (satisfy isFirst)
        (many (satisfy isRest))
        (fun first rest -> string first + System.String(List.toArray rest))
    .>> ws

// Forward declarations для взаимной рекурсии
let parseExpr,        parseExprRef        = createParserForwardedToRef<Expr, unit>()
let parseListContent, parseListContentRef = createParserForwardedToRef<Expr, unit>()

let parseSymbolExpr = parseSymbolName |>> Symbol
let parseAtom       = parseNumber <|> parseBoolean <|> parseSymbolExpr
let parseList       = between (str "(") (str ")") parseListContent

// Подключаем parseList в parseExpr
do parseExprRef := parseAtom <|> parseList

// Special forms
let parseLambda =
    pstring "lambda" >>. ws
    >>. between (str "(") (str ")") (many parseSymbolName)
    .>>. parseExpr
    |>> (fun (pars, body) -> Lambda(pars, body))

let parseIf =
    pstring "if" >>. ws
    >>. parseExpr .>>. parseExpr .>>. parseExpr
    |>> fun ((cond, thenExpr), elseExpr) -> If(cond, thenExpr, elseExpr)

// many вместо sepBy ws — пробелы уже съедаются внутри каждого парсера
let parseLetBindings =
    between (str "(") (str ")")
        (many (between (str "(") (str ")") (parseSymbolName .>>. parseExpr)))

let parseLet =
    pstring "let" >>. ws >>. parseLetBindings .>>. parseExpr
    |>> (fun (bindings, body) -> Let(bindings, body))

let parseLetRec =
    pstring "letrec" >>. ws >>. parseLetBindings .>>. parseExpr
    |>> (fun (bindings, body) -> LetRec(bindings, body))

let parseApplication =
    pipe2 parseExpr (many parseExpr) (fun func args -> Apply(func, args))

do parseListContentRef :=
    attempt parseLambda  <|>
    attempt parseIf      <|>
    attempt parseLet     <|>
    attempt parseLetRec  <|>
    parseApplication

let parseProgram = ws >>. parseExpr .>> eof

let parse input =
    match run parseProgram input with
    | Success(result, _, _) -> Result.Ok result
    | Failure(error, _, _)  -> Result.Error error
