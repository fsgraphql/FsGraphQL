module internal FsGraphQL.Ast.Parsing.Internal.BaseParsers

open FParsec

let ignored : Parser<unit, unit> = 
    let whiteSpace = skipAnyOf [| '\u0009'; '\u000B'; '\u000C'; '\u0020'; '\u00A0' |]
    let lineTerminators = skipAnyOf [| '\u000A'; '\u000D'; '\u2028'; '\u2029' |]
    let comments  = pchar '#' >>. skipManyTill anyChar (lineTerminators <|>  eof)
    let comma = skipChar ',' 
    whiteSpace <|> lineTerminators <|> comments <|> comma <?> "an ignored character"

let whiteSpaces = many ignored |>> ignore

let token p = p .>> notFollowedBy (letter <|> digit <|> pchar '_')

let stringToken : string -> Parser<string, unit> = pstring >> token
    
let tokenWhiteSpaces p = token p .>> whiteSpaces
    
let stringTokenWhiteSpaces = pstring >> tokenWhiteSpaces
    
let charTokenWhiteSpaces = pchar >> tokenWhiteSpaces

let someOrEmpty = function | Some lst -> lst | None -> []

let charsToString : char list -> string = Array.ofList >> string

let betweenChars left right p =
    (pchar left .>> whiteSpaces) >>. p .>> (whiteSpaces .>> pchar right)

let betweenCharsMany1 left right p =
    between (pchar left .>> whiteSpaces) (pchar right) (sepEndBy1 p whiteSpaces)

let betweenCharsMany left right p =
    between (pchar left .>> whiteSpaces) (pchar right) (sepEndBy p whiteSpaces)

let pairBetween seperator key value =
    (key .>> whiteSpaces) .>>. ((pchar seperator .>> whiteSpaces) >>. value)