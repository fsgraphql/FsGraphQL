/// Parser of GraphQL documents.
module FsGraphQL.Ast.Parsing.Parser

open FParsec
open FsGraphQL.Ast.Parsing

module internal Internal =
    let (>.>) (p : Parser<'a, 'u>) (mapper : 'a -> Position -> 'b) : Parser<'b, 'u> =
        fun stream ->
            let positionReply = getPosition stream
            if positionReply.Status = Ok then
                let pReply = p stream
                let position = { Line = positionReply.Result.Line; Column = positionReply.Result.Column }
                Reply(pReply.Status,
                        (if pReply.Status = Ok then mapper pReply.Result position else Unchecked.defaultof<_>),
                        pReply.Error)
            else
                Reply(positionReply.Status, positionReply.Error)

    let ignored = 
        let whiteSpace = skipAnyOf [| '\u0009'; '\u000B'; '\u000C'; '\u0020'; '\u00A0' |]
        let lineTerminators = skipAnyOf [| '\u000A'; '\u000D'; '\u2028'; '\u2029' |]
        let comments  = pchar '#' >>. skipManyTill anyChar (lineTerminators <|>  eof)
        let comma = skipChar ',' 
        whiteSpace <|> lineTerminators <|> comments <|> comma <?> "an ignored character"

    let whiteSpaces = many ignored |>> ignore

    let token p = p .>> notFollowedBy (letter <|> digit <|> pchar '_')

    let stringToken = pstring >> token
    
    let tokenWhiteSpaces p = token p .>> whiteSpaces
    
    let stringTokenWhiteSpaces = pstring >> tokenWhiteSpaces
    
    let charTokenWhiteSpaces = pchar >> tokenWhiteSpaces

    let someOrEmpty = function | Some lst -> lst | None -> []

    let charsToString = Array.ofList >> string

    let betweenChars left right p =
        (pchar left .>> whiteSpaces) >>. p .>> (whiteSpaces .>> pchar right)

    let betweenCharsMany1 left right p =
        between (pchar left .>> whiteSpaces) (pchar right) (sepEndBy1 p whiteSpaces)

    let betweenCharsMany left right p =
        between (pchar left .>> whiteSpaces) (pchar right) (sepEndBy p whiteSpaces)

    let pairBetween seperator key value =
        (key .>> whiteSpaces) .>>. ((pchar seperator .>> whiteSpaces) >>. value)

    let inputValue, inputValueRef = createParserForwardedToRef ()

    let name = 
        let isIdentifierFirstChar c = isAsciiLetter c || c = '_'
        let isIdentifierChar c = isAsciiLetter c || isDigit c || c = '_'
        many1Satisfy2 isIdentifierFirstChar isIdentifierChar

    let stringValue =
        let escapedCharacter =
            let escaped = 
                anyOf [| '"'; '\\'; '/'; 'b'; 'f'; 'n'; 'r'; 't' |]
                |>> function | 'b' -> '\b' | 'f' -> '\u000C' | 'n' -> '\n' 
                                | 'r' -> '\r' | 't' -> '\t' | c -> c 
            let unicode = 
                pchar 'u' >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                    let hex2int c = (int c &&& 15) + (int c >>> 6)*9 
                    (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0 |> char)
            pchar '\\' >>. (escaped <|> unicode)
      
        let normalCharacter = noneOf [| '\u000A';'\u000D';'\u2028';'\u2029';'"';'\'' |]
        let quote =  pchar '"'
        between quote quote (manyChars (normalCharacter <|> escapedCharacter))

    let booleanValue =
        choice [ stringToken "true" >>% true
                 stringToken "false" >>% false ]

    let integerPart = 
        let negativeSign = pchar '-'
        let nonZeroDigit = anyOf [| '1';'2';'3';'4';'5';'6';'7';'8';'9' |]
        let zero = pchar '0'
        let zeroInteger = opt negativeSign >>. zero >>% "0"
        let nonZeroInteger = 
            opt negativeSign .>>. (many1Chars2 nonZeroDigit digit) 
            |>> function | (Some _, v) -> "-" + v | (None, v) -> v
        (attempt zeroInteger) <|> nonZeroInteger

    let integerValue = integerPart |>> int64

    let floatValue =
        let exponentPart = 
            let sign = pchar '+' <|> pchar '-'
            let exponentIndicator = pchar 'e' <|> pchar 'E'
            pipe3 exponentIndicator (opt sign) (many1 digit) 
                (fun exp sign digits ->
                    charsToString (match sign with
                                    | Some sign -> exp :: sign :: digits
                                    | None -> exp :: digits))
        let fractionPart = 
            pchar '.' .>>. (many1 digit) 
            |>> (fun (dot, digits) -> charsToString(dot::digits))
        choice [ integerPart .>>. (exponentPart <|> fractionPart) |>> fun(p1, p2)-> p1 + p2
                 pipe3 integerPart fractionPart exponentPart
                (fun integer fraction exponent -> integer + fraction + exponent ) ] |>> float

    let nullValue = stringToken "null" >.> (fun _ position -> { Value = NullValue; Position = position })

    let enumValue = name

    let variable = pchar '$' >>. name 

    let objectValue =
        betweenCharsMany '{' '}' (pairBetween ':' name inputValue >.> (fun (name, value) position -> name, value, position) <?> "an object field")
        |>> (List.map (fun (name, value, position) -> { ObjectField.Name = name; Value = value; Position = position }))

    let listValue =
        betweenCharsMany '[' ']' (tokenWhiteSpaces inputValue <?> "a value") 

    inputValueRef :=
        choice [ variable >.> (fun v position -> { Value = Variable v; Position = position }) <?> "a variable"
                 (attempt floatValue) >.> (fun v position -> { Value = FloatValue v; Position = position }) <?> "a float value"
                 integerValue >.> (fun v position -> { Value = IntValue v; Position = position }) <?> "an integer value"
                 stringValue >.> (fun v position -> { Value = StringValue v; Position = position }) <?> "a string value"
                 (attempt booleanValue) >.> (fun v position -> { Value = BooleanValue v; Position = position }) <?> "a boolean value"
                 nullValue
                 enumValue >.> (fun v position -> { Value = EnumValue v; Position = position }) <?> "an enum value"
                 objectValue >.> (fun v position -> { Value = ObjectValue v; Position = position })  <?> "an object value"
                 listValue >.> (fun v position -> { Value = ListValue v; Position = position }) <?> "a list value" ]  

    let arguments = 
        let argument = 
            (pairBetween ':' name inputValue >.> (fun (name, value) position -> name, value, position))
            |>> fun (name, value, position) -> { Argument.Name = name; Value = value; Position = position } 
            <?> "an argument"
        betweenCharsMany '(' ')' argument <?> "one or more arguments"

    let directives = 
        let directive =
            pchar '@' >>. (name .>> whiteSpaces) .>>. (opt arguments) 
            >.> fun (name, args) position -> { Directive.Name = name; Arguments = someOrEmpty args; Position = position }
            <?> "a directive"
        sepEndBy directive whiteSpaces <?> "one or more directives"

    let inputType, inputTypeRef = createParserForwardedToRef()
    
    let namedType = name >.> (fun name position -> { Type = NamedType name; Position = position }) <?> "a named type"
    
    let listType = 
        betweenChars '[' ']' inputType
        >.> (fun t position -> { Type = ListType t; Position = position }) <?> "a list type"
    
    let nonNullType = 
        (listType <|> namedType) .>> pchar '!' 
        >.> (fun t position -> { Type = NonNullType t; Position = position }) <?> "an non-null type"
    
    inputTypeRef := choice [ attempt nonNullType; namedType; listType ]

    let selection, selectionRef = createParserForwardedToRef()

    let selectionSet, selectionSetRef = createParserForwardedToRef()

    let field = 
        let alias = opt (attempt (tokenWhiteSpaces name .>> pchar ':' .>> whiteSpaces))
        let name = (tokenWhiteSpaces name) >.> (fun name position -> name, position)
        let args = opt (tokenWhiteSpaces arguments)
        let directives = opt directives
        let selectionSet = opt selectionSet
        pipe5 alias name args directives selectionSet
            (fun oalias (name, position) oargs directives oselection ->
                (Field { Alias = oalias; Name = name; Arguments = someOrEmpty oargs; Position = position;
                         Directives = someOrEmpty directives; SelectionSet = oselection }))
        <?> "a field"

    let selectionFragment =
        let inlineFragment =
            pipe4 getPosition (opt(stringTokenWhiteSpaces "on" >>. tokenWhiteSpaces name)) (opt(tokenWhiteSpaces directives)) selectionSet
              (fun position typeCondition directives selectionSet -> 
                { InlineFragment.Directives = someOrEmpty directives
                  SelectionSet = selectionSet 
                  TypeCondition = typeCondition
                  Position = { Line = position.Line; Column = position.Column } })
            |>> InlineFragment <?> "an inline fragment"
        let fragmentSpread = 
            pipe3 getPosition (tokenWhiteSpaces name) (opt directives)
                (fun position name directives -> 
                    { FragmentSpread.Name = name
                      Directives = someOrEmpty directives
                      Position = { Line = position.Line; Column = position.Column } })
            |>> FragmentSpread <?> "a fragment spread"
        pstring "..." .>> whiteSpaces >>. (inlineFragment <|> fragmentSpread)  <?> "a fragment"

    selectionRef := field <|> selectionFragment  <?> "a selection"

    selectionSetRef := betweenCharsMany1 '{' '}' selection >.> (fun s position -> { Selections = s; Position = position }) <?> "a selection set"

    let executableDefinitions =
        let operationType =
            (stringTokenWhiteSpaces "query" >>% FsGraphQL.Ast.OperationType.Query)
            <|> (stringTokenWhiteSpaces "mutation" >>% FsGraphQL.Ast.OperationType.Mutation)
            <|> (stringTokenWhiteSpaces "subscription" >>% FsGraphQL.Ast.OperationType.Subscription)
        let operationDefinition = 
            let variableDefinition =
                let positionNameAndType = pairBetween ':' variable inputType >.> (fun (name, t) position -> name, t, position)
                let defaultValue = whiteSpaces >>. opt ((charTokenWhiteSpaces '=') >>. inputValue)
                pipe2 positionNameAndType defaultValue
                    (fun (variableName, variableType, position) defaultValue ->
                        { VariableDefinition.Name = variableName; Type = variableType
                          DefaultValue = defaultValue; Position = position })
            let variableDefinitions = opt (tokenWhiteSpaces (betweenCharsMany '(' ')' variableDefinition))
            let nameAndPosition = opt (tokenWhiteSpaces name) >.> (fun name position -> name, position)
            let directives = opt (tokenWhiteSpaces directives)
            let selectionSet = tokenWhiteSpaces selectionSet
            pipe5 operationType nameAndPosition variableDefinitions directives selectionSet
                (fun otype (name, position) ovars directives selection ->
                    { OperationType = otype; Name = name; SelectionSet = selection; Position = position
                      VariableDefinitions = someOrEmpty ovars; Directives = someOrEmpty directives })
            |>> Operation
        let shortHandQueryDefinition = selectionSet |>> QueryShorthand
        let operationDefinition = shortHandQueryDefinition <|> operationDefinition |>> OperationDefinition
        let fragmentDefinition =
            let nameAndPosition = (stringTokenWhiteSpaces "fragment") >>. (tokenWhiteSpaces name >.> (fun name position -> name, position)) .>> (stringTokenWhiteSpaces "on")
            let typeCondition = tokenWhiteSpaces name
            pipe4 nameAndPosition typeCondition directives selectionSet
                (fun (name, position) typeCondition directives selectionSet ->
                    { Name = name;  Directives = directives; SelectionSet = selectionSet
                      TypeCondition = typeCondition; Position = position })
            |>> FragmentDefinition
        sepEndBy (operationDefinition <|> fragmentDefinition) whiteSpaces

    let definitions =
        executableDefinitions |>> (List.map ExecutableDefinition)

    let document = 
        whiteSpaces >>. definitions .>> (skipMany ignored <|> eof)
        |>> (fun definitions -> { Document.Definitions = definitions })

/// Tries to parse a GraphQL document from a string.
let tryParse query =
    let parseFailure (msg : string) (err : FParsec.Error.ParserError) =
        { Message = msg
          Position = { Line = err.Position.Line; Column = err.Position.Column } }
        |> ParserResult.Failure
    match run Internal.document query with
    | CharParsers.ParserResult.Success (document, _, _) -> Success document
    | CharParsers.ParserResult.Failure (msg, err, _) -> parseFailure msg err

/// <summary>
/// Parses a GraphQL document from a string.
/// </summary>
/// <exception cref="GraphQLSharp.Ast.ParserErrorException">Thrown when the parser finds a error in the document.</exception>
let parse query =
    match tryParse query with
    | Success document -> document
    | Failure error -> raise (ParserErrorException(error))