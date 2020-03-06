/// Parser of GraphQL documents.
module FsGraphQL.Ast.Parsing.Parser

open FParsec
open FsGraphQL.Ast.Parsing
open FsGraphQL.Ast.Parsing.Internal.DefinitionParsers

/// Tries to parse a GraphQL document from a string.
let tryParse query =
    let parseFailure (msg : string) (err : FParsec.Error.ParserError) =
        { Message = msg
          Position = { Line = err.Position.Line; Column = err.Position.Column } }
        |> ParserResult.Failure
    match run document query with
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