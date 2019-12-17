namespace FsGraphQL.Ast.Parsing

/// Stores information about a parser error.
type ParserError =
    { /// Gets the error message.
      Message : string
      /// Gets the position of the referenced error in the document.
      Position : Position }

/// Exception thrown when a parsing of a GraphQL document fails.
type ParserErrorException(error : ParserError) =
    inherit exn(error.Message)
    /// Gets the position where the error is found in the Document.
    member __.Position = error.Position

/// Stores the result of a parsing operation.
type ParserResult =
    /// Returned in case of a successful parsing. Contains the parsed document.
    | Success of Document
    /// Returned in case of a failure at parsing. Contains the parser error information.
    | Failure of ParserError