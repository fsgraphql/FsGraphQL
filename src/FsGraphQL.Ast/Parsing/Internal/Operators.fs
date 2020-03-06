module internal FsGraphQL.Ast.Parsing.Internal.Operators

open FParsec
open FsGraphQL.Ast.Parsing

let (>.>) (p : Parser<'a, 'u>) (mapper : 'a -> FsGraphQL.Ast.Parsing.Position -> 'b) : Parser<'b, 'u> =
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