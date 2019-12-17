namespace FsGraphQL.Ast.Parsing

open System

/// Stores the position of a parsed item in a GraphQL document.
[<Struct; CustomEquality; CustomComparison>]
type Position =
    { /// Gets the line number of the position.
      Line : int64
      /// Gets the column number of the position.
      Column : int64 }
    override x.Equals(other) =
        match other with
        | :? Position as y -> x.Line = y.Line && x.Column = y.Column
        | _ -> false
    override x.GetHashCode() =
        x.Line.GetHashCode() ^^^ x.Column.GetHashCode()
    interface IComparable<Position> with
        member x.CompareTo(other) =
            if x.Line = other.Line
            then x.Column.CompareTo(other.Column)
            else x.Line.CompareTo(other.Line)
    interface IComparable with
        member x.CompareTo(other) =
            match other with
            | null -> 1
            | :? Position as y -> 
                let casted = x :> IComparable<Position>
                casted.CompareTo(y)
            | y -> 
                let xName = x.GetType().FullName
                let yName = y.GetType().FullName
                raise (ArgumentException(sprintf "Can not compare \"%s\" to \"%s\"." xName yName))