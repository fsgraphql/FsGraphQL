namespace FsGraphQL.Ast.Parsing

type Document =
    { Definitions : Definition list }

and Definition =
    | ExecutableDefinition of ExecutableDefinition
    | TypeSystemDefinition of TypeSystemDefinition
    | TypeSystemExtension of TypeSystemExtension
    member x.Position =
        match x with
        | ExecutableDefinition x -> x.Position
        | TypeSystemDefinition x -> x.Position
        | TypeSystemExtension x -> x.Position

and TypeSystemDefinition =
    | SchemaDefinition of SchemaDefinition
    | TypeDefinition of TypeDefinition
    | DirectiveDefinition of DirectiveDefinition
    member x.Position =
        match x with
        | SchemaDefinition x -> x.Position
        | TypeDefinition x -> x.Position
        | DirectiveDefinition x -> x.Position

and DirectiveDefinition =
    { Description : string
      Name : string
      Arguments : InputValueDefinition list
      DirectiveLocations : DirectiveLocation list
      Position : Position }

and DirectiveLocation =
    | ExecutableDirectiveLocation of ExecutableDirectiveLocation
    | TypeSystemDirectiveLocation of TypeSystemDirectiveLocation
    member x.Position =
        match x with
        | ExecutableDirectiveLocation x -> x.Position
        | TypeSystemDirectiveLocation x -> x.Position

and ExecutableDirectiveLocation =
    { Value : FsGraphQL.Ast.ExecutableDirectiveLocation
      Position : Position }

and TypeSystemDirectiveLocation =
    { Value : FsGraphQL.Ast.TypeSystemDirectiveLocation
      Position : Position }

and TypeSystemExtension =
    { Value : TypeSystemExtensionValue
      Position : Position }

and TypeSystemExtensionValue =
    | SchemaExtension of SchemaExtension
    | TypeExtension of TypeExtension
    member x.Position =
        match x with
        | SchemaExtension x -> x.Position
        | TypeExtension x -> x.Position

and SchemaExtension =
    { Directives : Directive list
      Operations : OperationTypeDefinition list
      Position : Position }

and TypeExtension =
    | ScalarTypeExtension of ScalarTypeExtension
    | ObjectTypeExtension of ObjectTypeExtension
    | InterfaceTypeExtension of InterfaceTypeExtension
    | UnionTypeExtension of UnionTypeExtension
    | EnumTypeExtension of EnumTypeExtension
    | InputObjectTypeExtension of InputObjectTypeExtension
    member x.Name =
        match x with
        | ScalarTypeExtension x -> x.Name
        | ObjectTypeExtension x -> x.Name
        | InterfaceTypeExtension x -> x.Name
        | UnionTypeExtension x -> x.Name
        | EnumTypeExtension x -> x.Name
        | InputObjectTypeExtension x -> x.Name
    member x.Directives =
        match x with
        | ScalarTypeExtension x -> x.Directives
        | ObjectTypeExtension x -> x.Directives
        | InterfaceTypeExtension x -> x.Directives
        | UnionTypeExtension x -> x.Directives
        | EnumTypeExtension x -> x.Directives
        | InputObjectTypeExtension x -> x.Directives
    member x.Position =
        match x with
        | ScalarTypeExtension x -> x.Position
        | ObjectTypeExtension x -> x.Position
        | InterfaceTypeExtension x -> x.Position
        | UnionTypeExtension x -> x.Position
        | EnumTypeExtension x -> x.Position
        | InputObjectTypeExtension x -> x.Position

and ScalarTypeExtension =
    { Name : string
      Directives : Directive list
      Position : Position }

and ObjectTypeExtension =
    { Name : string
      Interfaces : string list
      Directives : Directive list
      Fields : FieldDefinition list
      Position : Position }

and InterfaceTypeExtension =
    { Name : string
      Directives : Directive list
      Fields : FieldDefinition list
      Position : Position }

and UnionTypeExtension =
    { Name : string
      Directives : Directive list
      UnionMemberTypes : string list
      Position : Position }

and EnumTypeExtension =
    { Name : string
      Directives : Directive list
      EnumValues : EnumValueDefinition list
      Position : Position }

and InputObjectTypeExtension =
    { Name : string
      Directives : Directive list
      InputFields : InputValueDefinition list
      Position : Position }

and TypeDefinition =
    | ScalarTypeDefinition of ScalarTypeDefinition
    | ObjectTypeDefinition of ObjectTypeDefinition
    | InterfaceTypeDefinition of InterfaceTypeDefinition
    | UnionTypeDefinition of UnionTypeDefinition
    | EnumTypeDefinition of EnumTypeDefinition
    | InputObjectTypeDefinition of InputObjectTypeDefinition
    member x.Name =
        match x with
        | ScalarTypeDefinition x -> x.Name
        | ObjectTypeDefinition x -> x.Name
        | InterfaceTypeDefinition x -> x.Name
        | UnionTypeDefinition x -> x.Name
        | EnumTypeDefinition x -> x.Name
        | InputObjectTypeDefinition x -> x.Name
    member x.Directives =
        match x with
        | ScalarTypeDefinition x -> x.Directives
        | ObjectTypeDefinition x -> x.Directives
        | InterfaceTypeDefinition x -> x.Directives
        | UnionTypeDefinition x -> x.Directives
        | EnumTypeDefinition x -> x.Directives
        | InputObjectTypeDefinition x -> x.Directives
    member x.Description =
        match x with
        | ScalarTypeDefinition x -> x.Description
        | ObjectTypeDefinition x -> x.Description
        | InterfaceTypeDefinition x -> x.Description
        | UnionTypeDefinition x -> x.Description
        | EnumTypeDefinition x -> x.Description
        | InputObjectTypeDefinition x -> x.Description
    member x.Position =
        match x with
        | ScalarTypeDefinition x -> x.Position
        | ObjectTypeDefinition x -> x.Position
        | InterfaceTypeDefinition x -> x.Position
        | UnionTypeDefinition x -> x.Position
        | EnumTypeDefinition x -> x.Position
        | InputObjectTypeDefinition x -> x.Position

and ScalarTypeDefinition =
    { Description : string
      Name : string
      Directives : Directive list
      Position : Position }

and ObjectTypeDefinition =
    { Description : string
      Name : string
      Interfaces : string list
      Directives : Directive list
      Fields : FieldDefinition list
      Position : Position }

and InterfaceTypeDefinition =
    { Description : string
      Name : string
      Directives : Directive list
      Fields : FieldDefinition list
      Position : Position }

and UnionTypeDefinition =
    { Description : string
      Name : string 
      Directives : Directive list
      UnionMemberTypes : string list
      Position : Position }

and EnumTypeDefinition =
    { Description : string
      Name : string
      Directives : Directive list
      EnumValues : EnumValueDefinition list
      Position : Position }

and EnumValueDefinition =
    { Description : string
      EnumValue : string
      Directives : Directive list
      Position : Position }

and InputObjectTypeDefinition =
    { Description : string
      Name : string
      Directives : Directive list
      InputFields : InputValueDefinition list
      Position : Position }

and FieldDefinition =
    { Description : string
      Name : string
      Arguments : InputValueDefinition list
      Type : Type
      Directives : Directive list
      Position : Position }

and InputValueDefinition =
    { Description : string
      Name : string
      Type : Type
      DefaultValue : Value
      Directives : Directive list
      Position : Position }

and SchemaDefinition =
    { Directives : Directive list
      Operations : OperationTypeDefinition list
      Position : Position }

and OperationTypeDefinition =
    { OperationType : FsGraphQL.Ast.OperationType
      Name : string
      Position : Position }

and ExecutableDefinition =
    | OperationDefinition of OperationDefinition
    | FragmentDefinition of FragmentDefinition
    member x.Position =
        match x with
        | OperationDefinition x -> x.Position
        | FragmentDefinition x -> x.Position

and SelectionSet =
    { Selections : Selection list
      Position : Position }

and FragmentDefinition =
    { Name : string
      TypeCondition : string
      Directives : Directive list 
      SelectionSet : SelectionSet
      Position : Position }
    
and OperationDefinition =
    | QueryShorthand of SelectionSet
    | Operation of Operation
    member x.Position =
        match x with
        | QueryShorthand x -> x.Position
        | Operation x -> x.Position

and Operation =
    { OperationType : FsGraphQL.Ast.OperationType
      Name : string option
      VariableDefinitions : VariableDefinition list
      Directives : Directive list
      SelectionSet : SelectionSet
      Position : Position }

and Directive =
    { Name : string
      Arguments : Argument list
      Position : Position }

and Argument =
    { Name : string
      Value : Value
      Position : Position }

and ObjectField =
    { Name : string
      Value : Value
      Position : Position }

and Value =
    { Value : ValueContent
      Position : Position }

and ValueContent =
    | Variable of variableName : string
    | IntValue of int64
    | FloatValue of float
    | StringValue of string
    | BooleanValue of bool
    | NullValue
    | EnumValue of string
    | ListValue of Value list
    | ObjectValue of ObjectField list

and VariableDefinition =
    { Name : string
      Type : Type
      DefaultValue : Value option
      Position : Position }

and Type =
    { Type : TypeValue
      Position : Position }

and TypeValue =
    | NamedType of name : string
    | ListType of Type
    | NonNullType of Type

and Selection =
    | Field of Field
    | FragmentSpread of FragmentSpread
    | InlineFragment of InlineFragment
    member x.Position =
        match x with
        | Field x -> x.Position
        | FragmentSpread x -> x.Position
        | InlineFragment x -> x.Position

and FragmentSpread =
    { Name : string
      Directives : Directive list
      Position : Position }

and InlineFragment =
    { TypeCondition : string option
      Directives : Directive list
      SelectionSet : SelectionSet
      Position : Position }

and Field =
    { Alias : string option
      Name : string
      Arguments : Argument list
      Directives : Directive list
      SelectionSet : SelectionSet option
      Position : Position }
    member x.AliasOrName =
        Option.defaultValue x.Name x.Alias