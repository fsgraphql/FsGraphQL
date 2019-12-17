namespace FsGraphQL.Ast

[<AutoOpen>]
module private Conversions =
    let rec convertValue (parsed : Parsing.Value) =
        match parsed.Value with
        | Parsing.Variable x -> Variable x
        | Parsing.IntValue x -> IntValue x
        | Parsing.FloatValue x -> FloatValue x
        | Parsing.StringValue x -> StringValue x
        | Parsing.BooleanValue x -> BooleanValue x
        | Parsing.NullValue -> NullValue
        | Parsing.EnumValue x -> EnumValue x
        | Parsing.ListValue x -> ListValue (List.map convertValue x)
        | Parsing.ObjectValue x -> ObjectValue (List.map convertObjectField x)

    and convertObjectField (parsed : Parsing.ObjectField) =
        { Name = parsed.Name
          Value = convertValue parsed.Value }

    let convertArgument (parsed : Parsing.Argument) =
        { Argument.Name = parsed.Name
          Value = convertValue parsed.Value }

    let convertDirective (parsed : Parsing.Directive) =
        { Name = parsed.Name
          Arguments = List.map convertArgument parsed.Arguments }

    let rec convertSelection =
        function
        | Parsing.Field x -> Field (convertField x)
        | Parsing.InlineFragment x -> InlineFragment (convertInlineFragment x)
        | Parsing.FragmentSpread x -> FragmentSpread (convertFragmentSpread x)

    and convertField (parsed : Parsing.Field) =
        { Alias = parsed.Alias
          Name = parsed.Name
          Arguments = List.map convertArgument parsed.Arguments
          Directives = List.map convertDirective parsed.Directives
          SelectionSet = convertSelectionSetOption parsed.SelectionSet }

    and convertSelectionSet (parsed : Parsing.SelectionSet) =
        List.map convertSelection parsed.Selections

    and convertSelectionSetOption (parsed : Parsing.SelectionSet option) =
        parsed
        |> Option.map convertSelectionSet
        |> Option.defaultValue []

    and convertInlineFragment (parsed : Parsing.InlineFragment) =
        { TypeCondition = parsed.TypeCondition
          Directives = List.map convertDirective parsed.Directives
          SelectionSet = convertSelectionSet parsed.SelectionSet }

    and convertFragmentSpread (parsed : Parsing.FragmentSpread) =
        { Name = parsed.Name
          Directives = List.map convertDirective parsed.Directives }

    let rec convertType (parsed : Parsing.Type) =
        match parsed.Type with
        | Parsing.NamedType x -> NamedType x
        | Parsing.ListType x -> ListType (convertType x)
        | Parsing.NonNullType x -> NonNullType (convertType x)

    let convertVariableDefinition (parsed : Parsing.VariableDefinition) =
        { Name = parsed.Name
          Type = convertType parsed.Type
          DefaultValue = Option.map convertValue parsed.DefaultValue }

    let convertOperation (parsed : Parsing.Operation) =
        { OperationType = parsed.OperationType
          Name = parsed.Name
          VariableDefinitions = List.map convertVariableDefinition parsed.VariableDefinitions
          Directives = List.map convertDirective parsed.Directives
          SelectionSet = convertSelectionSet parsed.SelectionSet }

    let convertOperationDefinition =
        function
        | Parsing.QueryShorthand x -> QueryShorthand (convertSelectionSet x)
        | Parsing.Operation x -> Operation (convertOperation x)

    let convertFragmentDefinition (parsed : Parsing.FragmentDefinition) =
        { Name = parsed.Name
          TypeCondition = parsed.TypeCondition
          Directives = List.map convertDirective parsed.Directives
          SelectionSet = convertSelectionSet parsed.SelectionSet }

    let convertExecutableDefinition =
        function
        | Parsing.FragmentDefinition x -> FragmentDefinition (convertFragmentDefinition x)
        | Parsing.OperationDefinition x -> OperationDefinition (convertOperationDefinition x)

    let convertOperationTypeDefinition (parsed : Parsing.OperationTypeDefinition) =
        { OperationType = parsed.OperationType
          Name = parsed.Name }

    let convertSchemaDefinition (parsed : Parsing.SchemaDefinition) =
        { Directives = List.map convertDirective parsed.Directives
          Operations = List.map convertOperationTypeDefinition parsed.Operations }

    let convertInputValueDefinition (parsed : Parsing.InputValueDefinition) =
        { Description = parsed.Description
          Name = parsed.Name
          Type = convertType parsed.Type
          DefaultValue = convertValue parsed.DefaultValue
          Directives = List.map convertDirective parsed.Directives }

    let convertFieldDefinition (parsed : Parsing.FieldDefinition) =
        { Description = parsed.Description
          Name = parsed.Name
          Arguments = List.map convertInputValueDefinition parsed.Arguments
          Type = convertType parsed.Type
          Directives = List.map convertDirective parsed.Directives }

    let convertInputObjectTypeDefinition (parsed : Parsing.InputObjectTypeDefinition) =
        { Description = parsed.Description
          Name = parsed.Name
          Directives = List.map convertDirective parsed.Directives
          InputFields = List.map convertInputValueDefinition parsed.InputFields }

    let convertEnumValueDefinition (parsed : Parsing.EnumValueDefinition) =
        { Description = parsed.Description
          EnumValue = parsed.EnumValue
          Directives = List.map convertDirective parsed.Directives }

    let convertEnumTypeDefinition (parsed : Parsing.EnumTypeDefinition) =
        { Description = parsed.Description
          Name = parsed.Name
          Directives = List.map convertDirective parsed.Directives
          EnumValues = List.map convertEnumValueDefinition parsed.EnumValues }

    let convertUnionTypeDefinition (parsed : Parsing.UnionTypeDefinition) =
        { Description = parsed.Description
          Name = parsed.Name
          Directives = List.map convertDirective parsed.Directives
          UnionMemberTypes = parsed.UnionMemberTypes }

    let convertInterfaceTypeDefinition (parsed: Parsing.InterfaceTypeDefinition) =
        { Description = parsed.Description
          Name = parsed.Name
          Directives = List.map convertDirective parsed.Directives
          Fields = List.map convertFieldDefinition parsed.Fields }

    let convertObjectTypeDefinition (parsed : Parsing.ObjectTypeDefinition) =
        { Description = parsed.Description
          Name = parsed.Name
          Interfaces = parsed.Interfaces
          Directives = List.map convertDirective parsed.Directives
          Fields = List.map convertFieldDefinition parsed.Fields }

    let convertScalarTypeDefinition (parsed : Parsing.ScalarTypeDefinition) =
        { Description = parsed.Description
          Name = parsed.Name
          Directives = List.map convertDirective parsed.Directives }

    let convertTypeDefinition =
        function
        | Parsing.ScalarTypeDefinition x -> ScalarTypeDefinition (convertScalarTypeDefinition x)
        | Parsing.ObjectTypeDefinition x -> ObjectTypeDefinition (convertObjectTypeDefinition x)
        | Parsing.InterfaceTypeDefinition x -> InterfaceTypeDefinition (convertInterfaceTypeDefinition x)
        | Parsing.UnionTypeDefinition x -> UnionTypeDefinition (convertUnionTypeDefinition x)
        | Parsing.EnumTypeDefinition x -> EnumTypeDefinition (convertEnumTypeDefinition x)
        | Parsing.InputObjectTypeDefinition x -> InputObjectTypeDefinition (convertInputObjectTypeDefinition x)

    let convertInputObjectTypeExtension (parsed : Parsing.InputObjectTypeExtension) =
        { Name = parsed.Name
          Directives = List.map convertDirective parsed.Directives
          InputFields = List.map convertInputValueDefinition parsed.InputFields }

    let convertEnumTypeExtension (parsed : Parsing.EnumTypeExtension) =
        { Name = parsed.Name
          Directives = List.map convertDirective parsed.Directives
          EnumValues = List.map convertEnumValueDefinition parsed.EnumValues }

    let convertUnionTypeExtension (parsed : Parsing.UnionTypeExtension) =
        { Name = parsed.Name
          Directives = List.map convertDirective parsed.Directives
          UnionMemberTypes = parsed.UnionMemberTypes }

    let convertInterfaceTypeExtension (parsed : Parsing.InterfaceTypeExtension) =
        { Name = parsed.Name
          Directives = List.map convertDirective parsed.Directives
          Fields = List.map convertFieldDefinition parsed.Fields }

    let convertObjectTypeExtension (parsed : Parsing.ObjectTypeExtension) =
        { Name = parsed.Name
          Interfaces = parsed.Interfaces
          Directives = List.map convertDirective parsed.Directives
          Fields = List.map convertFieldDefinition parsed.Fields }

    let convertScalarTypeExtension (parsed : Parsing.ScalarTypeExtension) =
        { ScalarTypeExtension.Name = parsed.Name
          Directives = List.map convertDirective parsed.Directives }

    let convertTypeExtension =
        function
        | Parsing.ScalarTypeExtension x -> ScalarTypeExtension (convertScalarTypeExtension x)
        | Parsing.ObjectTypeExtension x -> ObjectTypeExtension (convertObjectTypeExtension x)
        | Parsing.InterfaceTypeExtension x -> InterfaceTypeExtension (convertInterfaceTypeExtension x)
        | Parsing.UnionTypeExtension x -> UnionTypeExtension (convertUnionTypeExtension x)
        | Parsing.EnumTypeExtension x -> EnumTypeExtension (convertEnumTypeExtension x)
        | Parsing.InputObjectTypeExtension x -> InputObjectTypeExtension (convertInputObjectTypeExtension x)

    let convertSchemaExtension (parsed : Parsing.SchemaExtension) =
        { SchemaExtension.Directives = List.map convertDirective parsed.Directives
          Operations = List.map convertOperationTypeDefinition parsed.Operations }

    let convertTypeSystemExtension (parsed : Parsing.TypeSystemExtension) =
        match parsed.Value with
        | Parsing.SchemaExtension x -> SchemaExtension (convertSchemaExtension x)
        | Parsing.TypeExtension x -> TypeExtension (convertTypeExtension x)

    let convertDirectiveLocation =
        function
        | Parsing.ExecutableDirectiveLocation x -> ExecutableDirectiveLocation x.Value
        | Parsing.TypeSystemDirectiveLocation x -> TypeSystemDirectiveLocation x.Value

    let convertDirectiveDefinition (parsed : Parsing.DirectiveDefinition) =
        { Description = parsed.Description
          Name = parsed.Name
          Arguments = List.map convertInputValueDefinition parsed.Arguments
          DirectiveLocations = List.map convertDirectiveLocation parsed.DirectiveLocations }

    let convertTypeSystemDefinition =
        function
        | Parsing.SchemaDefinition x -> SchemaDefinition (convertSchemaDefinition x)
        | Parsing.TypeDefinition x -> TypeDefinition (convertTypeDefinition x)
        | Parsing.DirectiveDefinition x -> DirectiveDefinition (convertDirectiveDefinition x)

    let convertDefinition =
        function
        | Parsing.ExecutableDefinition x -> ExecutableDefinition (convertExecutableDefinition x)
        | Parsing.TypeSystemDefinition x -> TypeSystemDefinition (convertTypeSystemDefinition x)
        | Parsing.TypeSystemExtension x -> TypeSystemExtension (convertTypeSystemExtension x)

    let convertDocument (parsed : Parsing.Document) =
        { Definitions = List.map convertDefinition parsed.Definitions }

[<AutoOpen>]
module Extensions =
    type Value with
        static member OfParsed = convertValue

    type ObjectField with
        static member OfParsed = convertObjectField

    type Argument with
        static member OfParsed = convertArgument

    type Directive with
        static member OfParsed = convertDirective

    type FragmentSpread with
        static member OfParsed = convertFragmentSpread

    type InlineFragment with
        static member OfParsed = convertInlineFragment

    type Field with
        static member OfParsed = convertField

    type Selection with
        static member OfParsed = convertSelection

    type Type with
        static member OfParsed = convertType

    type VariableDefinition with
        static member OfParsed = convertVariableDefinition

    type Operation with
        static member OfParsed = convertOperation

    type OperationDefinition with
        static member OfParsed = convertOperationDefinition

    type FragmentDefinition with
        static member OfParsed = convertFragmentDefinition

    type ExecutableDefinition with
        static member OfParsed = convertExecutableDefinition

    type OperationTypeDefinition with
        static member OfParsed = convertOperationTypeDefinition

    type SchemaDefinition with
        static member OfParsed = convertSchemaDefinition

    type InputValueDefinition with
        static member OfParsed = convertInputValueDefinition

    type FieldDefinition with
        static member OfParsed = convertFieldDefinition

    type InputObjectTypeDefinition with
        static member OfParsed = convertInputObjectTypeDefinition

    type EnumValueDefinition with
        static member OfParsed = convertEnumValueDefinition

    type EnumTypeDefinition with
        static member OfParsed = convertEnumTypeDefinition

    type UnionTypeDefinition with
        static member OfParsed = convertUnionTypeDefinition

    type InterfaceTypeDefinition with
        static member OfParsed = convertInterfaceTypeDefinition

    type ObjectTypeDefinition with
        static member OfParsed = convertObjectTypeDefinition

    type ScalarTypeDefinition with
        static member OfParsed = convertScalarTypeDefinition

    type TypeDefinition with
        static member OfParsed = convertTypeDefinition

    type InputObjectTypeExtension with
        static member OfParsed = convertInputObjectTypeExtension

    type EnumTypeExtension with
        static member OfParsed = convertEnumTypeExtension

    type UnionTypeExtension with
        static member OfParsed = convertUnionTypeExtension

    type InterfaceTypeExtension with
        static member OfParsed = convertInterfaceTypeExtension

    type ObjectTypeExtension with
        static member OfParsed = convertObjectTypeExtension

    type ScalarTypeExtension with
        static member OfParsed = convertScalarTypeExtension

    type TypeExtension with
        static member OfParsed = convertTypeExtension

    type SchemaExtension with
        static member OfParsed = convertSchemaExtension

    type TypeSystemExtension with
        static member OfParsed = convertTypeSystemExtension

    type DirectiveLocation with
        static member OfParsed = convertDirectiveLocation

    type DirectiveDefinition with
        static member OfParsed = convertDirectiveDefinition

    type TypeSystemDefinition with
        static member OfParsed = convertTypeSystemDefinition

    type Definition with
        static member OfParsed = convertDefinition

    type Document with
        static member OfParsed = convertDocument