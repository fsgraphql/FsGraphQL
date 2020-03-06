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

/// Contains extensions for GraphQL base types.
[<AutoOpen>]
module Extensions =
    type Value with
        /// Gets a Value type from a parsed Value type.
        static member OfParsed = convertValue

    type ObjectField with
        /// Gets an ObjectField type from a parsed ObjectField type.
        static member OfParsed = convertObjectField

    type Argument with
        /// Gets an Argument type from a parsed Argument type.
        static member OfParsed = convertArgument

    type Directive with
        /// Gets a Directive type from a parsed Directive type.
        static member OfParsed = convertDirective

    type FragmentSpread with
        /// Gets a FragmentSpread type from a parsed FragmentSpread type.
        static member OfParsed = convertFragmentSpread

    type InlineFragment with
        /// Gets an InlineFragment type from a parsed InlineFragment type.
        static member OfParsed = convertInlineFragment

    type Field with
        /// Gets a Field type from a parsed Field type.
        static member OfParsed = convertField

    type Selection with
        /// Gets a Selection type from a parsed Selection type.
        static member OfParsed = convertSelection

    type Type with
        /// Gets a Type type from a parsed Type type.
        static member OfParsed = convertType

    type VariableDefinition with
        /// Gets a VariableDefinition type from a parsed VariableDefinition type.
        static member OfParsed = convertVariableDefinition

    type Operation with
        /// Gets an Operation type from a parsed Operation type.
        static member OfParsed = convertOperation

    type OperationDefinition with
        /// Gets an OperationDefinition type from a parsed OperationDefinition type.
        static member OfParsed = convertOperationDefinition

    type FragmentDefinition with
        /// Gets a FragmentDefinition type from a parsed FragmentDefinition type.
        static member OfParsed = convertFragmentDefinition

    type ExecutableDefinition with
        /// Gets an ExecutableDefinition type from a parsed ExecutableDefinition type.
        static member OfParsed = convertExecutableDefinition

    type OperationTypeDefinition with
        /// Gets an OperationTypeDefinition type from a parsed OperationTypeDefinition type.
        static member OfParsed = convertOperationTypeDefinition

    type SchemaDefinition with
        /// Gets a SchemaDefinition type from a parsed SchemaDefinition type.
        static member OfParsed = convertSchemaDefinition

    type InputValueDefinition with
        /// Gets an InputValueDefinition type from a parsed InputValueDefinition type.
        static member OfParsed = convertInputValueDefinition

    type FieldDefinition with
        /// Gets a FieldDefinition type from a parsed FieldDefinition type.
        static member OfParsed = convertFieldDefinition

    type InputObjectTypeDefinition with
        /// Gets an InputObjectTypeDefinition type from a parsed InputObjectTypeDefinition type.
        static member OfParsed = convertInputObjectTypeDefinition

    type EnumValueDefinition with
        /// Gets an EnumValueDefinition type from a parsed EnumValueDefinition type.
        static member OfParsed = convertEnumValueDefinition

    type EnumTypeDefinition with
        /// Gets an EnumTypeDefinition type from a parsed EnumTypeDefinition type.
        static member OfParsed = convertEnumTypeDefinition

    type UnionTypeDefinition with
        /// Gets an UnionTypeDefinition type from a parsed UnionTypeDefinition type.
        static member OfParsed = convertUnionTypeDefinition

    type InterfaceTypeDefinition with
        /// Gets an InterfaceTypeDefinition type from a parsed InterfaceTypeDefinition type.
        static member OfParsed = convertInterfaceTypeDefinition

    type ObjectTypeDefinition with
        /// Gets an ObjectTypeDefinition type from a parsed ObjectTypeDefinition type.
        static member OfParsed = convertObjectTypeDefinition

    type ScalarTypeDefinition with
        /// Gets a ScalarTypeDefinition type from a parsed ScalarTypeDefinition type.
        static member OfParsed = convertScalarTypeDefinition

    type TypeDefinition with
        /// Gets a TypeDefinition type from a parsed TypeDefinition type.
        static member OfParsed = convertTypeDefinition

    type InputObjectTypeExtension with
        /// Gets an InputObjectTypeExtension type from a parsed InputObjectTypeExtension type.
        static member OfParsed = convertInputObjectTypeExtension

    type EnumTypeExtension with
        /// Gets an EnumTypeExtension type from a parsed EnumTypeExtension type.
        static member OfParsed = convertEnumTypeExtension

    type UnionTypeExtension with
        /// Gets an UnionTypeExtension type from a parsed UnionTypeExtension type.
        static member OfParsed = convertUnionTypeExtension

    type InterfaceTypeExtension with
        /// Gets an InterfaceTypeExtension type from a parsed InterfaceTypeExtension type.
        static member OfParsed = convertInterfaceTypeExtension

    type ObjectTypeExtension with
        /// Gets an ObjectTypeExtension type from a parsed ObjectTypeExtension type.
        static member OfParsed = convertObjectTypeExtension

    type ScalarTypeExtension with
        /// Gets a ScalarTypeExtension type from a parsed ScalarTypeExtension type.
        static member OfParsed = convertScalarTypeExtension

    type TypeExtension with
        /// Gets a TypeExtension type from a parsed TypeExtension type.
        static member OfParsed = convertTypeExtension

    type SchemaExtension with
        /// Gets a SchemaExtension type from a parsed SchemaExtension type.
        static member OfParsed = convertSchemaExtension

    type TypeSystemExtension with
        /// Gets a TypeSystemExtension type from a parsed TypeSystemExtension type.
        static member OfParsed = convertTypeSystemExtension

    type DirectiveLocation with
        /// Gets a DirectiveLocation type from a parsed DirectiveLocation type.
        static member OfParsed = convertDirectiveLocation

    type DirectiveDefinition with
        /// Gets a DirectiveDefinition type from a parsed DirectiveDefinition type.
        static member OfParsed = convertDirectiveDefinition

    type TypeSystemDefinition with
        /// Gets a TypeSystemDefinition type from a parsed TypeSystemDefinition type.
        static member OfParsed = convertTypeSystemDefinition

    type Definition with
        /// Gets a Definition type from a parsed Definition type.
        static member OfParsed = convertDefinition

    type Document with
        /// Gets a Document type from a parsed Document type.
        static member OfParsed = convertDocument