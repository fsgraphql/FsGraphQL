module FsGraphQL.UnitTests.Parser

open Expecto
open Assertions
open FsGraphQL.Ast
open FsGraphQL.Ast.Parsing.Parser

let parseAndAssert expected query =
    let actual = query |> parse |> Document.OfParsed
    equals actual expected

let queryShorthand selectionSet = 
    selectionSet
    |> QueryShorthand
    |> OperationDefinition 
    |> ExecutableDefinition

let operation name operationType variables directives selectionSet =
    { OperationType = operationType
      Name = name
      VariableDefinitions = variables
      Directives = directives
      SelectionSet = selectionSet }

let queryDefinition name selectionSet =
    selectionSet
    |> operation name Query [] []
    |> Operation
    |> OperationDefinition
    |> ExecutableDefinition

let queryDefinitionWithNameAndSelection name selection = queryDefinition (Some name) [ selection ]

let queryDefinitionWithSelectionSet = queryDefinition None

let queryDefinitionWithSelection selection = queryDefinition None [ selection ]

let field name alias directives args selectionSet =
    { Name = name
      Alias = alias
      Directives = directives
      Arguments = args
      SelectionSet = selectionSet }
    |> Field

let fragmentSpread name directives =
    { Name = name
      Directives = directives }
    |> FragmentSpread

let inlineFragment typeCondition directives selectionSet =
    { TypeCondition = typeCondition
      SelectionSet = selectionSet
      Directives = directives }
    |> InlineFragment

let fragment name typeCondition directives selectionSet =
    { Name = name
      TypeCondition = typeCondition
      SelectionSet = selectionSet
      Directives = directives }
    |> FragmentDefinition
    |> ExecutableDefinition

let fieldWithName name = field name None [] [] []
let fieldWithNameAndArgs name args = field name None [] args []
let fieldWithNameAliasAndArgs name alias args = field name (Some alias) [] args []
let fieldWithNameAndSelectionSet name selectionSet = field name None [] [] selectionSet
let fieldWithNameArgsAndSelectionSet name args selectionSet = field name None [] args selectionSet
let fieldWithNameAliasArgsAndSelectionSet name alias args selectionSet = field name (Some alias) [] args selectionSet
let fieldWithNameArgsAndSelection name args selection = fieldWithNameArgsAndSelectionSet name args [ selection ]

let arg name value = 
    { Argument.Name = name
      Value = value }

let intArg name (value : int) = arg name (IntValue (int64 value))
let nullArg name = arg name NullValue
let stringArg name value = arg name (StringValue value)
let listArg name values = arg name (ListValue values)
let variableArg name variableName = arg name (Variable variableName)
let objArg name fields = arg name (ObjectValue fields)
let booleanArg name value = arg name (BooleanValue value)

let documentWithDefinitions definitions = { Definitions = definitions }
let emptyDocument = documentWithDefinitions []
let documentWithDefinition definition = documentWithDefinitions [ definition ]
let shortHandSelectionSet selectionSet = selectionSet |> queryShorthand |> documentWithDefinition
let shortHandSelection selection = shortHandSelectionSet [ selection ]
let unamedQueryWithSelectionSet selectionSet = selectionSet |> queryDefinitionWithSelectionSet |> documentWithDefinition
let unamedQueryWithSelection selection = unamedQueryWithSelectionSet [ selection ]
let queryWithSelectionSet name selectionSet = queryDefinition (Some name) selectionSet |> documentWithDefinition
let queryWithSelection name selection = queryWithSelectionSet name [ selection ]

[<Tests>]
let tests =
    testList "FsGraphQL.Ast.Parser tests" [

        test "Should be able to parse an empty document" {
            parseAndAssert emptyDocument ""
        }

        test "Should be able to parse a document with white spaces, line terminators and commas" {
            [| '\u0009'; '\u000B'; '\u000C'; '\u0020'; '\u00A0'; '\u000A'; '\u000D'; '\u2028'; '\u2029'; ',' |]
            |> System.String
            |> parseAndAssert emptyDocument
        }

        test "Should be able to parse a document with comments" {
            "# This is a comment, should be ignored"
            |> parseAndAssert emptyDocument
        }

        test "Should be able to parse a query with a single field in shorthand format" {
            let expected = 
                fieldWithName "id"
                |> shortHandSelection
            parseAndAssert expected "{id}"
        }

        test "Should be able to parse an unamed query with a single field" {
            let expected = 
                fieldWithName "id"
                |> unamedQueryWithSelection
            parseAndAssert expected "query {id}"
        }

        test "Should be able to parse a named query with a single field" {
            let expected = 
                fieldWithName "id"
                |> queryWithSelection "MyQuery"
            parseAndAssert expected "query MyQuery {id}"
        }

        test "Should be able to ignore whitespaces and line terminators in an unamed query with a single field" {
            let expected = 
                fieldWithName "id"
                |> queryWithSelection "MyQuery"
            parseAndAssert expected """query MyQuery {
                  id
                }"""
        }

        test "Should be able to parse more than one field in a shorthand query" {
            let expected =
                [ "id"; "name"; "url" ]
                |> List.map fieldWithName
                |> shortHandSelectionSet
            parseAndAssert expected "{id, name, url}"
        }

        test "Should be able to ignore whitespaces and line terminators in a shorthand query with more than one field" {
            let expected =
                [ "id"; "name"; "url" ]
                |> List.map fieldWithName
                |> shortHandSelectionSet
            parseAndAssert expected "{
                    id
                    name
                    url
                }"
        }

        test "Should be able to ignore whitespaces, line terminators and commas in a shorthand query with more than one field" {
            let expected =
                [ "id"; "name"; "url" ]
                |> List.map fieldWithName
                |> shortHandSelectionSet
            parseAndAssert expected "{
                    id,
                    name,
                    url
                }"
        }

        test "Should be able to parse more than one field in an unamed query" {
            let expected =
                [ "id"; "name"; "url" ]
                |> List.map fieldWithName
                |> unamedQueryWithSelectionSet
            parseAndAssert expected "query {id, name, url}"
        }

        test "Should be able to ignore whitespaces and line terminators in an unamed query with more than one field" {
            let expected =
                [ "id"; "name"; "url" ]
                |> List.map fieldWithName
                |> unamedQueryWithSelectionSet
            parseAndAssert expected "query {
                    id
                    name
                    url
                }"
        }

        test "Should be able to ignore whitespaces, line terminators and commas in an unamed query with more than one field" {
            let expected =
                [ "id"; "name"; "url" ]
                |> List.map fieldWithName
                |> unamedQueryWithSelectionSet
            parseAndAssert expected "query {
                    id,
                    name,
                    url
                }"
        }

        test "Should be able to parse more than one field in a named query" {
            let expected =
                [ "id"; "name"; "url" ]
                |> List.map fieldWithName
                |> queryWithSelectionSet "MyQuery"
            parseAndAssert expected "query MyQuery {id, name, url}"
        }

        test "Should be able to ignore whitespaces and line terminators in a named query with more than one field" {
            let expected =
                [ "id"; "name"; "url" ]
                |> List.map fieldWithName
                |> queryWithSelectionSet "MyQuery"
            parseAndAssert expected "query MyQuery {
                    id
                    name
                    url
                }"
        }

        test "Should be able to ignore whitespaces, line terminators and commas in a named query with more than one field" {
            let expected =
                [ "id"; "name"; "url" ]
                |> List.map fieldWithName
                |> queryWithSelectionSet "MyQuery"
            parseAndAssert expected "query MyQuery {
                    id,
                    name,
                    url
                }"
        }

        test "Should be able to parse nested field" {
            let expected =
                [ "phone"; "name" ]
                |> List.map fieldWithName
                |> fieldWithNameAndSelectionSet "contact"
                |> shortHandSelection
            parseAndAssert expected """{
                    contact {
                        phone
                        name
                    }
                }"""
        }

        test "Should be able to parse nested field, ignoring commas" {
            let expected =
                [ "phone"; "name" ]
                |> List.map fieldWithName
                |> fieldWithNameAndSelectionSet "contact"
                |> shortHandSelection
            parseAndAssert expected """{
                    contact {
                        phone,
                        name
                    }
                }"""
        }

        test "Should be able to parse many nested fields" {
            let contact =
                [ "phone"; "name" ]
                |> List.map fieldWithName
                |> fieldWithNameAndSelectionSet "contact"
            let address =
                [ "street"; "number"; "zip" ]
                |> List.map fieldWithName
                |> fieldWithNameAndSelectionSet "address"
            let expected =
                [ fieldWithName "uri"; contact; address; fieldWithName "id" ]
                |> shortHandSelectionSet
            parseAndAssert expected """{
                    uri
                    contact {
                        phone
                        name
                    }
                    address {
                        street
                        number
                        zip
                    }
                    id
                }"""
        }

        test "Should be able to parse many nested fields, ignoring commas" {
            let contact =
                [ "phone"; "name" ]
                |> List.map fieldWithName
                |> fieldWithNameAndSelectionSet "contact"
            let address =
                [ "street"; "number"; "zip" ]
                |> List.map fieldWithName
                |> fieldWithNameAndSelectionSet "address"
            let expected =
                [ fieldWithName "uri"; contact; address; fieldWithName "id" ]
                |> shortHandSelectionSet
            parseAndAssert expected """{
                    uri,
                    contact {
                        phone,
                        name
                    },
                    address {
                        street,
                        number,
                        zip
                    },
                    id
                }"""
        }

        test "Should be able to parse multi level nested fields" {
            let name =
                [ "first"; "last" ]
                |> List.map fieldWithName
                |> fieldWithNameAndSelectionSet "name"
            let contact =
                [ fieldWithName "phone"; name ]
                |> fieldWithNameAndSelectionSet "contact"
            let expected =
                [ fieldWithName "uri"; contact; fieldWithName "id" ]
                |> shortHandSelectionSet
            parseAndAssert expected """{
                    uri
                    contact {
                        phone
                        name {
                            first
                            last
                        }
                    }
                    id
                }"""
        }

        test "Should be able to parse multi level nested fields, ignoring commas" {
            let name =
                [ "first"; "last" ]
                |> List.map fieldWithName
                |> fieldWithNameAndSelectionSet "name"
            let contact =
                [ fieldWithName "phone"; name ]
                |> fieldWithNameAndSelectionSet "contact"
            let expected =
                [ fieldWithName "uri"; contact; fieldWithName "id" ]
                |> shortHandSelectionSet
            parseAndAssert expected """{
                    uri,
                    contact {
                        phone,
                        name {
                            first,
                            last
                        }
                    },
                    id
                }"""
        }

        test "Should parse query with arguments" {
            let contact =
                [ "phone"; "name" ]
                |> List.map fieldWithName
                |> fieldWithNameAndSelectionSet "contact"
            let address =
                [ "street"; "number"; "zip" ]
                |> List.map fieldWithName
                |> fieldWithNameAndSelectionSet "address"
            let profilePicture =
                fieldWithNameAndArgs "profilePicture" [ intArg "size" 50 ]
            let expected =
                [ fieldWithName "uri"; contact; profilePicture; address ]
                |> fieldWithNameArgsAndSelectionSet "user" [ stringArg "id" "1000" ]
                |> shortHandSelection
            parseAndAssert expected """{
                    user(id: "1000") {
                        uri
                        contact {
                            phone
                            name
                        }
                        profilePicture(size: 50)
                        address {
                            street
                            number
                            zip
                        }
                    }
                }"""
        }

        test "Should parse query with arguments, ignoring commas" {
            let contact =
                [ "phone"; "name" ]
                |> List.map fieldWithName
                |> fieldWithNameAndSelectionSet "contact"
            let address =
                [ "street"; "number"; "zip" ]
                |> List.map fieldWithName
                |> fieldWithNameAndSelectionSet "address"
            let profilePicture =
                fieldWithNameAndArgs "profilePicture" [ intArg "size" 50 ]
            let expected =
                [ fieldWithName "uri"; contact; profilePicture; address ]
                |> fieldWithNameArgsAndSelectionSet "user" [ stringArg "id" "1000" ]
                |> shortHandSelection
            parseAndAssert expected """{
                    user(id: "1000") {
                        uri,
                        contact {
                            phone,
                            name
                        },
                        profilePicture(size: 50),
                        address {
                            street,
                            number,
                            zip
                        }
                    }
                }"""
        }

        test "Should parse null arguments" {
            let expected =
                fieldWithName "name"
                |> fieldWithNameArgsAndSelection "user" [ nullArg "id" ]
                |> shortHandSelection
            parseAndAssert expected """{
                    user(id: null) {
                        name
                    }
                }"""
        }

        test "Should parse multiple arguments" {
            let profilePicture =
                fieldWithNameAndArgs "profilePicture" [ intArg "width" 100; intArg "height" 50 ]
            let expected =
                [ fieldWithName "name"; profilePicture ]
                |> fieldWithNameArgsAndSelectionSet "user" [ stringArg "id" "5000" ]
                |> shortHandSelection
            parseAndAssert expected """{
                    user(id: "5000") {
                        name
                        profilePicture(width: 100, height: 50)
                    }
                }""" 
        }

        test "Should parse fields with alias" {
            let profilePicture alias size =
                [ intArg "size" size ]
                |> fieldWithNameAliasAndArgs "profilePicture" alias
            let expected =
                [ fieldWithName "name"; profilePicture "smallProfilePicture" 50; profilePicture "bigProfilePicture" 100 ]
                |> fieldWithNameArgsAndSelectionSet "user" [ stringArg "id" "2000" ]
                |> shortHandSelection
            parseAndAssert expected """{
                    user(id: "2000") {
                        name
                        smallProfilePicture: profilePicture(size: 50)
                        bigProfilePicture: profilePicture(size: 100)
                    }
                }"""
        }

        test "Should parse top level field with alias" {
            let expected =
                [ "id"; "name" ]
                |> List.map fieldWithName
                |> fieldWithNameAliasArgsAndSelectionSet "user" "zuck" [ stringArg "id" "8000" ]
                |> shortHandSelection
            parseAndAssert expected """{
                    zuck: user(id: "8000") {
                        id
                        name
                    }
                }"""
        }

        test "Should parse query with fragments" {
            let friends name = 
                fragmentSpread "Friends" []
                |> fieldWithNameArgsAndSelection name [ intArg "first" 10 ]
            let query =
                [ friends "friends"; friends "mutualFriends" ]
                |> fieldWithNameArgsAndSelectionSet "user" [ stringArg "id" "7000" ]
                |> queryDefinitionWithNameAndSelection "WithFragments"
            let fragment =
                [ fieldWithName "id"; fieldWithName "name"; fieldWithNameAndArgs "profilePicture" [ intArg "size" 50 ] ]
                |> fragment "Friends" "User" []
            let expected =
                documentWithDefinitions [ fragment; query ]
            parseAndAssert expected """fragment Friends on User {
                    id
                    name
                    profilePicture(size: 50)
                }
                query WithFragments {
                    user(id: "7000") {
                        friends(first: 10) {
                            ...Friends
                        }
                        mutualFriends(first: 10) {
                            ...Friends
                        }
                    }
                }"""
        }

        test "Should parse query with nested fragments" {
            let friends name = 
                fragmentSpread "Friends" []
                |> fieldWithNameArgsAndSelection name [ intArg "first" 10 ]
            let query =
                [ friends "friends"; friends "mutualFriends" ]
                |> fieldWithNameArgsAndSelectionSet "user" [ stringArg "id" "7000" ]
                |> queryDefinitionWithNameAndSelection "WithFragments"
            let pictureFragment =
                [ fieldWithNameAndArgs "profilePicture" [ intArg "size" 50 ] ]
                |> fragment "ProfilePicture" "User" []
            let friendFragment =
                [ fieldWithName "id"; fieldWithName "name"; fragmentSpread "ProfilePicture" [] ]
                |> fragment "Friends" "User" []
            let expected =
                documentWithDefinitions [ pictureFragment; friendFragment; query ]
            parseAndAssert expected """fragment ProfilePicture on User {
                    profilePicture(size: 50)
                }
                fragment Friends on User {
                    id
                    name
                    ...ProfilePicture
                }
                query WithFragments {
                    user(id: "7000") {
                        friends(first: 10) {
                            ...Friends
                        }
                        mutualFriends(first: 10) {
                            ...Friends
                        }
                    }
                }"""
        }

        test "Should parse a query with inline fragments" {
            let friends name = 
                [ inlineFragment (Some "User") [] [ fieldWithName "id"; fieldWithName "name"; fieldWithNameAndArgs "profilePicture" [ intArg "size" 50 ] ]
                  inlineFragment (Some "Employee") [] [ fieldWithName "id"; fieldWithName "name" ] ]
                |> fieldWithNameArgsAndSelectionSet name [ intArg "first" 10 ]
            let query =
                [ friends "friends"; friends "mutualFriends" ]
                |> fieldWithNameArgsAndSelectionSet "user" [ stringArg "id" "7000" ]
                |> queryDefinitionWithNameAndSelection "WithInlineFragments"
            let expected =
                documentWithDefinition query
            parseAndAssert expected """query WithInlineFragments {
                    user(id: "7000") {
                        friends(first: 10) {
                            ...on User {
                                id
                                name
                                profilePicture(size: 50)
                            }
                            ...on Employee {
                                id
                                name
                            }
                        }
                        mutualFriends(first: 10) {
                            ...on User {
                                id
                                name
                                profilePicture(size: 50)
                            }
                            ...on Employee {
                            id
                            name
                            }
                        }
                    }
                }"""
        }

        testList "Kitchen sink tests" [

            test "Should parse kitchen sink query" {
                let executableDefinition = Operation >> OperationDefinition >> ExecutableDefinition
                let query =
                    let variables =
                        [ { Name = "foo"; Type = NamedType "ComplexType"; DefaultValue = None }
                          { Name = "site"; Type = NamedType "Site"; DefaultValue = Some (EnumValue "MOBILE") }]
                    let directives = [ { Name = "sequential"; Arguments = [] } ]
                    [ fieldWithName "id"
                      inlineFragment (Some "User") [ { Name = "defer"; Arguments = [] } ] [
                          fieldWithNameAndSelectionSet "field2" [
                              fieldWithName "id"
                              field "field1" (Some "alias") [ { Name = "include"; Arguments = [ variableArg "if" "foo" ] } ] [
                                      intArg "first" 10
                                      variableArg "after" "foo" ] [
                                          fieldWithName "id"
                                          fragmentSpread "frag" [] ] ] ] ]
                    |> fieldWithNameAliasArgsAndSelectionSet "node" "whoever123is" [ listArg "id" [ IntValue 123L; IntValue 456L ] ]
                    |> List.singleton
                    |> operation (Some "queryName") Query variables directives
                    |> executableDefinition
                let mutation =
                    [ fieldWithNameAndSelectionSet "story" [ fieldWithName "id" ] ]
                    |> field "like" None [ { Name = "defer"; Arguments = [] } ] [ intArg "story" 123 ]
                    |> List.singleton
                    |> operation (Some "likeStory") Mutation [] []
                    |> executableDefinition
                let subscription =
                    [ fieldWithNameAndSelectionSet "likers" [ fieldWithName "count" ]
                      fieldWithNameAndSelectionSet "likeSentence" [ fieldWithName "text" ] ]
                    |> fieldWithNameAndSelectionSet "story"
                    |> fieldWithNameArgsAndSelection "storyLikeSubscribe" [ variableArg "input" "input" ]
                    |> List.singleton
                    |> operation (Some "StoryLikeSubscription") Subscription [ { Name = "input"; Type = NamedType "StoryLikeSubscribeInput"; DefaultValue = None } ] []
                    |> executableDefinition
                let fragment =
                    fieldWithNameAndArgs "foo" [ variableArg "size" "size"; variableArg "bar" "b"; objArg "obj" [ { Name = "key"; Value = StringValue "value" } ] ]
                    |> List.singleton
                    |> fragment "frag" "Friend" []
                let shorthand =
                    [ fieldWithNameAndArgs "unnamed" [ booleanArg "truthy" true; booleanArg "falsey" false ]
                      fieldWithName "query"
                      inlineFragment None [ { Name = "skip"; Arguments = [ variableArg "unless" "foo" ] } ] [ fieldWithName "id" ]
                      inlineFragment None [] [ fieldWithName "id" ] ]
                    |> queryShorthand
                let expected =
                    documentWithDefinitions [ query; mutation; subscription; fragment; shorthand ]
                parseAndAssert expected """query queryName($foo: ComplexType, $site: Site = MOBILE) @sequential {
                      whoever123is: node(id: [123, 456]) {
                        id ,
                        ... on User @defer {
                          field2 {
                            id ,
                            alias: field1(first:10, after:$foo,) @include(if: $foo) {
                              id,
                              ...frag
                            }
                          }
                        }
                      }
                    }
                
                    mutation likeStory {
                      like(story: 123) @defer {
                        story {
                          id
                        }
                      }
                    }
                
                    subscription StoryLikeSubscription($input: StoryLikeSubscribeInput) {
                      storyLikeSubscribe(input: $input) {
                        story {
                          likers {
                            count
                          }
                          likeSentence {
                            text
                          }
                        }
                      }
                    }
                
                    fragment frag on Friend {
                      foo(size: $size, bar: $b, obj: {key: "value"})
                    }
                
                    {
                      unnamed(truthy: true, falsey: false),
                      query
                
                      ... @skip(unless: $foo) {
                        id
                      }
                      ... {
                        id
                      }
                    }"""
            }
        ]
    ]