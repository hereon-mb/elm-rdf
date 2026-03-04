module Rdf.DecodeTest exposing (suite)

import Expect exposing (Expectation)
import Rdf
import Rdf.Decode as Decode exposing (Decoder)
import Rdf.Graph as Graph
import Rdf.Namespaces as Rdf
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Rdf.Decode"
        [ many
        , manyMany
        , manyWithInverse
        , manyWithInverseAndData
        , stringOrLangString
        , describe "property"
            [ propertyWithCorrectObject
            , propertyWithIncorrectObject
            , propertyMissing
            , propertyWithPathAndWithManyInstances
            ]
        , describe "noProperty"
            [ noPropertySuccess
            , noPropertyWithProperty
            ]
        , describe "combine"
            [ combineSuccess
            , combineOneFails
            ]
        ]


many : Test
many =
    test "many" <|
        \_ ->
            { raw =
                """
                    @base <http://example.org/> .
                    <x> <label> "a", "b" .
                """
            , decoder =
                Decode.from
                    (example "x")
                    (Decode.property
                        (example "label")
                        (Decode.many Decode.string)
                    )
            }
                |> expectAll [ List.sort >> Expect.equalLists [ "a", "b" ] ]


manyMany : Test
manyMany =
    test "many many" <|
        \_ ->
            { raw =
                """
                    @base <http://example.org/> .
                    <x> <label> "a", "b" .
                """
            , decoder =
                Decode.from (example "x")
                    (Decode.property
                        (example "label")
                        (Decode.map List.concat (Decode.many (Decode.many Decode.string)))
                    )
            }
                |> expectAll [ List.sort >> Expect.equalLists [ "a", "b" ] ]


manyWithInverse : Test
manyWithInverse =
    test "many with inverse" <|
        \_ ->
            { raw =
                """
                    @base <http://example.org/> .
                    <alice> a <#Person> .
                    <bob> a <#Person> .
                """
            , decoder =
                Decode.from (example "#Person")
                    (Decode.property (Rdf.inverse Rdf.a)
                        (Decode.many Decode.iri)
                    )
            }
                |> expectAll
                    [ List.map Rdf.serialize
                        >> List.sort
                        >> Expect.equalLists
                            (List.map Rdf.serialize
                                [ example "alice"
                                , example "bob"
                                ]
                            )
                    ]


manyWithInverseAndData : Test
manyWithInverseAndData =
    test "many with inverse and data" <|
        \_ ->
            { raw =
                """
                    @base <http://example.org/> .
                    <alice> a <#Person> ;
                        <#knows> <bob> ;
                        <#name> "Alice Wonderland" .
                    <bob> a <#Person> ;
                        <#knows> <alice> ;
                        <#name> "Bob Builder".
                """
            , decoder =
                Decode.from (example "#Person")
                    (Decode.property (Rdf.inverse Rdf.a)
                        (Decode.many (Decode.predicate (example "#name") Decode.string))
                    )
            }
                |> expectAll
                    [ List.sort
                        >> Expect.equalLists [ "Alice Wonderland", "Bob Builder" ]
                    ]


stringOrLangString : Test
stringOrLangString =
    test "string or lang string" <|
        \_ ->
            { raw =
                """
                    @base <http://example.org/> .
                    @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
                    <x> rdfs:label "de", "en"@en, "fr"@fr .
                """
            , decoder =
                Decode.from (example "x")
                    (Decode.property (Rdf.rdfs "label")
                        Decode.stringOrLangString
                    )
            }
                |> expectAll
                    [ Expect.equal
                        (Rdf.stringOrLangStringFrom (Just "de")
                            [ ( "en", "en" )
                            , ( "fr", "fr" )
                            ]
                        )
                    ]


propertyWithCorrectObject : Test
propertyWithCorrectObject =
    test "with correct object" <|
        \_ ->
            { raw =
                """
                    @base <http://example.org/> .
                    <x> <hasString> "string" .
                """
            , decoder =
                Decode.from (example "x")
                    (Decode.property (example "hasString")
                        Decode.string
                    )
            }
                |> expectAll [ Expect.equal "string" ]


propertyWithIncorrectObject : Test
propertyWithIncorrectObject =
    test "with incorrect object" <|
        \_ ->
            { raw =
                """
                    @base <http://example.org/> .
                    <x> <hasString> 42 .
                """
            , decoder =
                Decode.from (example "x")
                    (Decode.property (example "hasString")
                        Decode.string
                    )
            }
                |> expectAllError
                    [ Expect.equal
                        (Decode.ExpectedLiteralDatatype
                            (xsd "string")
                            (xsd "integer")
                        )
                    ]


propertyMissing : Test
propertyMissing =
    test "missing" <|
        \_ ->
            { raw =
                """
                    @base <http://example.org/> .
                    <x> <hasInteger> 42 .
                """
            , decoder =
                Decode.from (example "x")
                    (Decode.property (example "hasString")
                        Decode.string
                    )
            }
                |> expectAllError
                    [ Expect.equal
                        (Decode.AtPropertyPath
                            (Rdf.asIriOrPath (example "hasString"))
                            { contextStack = []
                            , error =
                                Decode.UnknownProperty
                                    (Rdf.asBlankNodeOrIri (example "x"))
                                    (Rdf.asIriOrPath (example "hasString"))
                            }
                        )
                    ]


propertyWithPathAndWithManyInstances : Test
propertyWithPathAndWithManyInstances =
    test "success" <|
        \_ ->
            { raw =
                """
                    @base <http://example.org/> .
                    <x> <hasInstance>
                        [ <hasInteger> 42 ] ,
                        [ <hasInteger> 84 ] .
                """
            , decoder =
                Decode.from (example "x")
                    (Decode.property
                        (Rdf.sequence
                            (example "hasInstance")
                            [ example "hasInteger" ]
                        )
                        (Decode.many Decode.int)
                    )
            }
                |> expectAll [ Expect.equal [ 84, 42 ] ]


noPropertySuccess : Test
noPropertySuccess =
    test "success" <|
        \_ ->
            { raw =
                """
                    @base <http://example.org/> .
                    <x> <hasInteger> 42 .
                """
            , decoder =
                Decode.from (example "x")
                    (Decode.noProperty (example "hasString"))
            }
                |> expectAll [ Expect.equal () ]


noPropertyWithProperty : Test
noPropertyWithProperty =
    test "with property" <|
        \_ ->
            { raw =
                """
                    @base <http://example.org/> .
                    <x> <hasString> "string" .
                """
            , decoder =
                Decode.from (example "x")
                    (Decode.noProperty (example "hasString"))
            }
                |> expectAllError
                    [ Expect.equal
                        (Decode.PropertyPresent
                            (Rdf.asBlankNodeOrIri (example "x"))
                            (Rdf.asIriOrPath (example "hasString"))
                        )
                    ]


combineSuccess : Test
combineSuccess =
    test "success" <|
        \_ ->
            { raw =
                """
                    @base <http://example.org/> .
                    <x> <hasIntegerA> 42 ;
                        <hasIntegerB> 84 .
                """
            , decoder =
                Decode.from (example "x")
                    (Decode.combine
                        [ Decode.predicate (example "hasIntegerA") Decode.int
                        , Decode.predicate (example "hasIntegerB") Decode.int
                        ]
                    )
            }
                |> expectAll [ Expect.equal [ 42, 84 ] ]


combineOneFails : Test
combineOneFails =
    test "one fails" <|
        \_ ->
            { raw =
                """
                    @base <http://example.org/> .
                    <x> <hasIntegerA> 42 ;
                        <hasIntegerB> "string" .
                """
            , decoder =
                Decode.from (example "x")
                    (Decode.combine
                        [ Decode.predicate (example "hasIntegerA") Decode.int
                        , Decode.predicate (example "hasIntegerB") Decode.int
                        ]
                    )
            }
                |> expectAllError
                    [ Expect.equal
                        (Decode.Batch
                            [ { error =
                                    Decode.ExpectedLiteralDatatype
                                        (xsd "integer")
                                        (xsd "string")
                              , contextStack = []
                              }
                            , { error =
                                    Decode.ExpectedLiteralDatatype
                                        (xsd "int")
                                        (xsd "string")
                              , contextStack = []
                              }
                            ]
                        )
                    ]


example : String -> Rdf.Iri
example name =
    Rdf.iri ("http://example.org/" ++ name)


xsd : String -> Rdf.Iri
xsd name =
    Rdf.iri ("http://www.w3.org/2001/XMLSchema#" ++ name)


expectAll : List (a -> Expectation) -> { raw : String, decoder : Decoder a } -> Expectation
expectAll expectations { raw, decoder } =
    case Graph.parse raw of
        Err error ->
            Expect.fail ("Could not parse the graph: " ++ Graph.errorToString raw error)

        Ok graph ->
            case Decode.decode decoder graph of
                Err error ->
                    Expect.fail ("Could not decode the value: " ++ Decode.errorToString error)

                Ok value ->
                    Expect.all expectations value


expectAllError : List (Decode.Problem -> Expectation) -> { raw : String, decoder : Decoder a } -> Expectation
expectAllError expectations { raw, decoder } =
    case Graph.parse raw of
        Err error ->
            Expect.fail ("Could not parse the graph: " ++ Graph.errorToString raw error)

        Ok graph ->
            case Decode.decode decoder graph of
                Err error ->
                    Expect.all expectations error.error

                Ok _ ->
                    Expect.fail "Could decode the value."
