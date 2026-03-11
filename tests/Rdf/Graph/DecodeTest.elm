{- Copyright 2024-2026 Helmholtz-Zentrum hereon GmbH

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}


module Rdf.Graph.DecodeTest exposing (suite)

import Expect exposing (Expectation)
import Rdf
import Rdf.Graph as Graph
import Rdf.Graph.Decode as Decode exposing (Decoder)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Rdf.Decode"
        [ many
        , manyMany
        , manyWithInverse
        , manyWithInverseAndData
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
                        (Decode.many (Decode.property (example "#name") Decode.string))
                    )
            }
                |> expectAll
                    [ List.sort
                        >> Expect.equalLists [ "Alice Wonderland", "Bob Builder" ]
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
                        (Decode.ProblemAt
                            [ Rdf.asBlankNodeOrIri (example "x") ]
                            (Rdf.asPath (example "hasString"))
                            (Decode.ExpectedLiteralOf
                                [ xsd "string" ]
                                (Rdf.integer 42)
                            )
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
                        (Decode.ProblemAt
                            [ Rdf.asBlankNodeOrIri (example "x") ]
                            (Rdf.asPath (example "hasString"))
                            (Decode.ExpectedPath
                                (Rdf.asBlankNodeOrIri (example "x"))
                                (Rdf.asPath (example "hasString"))
                            )
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
                        (Decode.ExpectedNoPath
                            (Rdf.asBlankNodeOrIri (example "x"))
                            (Rdf.asPath (example "hasString"))
                            [ Rdf.asBlankNodeOrIriOrLiteral
                                (Rdf.string "string")
                            ]
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
                        [ Decode.property (example "hasIntegerA") Decode.int
                        , Decode.property (example "hasIntegerB") Decode.int
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
                        [ Decode.property (example "hasIntegerA") Decode.int
                        , Decode.property (example "hasIntegerB") Decode.int
                        ]
                    )
            }
                |> expectAllError
                    [ Expect.equal
                        (Decode.ProblemAt
                            [ Rdf.asBlankNodeOrIri (example "x") ]
                            (Rdf.asPath (example "hasIntegerB"))
                            (Decode.ExpectedLiteralOf
                                [ xsd "integer"
                                , xsd "int"
                                ]
                                (Rdf.string "string")
                            )
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


expectAllError :
    List (Decode.Problem -> Expectation)
    -> { raw : String, decoder : Decoder a }
    -> Expectation
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
