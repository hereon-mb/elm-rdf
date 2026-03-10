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


module Rdf.Graph.EncodeTest exposing (suite)

import Expect exposing (Expectation)
import Rdf exposing (Path)
import Rdf.Graph as Graph
import Rdf.Graph.Encode as Encode exposing (GraphEncoder)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Rdf.Encode"
        [ fromNode
        , fromBlankNode
        ]


fromNode : Test
fromNode =
    describe "fromNode"
        [ describe "with predicate"
            [ test "nested once inside node" <|
                \_ ->
                    Encode.node (Rdf.asBlankNodeOrIri (base "root"))
                        [ Encode.fromNode (Rdf.asBlankNodeOrIri (base "other"))
                            [ Encode.property (base "hasValue")
                                (Encode.term valueValue)
                            ]
                        ]
                        |> expectGraph
                            """
                            <other> <http://example.org/hasValue> 1 .
                            """
            ]
        ]


fromBlankNode : Test
fromBlankNode =
    describe "fromBlankNode"
        [ describe "with predicate"
            [ test "nested once inside node" <|
                \_ ->
                    Encode.node (Rdf.asBlankNodeOrIri (base "root"))
                        [ Encode.fromBlankNode
                            [ Encode.property (base "hasValue")
                                (Encode.term valueValue)
                            ]
                        ]
                        |> expectGraph
                            """
                            [] <http://example.org/hasValue> 1 .
                            """
            , test "nested twice inside node" <|
                \_ ->
                    Encode.node (Rdf.asBlankNodeOrIri (base "root"))
                        [ Encode.fromBlankNode
                            [ Encode.property (base "hasValue")
                                (Encode.term valueValue)
                            ]
                        , Encode.fromBlankNode
                            [ Encode.property (base "hasValue")
                                (Encode.term valueValue)
                            ]
                        ]
                        |> expectGraph
                            """
                            [] <http://example.org/hasValue> 1 .
                            [] <http://example.org/hasValue> 1 .
                            """
            ]
        , describe "with two predicates"
            [ test "nested once inside node" <|
                \_ ->
                    Encode.node (Rdf.asBlankNodeOrIri (base "root"))
                        [ Encode.fromBlankNode
                            [ Encode.property (base "hasValueA")
                                (Encode.term valueValue)
                            , Encode.property (base "hasValueB")
                                (Encode.term valueValue)
                            ]
                        ]
                        |> expectGraph
                            """
                            []
                                <http://example.org/hasValueA> 1 ;
                                <http://example.org/hasValueB> 1 ;
                            .
                            """
            ]
        , describe "with property"
            [ test "nested once inside node" <|
                \_ ->
                    Encode.node (Rdf.asBlankNodeOrIri (base "root"))
                        [ Encode.fromBlankNode
                            [ Encode.property hasValue
                                (Encode.term valueValue)
                            ]
                        ]
                        |> expectGraph
                            """
                            [] <http://example.org/hasValue> 1 .
                            """
            , test "nested twice inside node" <|
                \_ ->
                    Encode.node (Rdf.asBlankNodeOrIri (base "root"))
                        [ Encode.fromBlankNode
                            [ Encode.property hasValue
                                (Encode.term valueValue)
                            ]
                        , Encode.fromBlankNode
                            [ Encode.property hasValue
                                (Encode.term valueValue)
                            ]
                        ]
                        |> expectGraph
                            """
                            [] <http://example.org/hasValue> 1 .
                            [] <http://example.org/hasValue> 1 .
                            """
            ]
        ]


base : String -> Rdf.Iri
base name =
    Rdf.iri ("http://example.org/" ++ name)


hasValue : Path
hasValue =
    Rdf.asPath (base "hasValue")


valueValue : Rdf.Literal
valueValue =
    Rdf.integer 1


expectGraph : String -> GraphEncoder -> Expectation
expectGraph raw encoder =
    let
        prefix : String
        prefix =
            """
            @base <""" ++ Rdf.toUrl (base "") ++ """> .
            @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
            """
    in
    encoder
        |> Encode.encode Graph.initialSeed
        |> Tuple.first
        |> Graph.serialize
        |> Ok
        |> Expect.equal
            ((prefix ++ raw)
                |> Graph.parse
                |> Result.map Graph.serialize
            )
