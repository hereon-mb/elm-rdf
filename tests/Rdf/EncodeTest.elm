module Rdf.EncodeTest exposing (suite)

import Expect exposing (Expectation)
import Rdf exposing (Path)
import Rdf.Encode as Encode exposing (GraphEncoder)
import Rdf.Graph as Graph
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
