module Rdf.EncodeTest exposing (suite)

import Expect exposing (Expectation)
import Rdf exposing (IriOrPath)
import Rdf.Encode as Encode exposing (GraphEncoder)
import Rdf.Graph as Graph
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Rdf.Encode"
        [ bunchLeaf
        , from
        , fromBlankNode
        ]


bunchLeaf : Test
bunchLeaf =
    test "bunch leaf" <|
        \_ ->
            Encode.node (Rdf.asBlankNodeOrIri (base "root"))
                (Encode.bunch
                    [ ( hasValue, Encode.literal valueValue )
                    , ( Rdf.sequence hasNested [ hasUnit ]
                      , Encode.iri valueUnit
                      )
                    , ( Rdf.sequence hasNested [ hasConstant ]
                      , Encode.literal valueConstant
                      )
                    ]
                )
                |> expectGraph
                    """
                    <root>
                        <hasValue> "1"^^xsd:integer ;
                        <hasNested> [
                            <hasUnit> <meter> ;
                            <hasConstant> "something" ;
                        ] ;
                     .
                    """


from : Test
from =
    describe "from"
        [ describe "with predicate"
            [ test "nested once inside node" <|
                \_ ->
                    Encode.node (Rdf.asBlankNodeOrIri (base "root"))
                        [ Encode.from (Rdf.asBlankNodeOrIri (base "other"))
                            [ Encode.predicate (base "hasValue")
                                (Encode.literal valueValue)
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
                            [ Encode.predicate (base "hasValue")
                                (Encode.literal valueValue)
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
                            [ Encode.predicate (base "hasValue")
                                (Encode.literal valueValue)
                            ]
                        , Encode.fromBlankNode
                            [ Encode.predicate (base "hasValue")
                                (Encode.literal valueValue)
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
                            [ Encode.predicate (base "hasValueA")
                                (Encode.literal valueValue)
                            , Encode.predicate (base "hasValueB")
                                (Encode.literal valueValue)
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
                                (Encode.literal valueValue)
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
                                (Encode.literal valueValue)
                            ]
                        , Encode.fromBlankNode
                            [ Encode.property hasValue
                                (Encode.literal valueValue)
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


hasValue : IriOrPath
hasValue =
    Rdf.asIriOrPath (base "hasValue")


valueValue : Rdf.Literal
valueValue =
    Rdf.integer 1


hasNested : IriOrPath
hasNested =
    Rdf.asIriOrPath (base "hasNested")


hasUnit : IriOrPath
hasUnit =
    Rdf.asIriOrPath (base "hasUnit")


valueUnit : Rdf.Iri
valueUnit =
    base "meter"


hasConstant : IriOrPath
hasConstant =
    Rdf.asIriOrPath (base "hasConstant")


valueConstant : Rdf.Literal
valueConstant =
    Rdf.string "something"


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
