module Rdf.EncodeTest exposing (suite)

import Expect exposing (Expectation)
import Rdf
import Rdf.Encode as Encode exposing (GraphEncoder)
import Rdf.Graph as Rdf
import Rdf.PropertyPath as Rdf
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
                    , ( Rdf.SequencePath hasNested [ hasUnit ]
                      , Encode.iri valueUnit
                      )
                    , ( Rdf.SequencePath hasNested [ hasConstant ]
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
                \test ->
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
                \test ->
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
                \test ->
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
                \test ->
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


hasValue : Rdf.PropertyPath
hasValue =
    Rdf.PredicatePath (base "hasValue")


valueValue : Rdf.Literal Int
valueValue =
    Rdf.integer 1


hasNested : Rdf.PropertyPath
hasNested =
    Rdf.PredicatePath (base "hasNested")


hasUnit : Rdf.PropertyPath
hasUnit =
    Rdf.PredicatePath (base "hasUnit")


valueUnit : Rdf.Iri
valueUnit =
    base "meter"


hasConstant : Rdf.PropertyPath
hasConstant =
    Rdf.PredicatePath (base "hasConstant")


valueConstant : Rdf.Literal String
valueConstant =
    Rdf.string "something"


expectGraph : String -> GraphEncoder -> Expectation
expectGraph raw encoder =
    let
        prefix =
            """
            @base <""" ++ Rdf.toUrl (base "") ++ """> .
            @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
            """
    in
    encoder
        |> Encode.encode Rdf.initialSeed
        |> Tuple.first
        |> Rdf.serialize
        |> Ok
        |> Expect.equal
            ((prefix ++ raw)
                |> Rdf.parse
                |> Result.map Rdf.serialize
            )
