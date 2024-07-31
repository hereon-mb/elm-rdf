module Rdf.EncodeTest exposing (suite)

import Expect
import Rdf
import Rdf.Encode as Encode
import Rdf.Graph as Rdf
import Rdf.PropertyPath as Rdf
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Rdf.Encode"
        [ bunchLeaf
        ]


bunchLeaf : Test
bunchLeaf =
    test "bunch leaf" <|
        \_ ->
            let
                hasValue : Rdf.PropertyPath
                hasValue =
                    Rdf.PredicatePath (base "hasValue")

                valueValue : Rdf.Literal Int
                valueValue =
                    Rdf.int 1

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
            in
            (Encode.node (Rdf.toBlankNodeOrIri (base "root"))
                (Encode.bunch
                    [ ( hasValue, Encode.literal valueValue )
                    , ( Rdf.SequencePath hasNested [ hasUnit ], Encode.iri valueUnit )
                    , ( Rdf.SequencePath hasNested [ hasConstant ], Encode.literal valueConstant )
                    ]
                )
                |> Encode.encode Rdf.initialSeed
                |> Tuple.first
                |> Rdf.serialize
            )
                |> Ok
                |> Expect.equal
                    (("""
                      @base <""" ++ Rdf.toUrl (base "") ++ """> .
                      @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                      <root>
                          <hasValue> "1"^^xsd:integer ;
                          <hasNested> [
                              <hasUnit> <meter> ;
                              <hasConstant> "something" ;
                          ] ;
                       .
                      """)
                        |> Rdf.parse
                        |> Result.map Rdf.serialize
                    )


base : String -> Rdf.Iri
base name =
    Rdf.iri ("http://example.org/" ++ name)
