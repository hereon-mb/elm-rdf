module RDF.EncodeTest exposing (suite)

import Expect
import RDF
import RDF.Encode as Encode
import RDF.Graph as RDF
import RDF.PropertyPath as RDF
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "RDF.Encode"
        [ bunchLeaf
        ]


bunchLeaf : Test
bunchLeaf =
    test "bunch leaf" <|
        \_ ->
            let
                hasValue : RDF.PropertyPath
                hasValue =
                    RDF.PredicatePath (base "hasValue")

                valueValue : RDF.Literal Int
                valueValue =
                    RDF.int 1

                hasNested : RDF.PropertyPath
                hasNested =
                    RDF.PredicatePath (base "hasNested")

                hasUnit : RDF.PropertyPath
                hasUnit =
                    RDF.PredicatePath (base "hasUnit")

                valueUnit : RDF.Iri
                valueUnit =
                    base "meter"

                hasConstant : RDF.PropertyPath
                hasConstant =
                    RDF.PredicatePath (base "hasConstant")

                valueConstant : RDF.Literal String
                valueConstant =
                    RDF.string "something"
            in
            (Encode.node (RDF.toBlankNodeOrIri (base "root"))
                (Encode.bunch
                    [ ( hasValue, Encode.literal valueValue )
                    , ( RDF.SequencePath hasNested [ hasUnit ], Encode.iri valueUnit )
                    , ( RDF.SequencePath hasNested [ hasConstant ], Encode.literal valueConstant )
                    ]
                )
                |> Encode.encode RDF.initialSeed
                |> Tuple.first
                |> RDF.serialize
            )
                |> Ok
                |> Expect.equal
                    (("""
                      @base <""" ++ RDF.toUrl (base "") ++ """> .
                      @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                      <root>
                          <hasValue> "1"^^xsd:integer ;
                          <hasNested> [
                              <hasUnit> <meter> ;
                              <hasConstant> "something" ;
                          ] ;
                       .
                      """)
                        |> RDF.parse
                        |> Result.map RDF.serialize
                    )


base : String -> RDF.Iri
base name =
    RDF.iri ("http://example.org/" ++ name)
