module RDFTest exposing (suite)

import Expect
import RDF
import RDF.Namespaces exposing (rdf, xsd)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "RDF"
        [ parseTurtle
        ]


parseTurtle : Test
parseTurtle =
    testParseTurtle
        [ ( ""
          , []
          )
        , ( "<http://example.org/alice> <http://example.org/knows> <http://example.org/bob> ."
          , [ { subject = RDF.toBlankNodeOrIri (RDF.iriAbsolute "http://example.org/alice")
              , predicate = RDF.iriAbsolute "http://example.org/knows"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.iriAbsolute "http://example.org/bob")
              }
            ]
          )
        , ( [ "@prefix example: <http://example.org/> ."
            , "example:alice example:knows example:bob ."
            ]
                |> String.join "\n"
          , [ { subject = RDF.toBlankNodeOrIri (RDF.iriAbsolute "http://example.org/alice")
              , predicate = RDF.iriAbsolute "http://example.org/knows"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.iriAbsolute "http://example.org/bob")
              }
            ]
          )
        , ( "[] <http://example.org/knows> <http://example.org/bob> ."
          , [ { subject = RDF.toBlankNodeOrIri (RDF.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              , predicate = RDF.iriAbsolute "http://example.org/knows"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.iriAbsolute "http://example.org/bob")
              }
            ]
          )
        , ( [ "[ <http://example.org/knows> <http://example.org/bob> ]"
            , "    <http://example.org/knows> <http://example.org/cindi> ."
            ]
                |> String.join "\n"
          , [ { subject = RDF.toBlankNodeOrIri (RDF.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              , predicate = RDF.iriAbsolute "http://example.org/knows"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.iriAbsolute "http://example.org/cindi")
              }
            , { subject = RDF.toBlankNodeOrIri (RDF.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              , predicate = RDF.iriAbsolute "http://example.org/knows"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.iriAbsolute "http://example.org/bob")
              }
            ]
          )
        , ( "_:alice <http://example.org/knows> <http://example.org/bob> ."
          , [ { subject = RDF.toBlankNodeOrIri (RDF.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              , predicate = RDF.iriAbsolute "http://example.org/knows"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.iriAbsolute "http://example.org/bob")
              }
            ]
          )
        , ( [ "_:alice <http://example.org/knows> <http://example.org/bob> ."
            , "_:alice <http://example.org/knows> <http://example.org/cindi> ."
            ]
                |> String.join "\n"
          , [ { subject = RDF.toBlankNodeOrIri (RDF.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              , predicate = RDF.iriAbsolute "http://example.org/knows"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.iriAbsolute "http://example.org/cindi")
              }
            , { subject = RDF.toBlankNodeOrIri (RDF.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              , predicate = RDF.iriAbsolute "http://example.org/knows"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.iriAbsolute "http://example.org/bob")
              }
            ]
          )
        , ( "<http://example.org/alice> <http://example.org/knows> [] ."
          , [ { subject = RDF.toBlankNodeOrIri (RDF.iriAbsolute "http://example.org/alice")
              , predicate = RDF.iriAbsolute "http://example.org/knows"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              }
            ]
          )
        , ( [ "<http://example.org/alice> <http://example.org/knows> ["
            , "    <http://example.org/knows> <http://example.org/bob>"
            , "] ."
            ]
                |> String.join "\n"
          , [ { subject = RDF.toBlankNodeOrIri (RDF.iriAbsolute "http://example.org/alice")
              , predicate = RDF.iriAbsolute "http://example.org/knows"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              }
            , { subject = RDF.toBlankNodeOrIri (RDF.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              , predicate = RDF.iriAbsolute "http://example.org/knows"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.iriAbsolute "http://example.org/bob")
              }
            ]
          )
        , ( "<http://example.org/alice> <http://example.org/knows> _:bob ."
          , [ { subject = RDF.toBlankNodeOrIri (RDF.iriAbsolute "http://example.org/alice")
              , predicate = RDF.iriAbsolute "http://example.org/knows"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              }
            ]
          )
        , ( [ "@base <http://example.org/> ."
            , "<alice> <knows> <bob> ."
            ]
                |> String.join "\n"
          , [ { subject = RDF.toBlankNodeOrIri (RDF.iriAbsolute "http://example.org/alice")
              , predicate = RDF.iriAbsolute "http://example.org/knows"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.iriAbsolute "http://example.org/bob")
              }
            ]
          )
        , ( [ "@base <http://example.org/> ."
            , "<alice> <value> \"Alice\" ."
            ]
                |> String.join "\n"
          , [ { subject = RDF.toBlankNodeOrIri (RDF.iriAbsolute "http://example.org/alice")
              , predicate = RDF.iriAbsolute "http://example.org/value"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.string "Alice")
              }
            ]
          )
        , ( [ "@base <http://example.org/> ."
            , "<alice> <value> \"Alice\"@en ."
            ]
                |> String.join "\n"
          , [ { subject = RDF.toBlankNodeOrIri (RDF.iriAbsolute "http://example.org/alice")
              , predicate = RDF.iriAbsolute "http://example.org/value"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.literal (xsd "langString") (Just "en") "Alice")
              }
            ]
          )
        , ( [ "@base <http://example.org/> ."
            , "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> ."
            , "<alice> <value> \"42\"^^xsd:integer ."
            ]
                |> String.join "\n"
          , [ { subject = RDF.toBlankNodeOrIri (RDF.iriAbsolute "http://example.org/alice")
              , predicate = RDF.iriAbsolute "http://example.org/value"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.literal (xsd "integer") Nothing "42")
              }
            ]
          )
        , ( [ "@base <http://example.org/> ."
            , "<alice> <value> 42 ."
            ]
                |> String.join "\n"
          , [ { subject = RDF.toBlankNodeOrIri (RDF.iriAbsolute "http://example.org/alice")
              , predicate = RDF.iriAbsolute "http://example.org/value"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.literal (xsd "integer") Nothing "42")
              }
            ]
          )
        , ( [ "@base <http://example.org/> ."
            , "<alice> <value> 3.14 ."
            ]
                |> String.join "\n"
          , [ { subject = RDF.toBlankNodeOrIri (RDF.iriAbsolute "http://example.org/alice")
              , predicate = RDF.iriAbsolute "http://example.org/value"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.literal (xsd "decimal") Nothing "3.14")
              }
            ]
          )
        , ( [ "@base <http://example.org/> ."
            , "<alice> <value> 0.314e1 ."
            ]
                |> String.join "\n"
          , [ { subject = RDF.toBlankNodeOrIri (RDF.iriAbsolute "http://example.org/alice")
              , predicate = RDF.iriAbsolute "http://example.org/value"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.literal (xsd "double") Nothing "3.14")
              }
            ]
          )
        , ( [ "@base <http://example.org/> ."
            , "<alice> <value> true ."
            ]
                |> String.join "\n"
          , [ { subject = RDF.toBlankNodeOrIri (RDF.iriAbsolute "http://example.org/alice")
              , predicate = RDF.iriAbsolute "http://example.org/value"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.literal (xsd "boolean") Nothing "true")
              }
            ]
          )
        , ( [ "@base <http://example.org/> ."
            , "<alice> <value> false ."
            ]
                |> String.join "\n"
          , [ { subject = RDF.toBlankNodeOrIri (RDF.iriAbsolute "http://example.org/alice")
              , predicate = RDF.iriAbsolute "http://example.org/value"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.literal (xsd "boolean") Nothing "false")
              }
            ]
          )
        , ( [ "<http://example.org/alice> <http://example.org/knows> <http://example.org/bob> ."
            , "<http://example.org/alice> <http://example.org/knows> <http://example.org/cindi> ."
            ]
                |> String.join "\n"
          , [ { subject = RDF.toBlankNodeOrIri (RDF.iriAbsolute "http://example.org/alice")
              , predicate = RDF.iriAbsolute "http://example.org/knows"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.iriAbsolute "http://example.org/cindi")
              }
            , { subject = RDF.toBlankNodeOrIri (RDF.iriAbsolute "http://example.org/alice")
              , predicate = RDF.iriAbsolute "http://example.org/knows"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.iriAbsolute "http://example.org/bob")
              }
            ]
          )
        , ( [ "<http://example.org/alice> <http://example.org/knows> <http://example.org/bob> ;"
            , "                           <http://example.org/knows> <http://example.org/cindi> ."
            ]
                |> String.join "\n"
          , [ { subject = RDF.toBlankNodeOrIri (RDF.iriAbsolute "http://example.org/alice")
              , predicate = RDF.iriAbsolute "http://example.org/knows"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.iriAbsolute "http://example.org/cindi")
              }
            , { subject = RDF.toBlankNodeOrIri (RDF.iriAbsolute "http://example.org/alice")
              , predicate = RDF.iriAbsolute "http://example.org/knows"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.iriAbsolute "http://example.org/bob")
              }
            ]
          )
        , ( [ "<http://example.org/alice> <http://example.org/knows> <http://example.org/bob> ,"
            , "                                                       <http://example.org/cindi> ."
            ]
                |> String.join "\n"
          , [ { subject = RDF.toBlankNodeOrIri (RDF.iriAbsolute "http://example.org/alice")
              , predicate = RDF.iriAbsolute "http://example.org/knows"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.iriAbsolute "http://example.org/cindi")
              }
            , { subject = RDF.toBlankNodeOrIri (RDF.iriAbsolute "http://example.org/alice")
              , predicate = RDF.iriAbsolute "http://example.org/knows"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.iriAbsolute "http://example.org/bob")
              }
            ]
          )
        , ( [ "@base <http://example.org/> ."
            , "<alice> <knows> () ."
            ]
                |> String.join "\n"
          , [ { subject = RDF.toBlankNodeOrIri (RDF.iriAbsolute "http://example.org/alice")
              , predicate = RDF.iriAbsolute "http://example.org/knows"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (rdf "nil")
              }
            ]
          )
        , ( [ "@base <http://example.org/> ."
            , "<alice> <knows> ( 1 2 ) ."
            ]
                |> String.join "\n"
          , [ { subject = RDF.toBlankNodeOrIri (RDF.blankNode "06aa2bfa-af53-42c5-aa42-a0b964471911")
              , predicate = rdf "rest"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (rdf "nil")
              }
            , { subject = RDF.toBlankNodeOrIri (RDF.blankNode "06aa2bfa-af53-42c5-aa42-a0b964471911")
              , predicate = rdf "first"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.int 2)
              }
            , { subject = RDF.toBlankNodeOrIri (RDF.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              , predicate = rdf "rest"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.blankNode "06aa2bfa-af53-42c5-aa42-a0b964471911")
              }
            , { subject = RDF.toBlankNodeOrIri (RDF.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              , predicate = rdf "first"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.int 1)
              }
            , { subject = RDF.toBlankNodeOrIri (RDF.iriAbsolute "http://example.org/alice")
              , predicate = RDF.iriAbsolute "http://example.org/knows"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              }
            ]
          )
        , ( [ "@base <http://example.org/> ."
            , "() <knows> <alice> ."
            ]
                |> String.join "\n"
          , [ { subject = RDF.toBlankNodeOrIri (rdf "nil")
              , predicate = RDF.iriAbsolute "http://example.org/knows"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.iriAbsolute "http://example.org/alice")
              }
            ]
          )
        , ( [ "@base <http://example.org/> ."
            , "( 1 2 ) <knows> <alice> ."
            ]
                |> String.join "\n"
          , [ { subject = RDF.toBlankNodeOrIri (RDF.blankNode "06aa2bfa-af53-42c5-aa42-a0b964471911")
              , predicate = rdf "rest"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (rdf "nil")
              }
            , { subject = RDF.toBlankNodeOrIri (RDF.blankNode "06aa2bfa-af53-42c5-aa42-a0b964471911")
              , predicate = rdf "first"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.int 2)
              }
            , { subject = RDF.toBlankNodeOrIri (RDF.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              , predicate = rdf "rest"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.blankNode "06aa2bfa-af53-42c5-aa42-a0b964471911")
              }
            , { subject = RDF.toBlankNodeOrIri (RDF.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              , predicate = rdf "first"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.int 1)
              }
            , { subject = RDF.toBlankNodeOrIri (RDF.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              , predicate = RDF.iriAbsolute "http://example.org/knows"
              , object = RDF.toBlankNodeOrIriOrAnyLiteral (RDF.iriAbsolute "http://example.org/alice")
              }
            ]
          )
        ]


testParseTurtle : List ( String, List RDF.NTriple ) -> Test
testParseTurtle tests =
    tests
        |> List.map (\( raw, parsed ) -> testParseTurtleCase raw parsed)
        |> describe "parse"


testParseTurtleCase : String -> List RDF.NTriple -> Test
testParseTurtleCase raw expected =
    let
        description : String
        description =
            if raw == "" then
                "<empty document>"

            else
                raw
    in
    test description <|
        \_ ->
            raw
                |> RDF.parseTurtle
                |> Expect.equal (Ok expected)
