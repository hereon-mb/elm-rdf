module RdfTest exposing (suite)

import Expect
import Rdf
import Rdf.Graph
import Rdf.Namespaces exposing (rdf, xsd)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Rdf"
        [ parse
        , describe "utils"
            [ appendPath
            , dropFragment
            , setFragment
            ]
        ]


parse : Test
parse =
    testParse
        [ ( ""
          , []
          )
        , ( "<http://example.org/alice> <http://example.org/knows> <http://example.org/bob> ."
          , [ { subject = Rdf.asBlankNodeOrIri (Rdf.iri "http://example.org/alice")
              , predicate = Rdf.iri "http://example.org/knows"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.iri "http://example.org/bob")
              }
            ]
          )
        , ( [ "@prefix example: <http://example.org/> ."
            , "example:alice example:knows example:bob ."
            ]
                |> String.join "\n"
          , [ { subject = Rdf.asBlankNodeOrIri (Rdf.iri "http://example.org/alice")
              , predicate = Rdf.iri "http://example.org/knows"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.iri "http://example.org/bob")
              }
            ]
          )
        , ( "[] <http://example.org/knows> <http://example.org/bob> ."
          , [ { subject = Rdf.asBlankNodeOrIri (Rdf.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              , predicate = Rdf.iri "http://example.org/knows"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.iri "http://example.org/bob")
              }
            ]
          )
        , ( [ "[ <http://example.org/knows> <http://example.org/bob> ]"
            , "    <http://example.org/knows> <http://example.org/cindi> ."
            ]
                |> String.join "\n"
          , [ { subject = Rdf.asBlankNodeOrIri (Rdf.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              , predicate = Rdf.iri "http://example.org/knows"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.iri "http://example.org/cindi")
              }
            , { subject = Rdf.asBlankNodeOrIri (Rdf.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              , predicate = Rdf.iri "http://example.org/knows"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.iri "http://example.org/bob")
              }
            ]
          )
        , ( "_:alice <http://example.org/knows> <http://example.org/bob> ."
          , [ { subject = Rdf.asBlankNodeOrIri (Rdf.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              , predicate = Rdf.iri "http://example.org/knows"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.iri "http://example.org/bob")
              }
            ]
          )
        , ( [ "_:alice <http://example.org/knows> <http://example.org/bob> ."
            , "_:alice <http://example.org/knows> <http://example.org/cindi> ."
            ]
                |> String.join "\n"
          , [ { subject = Rdf.asBlankNodeOrIri (Rdf.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              , predicate = Rdf.iri "http://example.org/knows"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.iri "http://example.org/cindi")
              }
            , { subject = Rdf.asBlankNodeOrIri (Rdf.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              , predicate = Rdf.iri "http://example.org/knows"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.iri "http://example.org/bob")
              }
            ]
          )
        , ( "<http://example.org/alice> <http://example.org/knows> [] ."
          , [ { subject = Rdf.asBlankNodeOrIri (Rdf.iri "http://example.org/alice")
              , predicate = Rdf.iri "http://example.org/knows"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              }
            ]
          )
        , ( [ "<http://example.org/alice> <http://example.org/knows> ["
            , "    <http://example.org/knows> <http://example.org/bob>"
            , "] ."
            ]
                |> String.join "\n"
          , [ { subject = Rdf.asBlankNodeOrIri (Rdf.iri "http://example.org/alice")
              , predicate = Rdf.iri "http://example.org/knows"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              }
            , { subject = Rdf.asBlankNodeOrIri (Rdf.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              , predicate = Rdf.iri "http://example.org/knows"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.iri "http://example.org/bob")
              }
            ]
          )
        , ( "<http://example.org/alice> <http://example.org/knows> _:bob ."
          , [ { subject = Rdf.asBlankNodeOrIri (Rdf.iri "http://example.org/alice")
              , predicate = Rdf.iri "http://example.org/knows"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              }
            ]
          )
        , ( [ "@base <http://example.org/> ."
            , "<alice> <knows> <bob> ."
            ]
                |> String.join "\n"
          , [ { subject = Rdf.asBlankNodeOrIri (Rdf.iri "http://example.org/alice")
              , predicate = Rdf.iri "http://example.org/knows"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.iri "http://example.org/bob")
              }
            ]
          )
        , ( [ "@base <http://example.org/> ."
            , "<alice> <value> \"Alice\" ."
            ]
                |> String.join "\n"
          , [ { subject = Rdf.asBlankNodeOrIri (Rdf.iri "http://example.org/alice")
              , predicate = Rdf.iri "http://example.org/value"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.string "Alice")
              }
            ]
          )
        , ( [ "@base <http://example.org/> ."
            , "<alice> <value> \"Alice\"@en ."
            ]
                |> String.join "\n"
          , [ { subject = Rdf.asBlankNodeOrIri (Rdf.iri "http://example.org/alice")
              , predicate = Rdf.iri "http://example.org/value"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.langString "en" "Alice")
              }
            ]
          )
        , ( [ "@base <http://example.org/> ."
            , "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> ."
            , "<alice> <value> \"42\"^^xsd:integer ."
            ]
                |> String.join "\n"
          , [ { subject = Rdf.asBlankNodeOrIri (Rdf.iri "http://example.org/alice")
              , predicate = Rdf.iri "http://example.org/value"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.literal (xsd "integer") "42")
              }
            ]
          )
        , ( [ "@base <http://example.org/> ."
            , "<alice> <value> 42 ."
            ]
                |> String.join "\n"
          , [ { subject = Rdf.asBlankNodeOrIri (Rdf.iri "http://example.org/alice")
              , predicate = Rdf.iri "http://example.org/value"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.literal (xsd "integer") "42")
              }
            ]
          )
        , ( [ "@base <http://example.org/> ."
            , "<alice> <value> 3.14 ."
            ]
                |> String.join "\n"
          , [ { subject = Rdf.asBlankNodeOrIri (Rdf.iri "http://example.org/alice")
              , predicate = Rdf.iri "http://example.org/value"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.literal (xsd "decimal") "3.14")
              }
            ]
          )
        , ( [ "@base <http://example.org/> ."
            , "<alice> <value> 0.314e1 ."
            ]
                |> String.join "\n"
          , [ { subject = Rdf.asBlankNodeOrIri (Rdf.iri "http://example.org/alice")
              , predicate = Rdf.iri "http://example.org/value"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.literal (xsd "double") "3.14")
              }
            ]
          )
        , ( [ "@base <http://example.org/> ."
            , "<alice> <value> true ."
            ]
                |> String.join "\n"
          , [ { subject = Rdf.asBlankNodeOrIri (Rdf.iri "http://example.org/alice")
              , predicate = Rdf.iri "http://example.org/value"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.literal (xsd "boolean") "true")
              }
            ]
          )
        , ( [ "@base <http://example.org/> ."
            , "<alice> <value> false ."
            ]
                |> String.join "\n"
          , [ { subject = Rdf.asBlankNodeOrIri (Rdf.iri "http://example.org/alice")
              , predicate = Rdf.iri "http://example.org/value"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.literal (xsd "boolean") "false")
              }
            ]
          )
        , ( [ "<http://example.org/alice> <http://example.org/knows> <http://example.org/bob> ."
            , "<http://example.org/alice> <http://example.org/knows> <http://example.org/cindi> ."
            ]
                |> String.join "\n"
          , [ { subject = Rdf.asBlankNodeOrIri (Rdf.iri "http://example.org/alice")
              , predicate = Rdf.iri "http://example.org/knows"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.iri "http://example.org/cindi")
              }
            , { subject = Rdf.asBlankNodeOrIri (Rdf.iri "http://example.org/alice")
              , predicate = Rdf.iri "http://example.org/knows"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.iri "http://example.org/bob")
              }
            ]
          )
        , ( [ "<http://example.org/alice> <http://example.org/knows> <http://example.org/bob> ;"
            , "                           <http://example.org/knows> <http://example.org/cindi> ."
            ]
                |> String.join "\n"
          , [ { subject = Rdf.asBlankNodeOrIri (Rdf.iri "http://example.org/alice")
              , predicate = Rdf.iri "http://example.org/knows"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.iri "http://example.org/cindi")
              }
            , { subject = Rdf.asBlankNodeOrIri (Rdf.iri "http://example.org/alice")
              , predicate = Rdf.iri "http://example.org/knows"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.iri "http://example.org/bob")
              }
            ]
          )
        , ( [ "<http://example.org/alice> <http://example.org/knows> <http://example.org/bob> ,"
            , "                                                       <http://example.org/cindi> ."
            ]
                |> String.join "\n"
          , [ { subject = Rdf.asBlankNodeOrIri (Rdf.iri "http://example.org/alice")
              , predicate = Rdf.iri "http://example.org/knows"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.iri "http://example.org/cindi")
              }
            , { subject = Rdf.asBlankNodeOrIri (Rdf.iri "http://example.org/alice")
              , predicate = Rdf.iri "http://example.org/knows"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.iri "http://example.org/bob")
              }
            ]
          )
        , ( [ "@base <http://example.org/> ."
            , "<alice> <knows> () ."
            ]
                |> String.join "\n"
          , [ { subject = Rdf.asBlankNodeOrIri (Rdf.iri "http://example.org/alice")
              , predicate = Rdf.iri "http://example.org/knows"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (rdf "nil")
              }
            ]
          )
        , ( [ "@base <http://example.org/> ."
            , "<alice> <knows> ( 1 2 ) ."
            ]
                |> String.join "\n"
          , [ { subject = Rdf.asBlankNodeOrIri (Rdf.blankNode "06aa2bfa-af53-42c5-aa42-a0b964471911")
              , predicate = rdf "rest"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (rdf "nil")
              }
            , { subject = Rdf.asBlankNodeOrIri (Rdf.blankNode "06aa2bfa-af53-42c5-aa42-a0b964471911")
              , predicate = rdf "first"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.int 2)
              }
            , { subject = Rdf.asBlankNodeOrIri (Rdf.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              , predicate = rdf "rest"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.blankNode "06aa2bfa-af53-42c5-aa42-a0b964471911")
              }
            , { subject = Rdf.asBlankNodeOrIri (Rdf.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              , predicate = rdf "first"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.int 1)
              }
            , { subject = Rdf.asBlankNodeOrIri (Rdf.iri "http://example.org/alice")
              , predicate = Rdf.iri "http://example.org/knows"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              }
            ]
          )
        , ( [ "@base <http://example.org/> ."
            , "() <knows> <alice> ."
            ]
                |> String.join "\n"
          , [ { subject = Rdf.asBlankNodeOrIri (rdf "nil")
              , predicate = Rdf.iri "http://example.org/knows"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.iri "http://example.org/alice")
              }
            ]
          )
        , ( [ "@base <http://example.org/> ."
            , "( 1 2 ) <knows> <alice> ."
            ]
                |> String.join "\n"
          , [ { subject = Rdf.asBlankNodeOrIri (Rdf.blankNode "06aa2bfa-af53-42c5-aa42-a0b964471911")
              , predicate = rdf "rest"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (rdf "nil")
              }
            , { subject = Rdf.asBlankNodeOrIri (Rdf.blankNode "06aa2bfa-af53-42c5-aa42-a0b964471911")
              , predicate = rdf "first"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.int 2)
              }
            , { subject = Rdf.asBlankNodeOrIri (Rdf.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              , predicate = rdf "rest"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.blankNode "06aa2bfa-af53-42c5-aa42-a0b964471911")
              }
            , { subject = Rdf.asBlankNodeOrIri (Rdf.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              , predicate = rdf "first"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.int 1)
              }
            , { subject = Rdf.asBlankNodeOrIri (Rdf.blankNode "2273e9c9-fa2d-4c28-ae87-947ad36cbecd")
              , predicate = Rdf.iri "http://example.org/knows"
              , object = Rdf.asBlankNodeOrIriOrAnyLiteral (Rdf.iri "http://example.org/alice")
              }
            ]
          )
        ]


testParse : List ( String, List Rdf.NTriple ) -> Test
testParse tests =
    tests
        |> List.map (\( raw, parsed ) -> testParseCase raw parsed)
        |> describe "parse"


testParseCase : String -> List Rdf.NTriple -> Test
testParseCase raw expected =
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
                |> Rdf.Graph.parse
                |> Result.map (Rdf.Graph.clearBase >> Rdf.Graph.clearPrefixes)
                |> Expect.equal (Ok (Rdf.Graph.fromNTriples expected))


appendPath : Test
appendPath =
    describe "appendPath"
        [ test "without fragment or query" <|
            \_ ->
                "http://example.org/"
                    |> Rdf.iri
                    |> Rdf.appendPath "segment"
                    |> Expect.equal (Rdf.iri "http://example.org/segment")
        , test "with query" <|
            \_ ->
                "http://example.org/?query"
                    |> Rdf.iri
                    |> Rdf.appendPath "segment"
                    |> Expect.equal (Rdf.iri "http://example.org/segment?query")
        , test "with fragment" <|
            \_ ->
                "http://example.org/#fragment"
                    |> Rdf.iri
                    |> Rdf.appendPath "segment"
                    |> Expect.equal (Rdf.iri "http://example.org/segment#fragment")
        , test "with query and fragment" <|
            \_ ->
                "http://example.org/?query#fragment"
                    |> Rdf.iri
                    |> Rdf.appendPath "segment"
                    |> Expect.equal (Rdf.iri "http://example.org/segment?query#fragment")
        ]


dropFragment : Test
dropFragment =
    describe "dropFragment"
        [ test "with fragment" <|
            \_ ->
                "http://example.org/#fragment"
                    |> Rdf.iri
                    |> Rdf.dropFragment
                    |> Expect.equal (Rdf.iri "http://example.org/")
        , test "without fragment" <|
            \_ ->
                "http://example.org/"
                    |> Rdf.iri
                    |> Rdf.dropFragment
                    |> Expect.equal (Rdf.iri "http://example.org/")
        ]


setFragment : Test
setFragment =
    describe "setFragment"
        [ test "with fragment" <|
            \_ ->
                "http://example.org/#original"
                    |> Rdf.iri
                    |> Rdf.setFragment "fragment"
                    |> Expect.equal (Rdf.iri "http://example.org/#fragment")
        , test "without fragment" <|
            \_ ->
                "http://example.org/"
                    |> Rdf.iri
                    |> Rdf.setFragment "fragment"
                    |> Expect.equal (Rdf.iri "http://example.org/#fragment")
        ]
