module Rdf.DecodeTest exposing (suite)

import Expect exposing (Expectation)
import Rdf
import Rdf.Decode as Decode exposing (Decoder)
import Rdf.Graph as Graph
import Rdf.Namespaces as Rdf
import Rdf.PropertyPath as Rdf
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Rdf.Decode"
        [ many
        , manyMany
        , stringOrLangString
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
                        (Rdf.PredicatePath (example "label"))
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
                        (Rdf.PredicatePath (example "label"))
                        (Decode.map List.concat (Decode.many (Decode.many Decode.string)))
                    )
            }
                |> expectAll [ List.sort >> Expect.equalLists [ "a", "b" ] ]


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
                    (Decode.property (Rdf.PredicatePath (Rdf.rdfs "label"))
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


example : String -> Rdf.Iri
example name =
    Rdf.iri ("http://example.org/" ++ name)


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
