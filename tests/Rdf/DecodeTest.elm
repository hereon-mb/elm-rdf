module Rdf.DecodeTest exposing (suite)

import Expect
import Rdf
import Rdf.Decode as Decode exposing (Decoder, decode)
import Rdf.Graph as Graph
import Rdf.Namespaces as Rdf
import Rdf.PropertyPath as Rdf
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Rdf.Decode"
        [ bool
        , many
        , manyMany
        , stringOrLangString
        ]


bool : Test
bool =
    decodeLiteral "bool" Decode.bool (Rdf.bool True) True


many : Test
many =
    test "many" <|
        \_ ->
            """
            @base <http://example.org/> .
            <x> <label> "a", "b" .
            """
                |> Graph.parse
                |> Result.map
                    (\graph ->
                        Decode.decode
                            (Decode.property
                                (Rdf.PredicatePath (example "label"))
                                (Decode.many Decode.string)
                            )
                            graph
                            [ Rdf.asBlankNodeOrIriOrAnyLiteral (example "x") ]
                            |> Result.map List.sort
                    )
                |> Expect.equal (Ok (Ok [ "a", "b" ]))


manyMany : Test
manyMany =
    test "many many" <|
        \_ ->
            """
            @base <http://example.org/> .
            <x> <label> "a", "b" .
            """
                |> Graph.parse
                |> Result.map
                    (\graph ->
                        Decode.decode
                            (Decode.property
                                (Rdf.PredicatePath (example "label"))
                                (Decode.map List.concat (Decode.many (Decode.many Decode.string)))
                            )
                            graph
                            [ Rdf.asBlankNodeOrIriOrAnyLiteral (example "x") ]
                            |> Result.map List.sort
                    )
                |> Expect.equal (Ok (Ok [ "a", "b" ]))


stringOrLangString : Test
stringOrLangString =
    test "string or lang string" <|
        \_ ->
            """
            @base <http://example.org/> .
            @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
            <x> rdfs:label "de", "en"@en, "fr"@fr .
            """
                |> Graph.parse
                |> Result.map
                    (\graph ->
                        Decode.decode
                            (Decode.property (Rdf.PredicatePath (Rdf.rdfs "label"))
                                Decode.stringOrLangString
                            )
                            graph
                            [ Rdf.asBlankNodeOrIriOrAnyLiteral (example "x") ]
                    )
                |> Expect.equal
                    (Ok
                        (Ok
                            (Rdf.stringOrLangStringFrom (Just "de")
                                [ ( "en", "en" )
                                , ( "fr", "fr" )
                                ]
                            )
                        )
                    )


example : String -> Rdf.Iri
example name =
    Rdf.iri ("http://example.org/" ++ name)


decodeLiteral : String -> Decoder a -> Rdf.Literal compatible -> a -> Test
decodeLiteral description decoder value expected =
    test description <|
        \_ ->
            let
                raw : String
                raw =
                    ""
            in
            case Graph.parse raw of
                Err errorGraph ->
                    Expect.fail (Graph.errorToString raw errorGraph)

                Ok graph ->
                    case decode decoder graph [ Rdf.asBlankNodeOrIriOrAnyLiteral value ] of
                        Err errorDecode ->
                            Expect.fail (Decode.errorToString errorDecode)

                        Ok result ->
                            result
                                |> Expect.equal expected
