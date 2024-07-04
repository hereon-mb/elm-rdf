module RDF.DecodeTest exposing (suite)

import Expect
import RDF
import RDF.Decode as Decode exposing (Decoder, decode)
import RDF.Graph as Graph
import RDF.PropertyPath as RDF
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "RDF.Decode"
        [ bool
        , many
        , manyMany
        ]


bool : Test
bool =
    decodeLiteral "bool" Decode.bool (RDF.bool True) True


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
                                (RDF.PredicatePath (example "label"))
                                (Decode.many Decode.string)
                            )
                            graph
                            [ RDF.toBlankNodeOrIriOrAnyLiteral (example "x") ]
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
                                (RDF.PredicatePath (example "label"))
                                (Decode.map List.concat (Decode.many (Decode.many Decode.string)))
                            )
                            graph
                            [ RDF.toBlankNodeOrIriOrAnyLiteral (example "x") ]
                            |> Result.map List.sort
                    )
                |> Expect.equal (Ok (Ok [ "a", "b" ]))


example : String -> RDF.Iri
example name =
    RDF.iri ("http://example.org/" ++ name)


decodeLiteral : String -> Decoder a -> RDF.Literal compatible -> a -> Test
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
                    case decode decoder graph [ RDF.toBlankNodeOrIriOrAnyLiteral value ] of
                        Err errorDecode ->
                            Expect.fail (Decode.errorToString errorDecode)

                        Ok result ->
                            result
                                |> Expect.equal expected
