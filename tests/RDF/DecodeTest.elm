module RDF.DecodeTest exposing (suite)

import Expect
import RDF
import RDF.Decode as Decode exposing (Decoder, decode)
import RDF.Graph as Graph
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "RDF.Decode"
        [ bool
        ]


bool : Test
bool =
    decodeLiteral "bool" Decode.bool (RDF.bool True) True


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
                    case decode decoder graph (RDF.toBlankNodeOrIriOrAnyLiteral value) of
                        Err errorDecode ->
                            Expect.fail (Decode.errorToString errorDecode)

                        Ok result ->
                            result
                                |> Expect.equal expected
