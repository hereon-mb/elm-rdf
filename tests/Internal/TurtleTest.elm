module Internal.TurtleTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Internal.Turtle as Turtle
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Internal.Turtle"
        [ parse
        ]


parse : Test
parse =
    testParse
        [ ( ""
          , []
          )
        , ( "@prefix example: <http://example.org/> ."
          , [ Turtle.Directive
                (Turtle.PrefixId "example" "http://example.org/")
            ]
          )
        , ( "@base <http://example.org/> ."
          , [ Turtle.Directive
                (Turtle.Base "http://example.org/")
            ]
          )
        , ( "PREFIX example: <http://example.org/>"
          , [ Turtle.Directive
                (Turtle.SparqlPrefix "example" "http://example.org/")
            ]
          )
        , ( "BASE <http://example.org/>"
          , [ Turtle.Directive
                (Turtle.SparqlBase "http://example.org/")
            ]
          )
        , ( "<http://example.org/alice> a <http://example.org/#Person> ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.IriRef "http://example.org/alice"))
                    [ { verb = Turtle.A
                      , objectList = [ Turtle.ObjectIri (Turtle.IriRef "http://example.org/#Person") ]
                      }
                    ]
                )
            ]
          )
        , ( "<http://example.org/alice> <http://example.org/#name> <http://example.org/#Person> ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.IriRef "http://example.org/alice"))
                    [ { verb = Turtle.Predicate (Turtle.IriRef "http://example.org/#name")
                      , objectList = [ Turtle.ObjectIri (Turtle.IriRef "http://example.org/#Person") ]
                      }
                    ]
                )
            ]
          )
        , ( "_:alice <http://example.org/#name> _:Person ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectBlankNode (Turtle.BlankNodeLabel "alice"))
                    [ { verb = Turtle.Predicate (Turtle.IriRef "http://example.org/#name")
                      , objectList = [ Turtle.ObjectBlankNode (Turtle.BlankNodeLabel "Person") ]
                      }
                    ]
                )
            ]
          )
        , ( "(_:a _:b) a (_:c _:d) ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject
                    (Turtle.SubjectCollection
                        [ Turtle.ObjectBlankNode (Turtle.BlankNodeLabel "a")
                        , Turtle.ObjectBlankNode (Turtle.BlankNodeLabel "b")
                        ]
                    )
                    [ { verb = Turtle.A
                      , objectList =
                            [ Turtle.ObjectCollection
                                [ Turtle.ObjectBlankNode (Turtle.BlankNodeLabel "c")
                                , Turtle.ObjectBlankNode (Turtle.BlankNodeLabel "d")
                                ]
                            ]
                      }
                    ]
                )
            ]
          )
        , ( "<http://example.org/alice> <http://example.org/#name> \"Alice\" ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.IriRef "http://example.org/alice"))
                    [ { verb = Turtle.Predicate (Turtle.IriRef "http://example.org/#name")
                      , objectList = [ Turtle.ObjectLiteral (Turtle.RdfLiteralString "Alice") ]
                      }
                    ]
                )
            ]
          )
        , ( "<http://example.org/alice> <http://example.org/#name> \"Alice\"@en-US ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.IriRef "http://example.org/alice"))
                    [ { verb = Turtle.Predicate (Turtle.IriRef "http://example.org/#name")
                      , objectList = [ Turtle.ObjectLiteral (Turtle.RdfLiteralLangString "Alice" "en-US") ]
                      }
                    ]
                )
            ]
          )
        , ( "<http://example.org/alice> <http://example.org/#name> \"Alice\"^^<http://example.org/#datatype> ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.IriRef "http://example.org/alice"))
                    [ { verb = Turtle.Predicate (Turtle.IriRef "http://example.org/#name")
                      , objectList = [ Turtle.ObjectLiteral (Turtle.RdfLiteralTyped "Alice" (Turtle.IriRef "http://example.org/#datatype")) ]
                      }
                    ]
                )
            ]
          )
        ]


testParse : List ( String, Turtle.TurtleDoc ) -> Test
testParse tests =
    tests
        |> List.map (\( raw, parsed ) -> testParseCase raw parsed)
        |> describe "parse"


testParseCase : String -> Turtle.TurtleDoc -> Test
testParseCase raw parsed =
    let
        description =
            if raw == "" then
                "<empty document>"

            else
                raw
    in
    test description <|
        \_ ->
            raw
                |> Turtle.parse
                |> Expect.equal (Ok parsed)
