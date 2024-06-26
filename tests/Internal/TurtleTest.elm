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

        -- A
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

        -- IRI
        , ( "<http://example.org/alice> <http://example.org/#knows> <http://example.org/#bob> ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.IriRef "http://example.org/alice"))
                    [ { verb = Turtle.Predicate (Turtle.IriRef "http://example.org/#knows")
                      , objectList = [ Turtle.ObjectIri (Turtle.IriRef "http://example.org/#bob") ]
                      }
                    ]
                )
            ]
          )
        , ( "example:alice example:knows example:bob ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "example" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "example" "knows")
                      , objectList = [ Turtle.ObjectIri (Turtle.PrefixedName "example" "bob") ]
                      }
                    ]
                )
            ]
          )
        , ( "example: example: example: ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "example" ""))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "example" "")
                      , objectList = [ Turtle.ObjectIri (Turtle.PrefixedName "example" "") ]
                      }
                    ]
                )
            ]
          )
        , ( ": : : ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" ""))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "")
                      , objectList = [ Turtle.ObjectIri (Turtle.PrefixedName "" "") ]
                      }
                    ]
                )
            ]
          )

        -- BLANK NODE
        , ( "_:alice example:knows _:bob ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectBlankNode (Turtle.BlankNodeLabel "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "example" "knows")
                      , objectList = [ Turtle.ObjectBlankNode (Turtle.BlankNodeLabel "bob") ]
                      }
                    ]
                )
            ]
          )
        , ( "[] example:knows [] ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectBlankNode Turtle.Anon)
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "example" "knows")
                      , objectList = [ Turtle.ObjectBlankNode Turtle.Anon ]
                      }
                    ]
                )
            ]
          )

        -- TRIPLES
        , ( "example:alice example:knows example:bob .  example:alice example:knows example:cindi ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "example" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "example" "knows")
                      , objectList = [ Turtle.ObjectIri (Turtle.PrefixedName "example" "bob") ]
                      }
                    ]
                )
            , Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "example" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "example" "knows")
                      , objectList = [ Turtle.ObjectIri (Turtle.PrefixedName "example" "cindi") ]
                      }
                    ]
                )
            ]
          )

        -- PREDICATE OBJECT LIST
        , ( "[ example:knows example:bob ] ."
          , [ Turtle.Triples
                (Turtle.TriplesBlankNodePropertyList
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "example" "knows")
                      , objectList = [ Turtle.ObjectIri (Turtle.PrefixedName "example" "bob") ]
                      }
                    ]
                    []
                )
            ]
          )
        , ( "[ example:knows example:bob ; example:knows example:cindi ] ."
          , [ Turtle.Triples
                (Turtle.TriplesBlankNodePropertyList
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "example" "knows")
                      , objectList = [ Turtle.ObjectIri (Turtle.PrefixedName "example" "bob") ]
                      }
                    , { verb = Turtle.Predicate (Turtle.PrefixedName "example" "knows")
                      , objectList = [ Turtle.ObjectIri (Turtle.PrefixedName "example" "cindi") ]
                      }
                    ]
                    []
                )
            ]
          )
        , ( "[] example:knows example:bob ; example:knows example:cindi ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectBlankNode Turtle.Anon)
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "example" "knows")
                      , objectList = [ Turtle.ObjectIri (Turtle.PrefixedName "example" "bob") ]
                      }
                    , { verb = Turtle.Predicate (Turtle.PrefixedName "example" "knows")
                      , objectList = [ Turtle.ObjectIri (Turtle.PrefixedName "example" "cindi") ]
                      }
                    ]
                )
            ]
          )
        , ( "[ example:knows example:bob ] example:knows example:cindi ."
          , [ Turtle.Triples
                (Turtle.TriplesBlankNodePropertyList
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "example" "knows")
                      , objectList = [ Turtle.ObjectIri (Turtle.PrefixedName "example" "bob") ]
                      }
                    ]
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "example" "knows")
                      , objectList = [ Turtle.ObjectIri (Turtle.PrefixedName "example" "cindi") ]
                      }
                    ]
                )
            ]
          )

        -- OBJECT LIST
        , ( "[ example:knows example:bob , example:cindi ] ."
          , [ Turtle.Triples
                (Turtle.TriplesBlankNodePropertyList
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "example" "knows")
                      , objectList =
                            [ Turtle.ObjectIri (Turtle.PrefixedName "example" "bob")
                            , Turtle.ObjectIri (Turtle.PrefixedName "example" "cindi")
                            ]
                      }
                    ]
                    []
                )
            ]
          )
        , ( "[] example:knows example:bob , example:cindi ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectBlankNode Turtle.Anon)
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "example" "knows")
                      , objectList =
                            [ Turtle.ObjectIri (Turtle.PrefixedName "example" "bob")
                            , Turtle.ObjectIri (Turtle.PrefixedName "example" "cindi")
                            ]
                      }
                    ]
                )
            ]
          )

        -- COLLECTION
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

        -- LITERAL
        , ( ":alice :value \"Alice\" ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objectList = [ Turtle.ObjectLiteral (Turtle.RdfLiteralString "Alice") ]
                      }
                    ]
                )
            ]
          )
        , ( ":alice :value \"Alice\"@en-US ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objectList = [ Turtle.ObjectLiteral (Turtle.RdfLiteralLangString "Alice" "en-US") ]
                      }
                    ]
                )
            ]
          )
        , ( ":alice :value \"Alice\"^^<http://example.org/#datatype> ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objectList = [ Turtle.ObjectLiteral (Turtle.RdfLiteralTyped "Alice" (Turtle.IriRef "http://example.org/#datatype")) ]
                      }
                    ]
                )
            ]
          )
        , ( ":alice :value 42 ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objectList = [ Turtle.ObjectLiteral (Turtle.NumericLiteral (Turtle.Integer 42)) ]
                      }
                    ]
                )
            ]
          )
        , ( ":alice :value -42 ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objectList = [ Turtle.ObjectLiteral (Turtle.NumericLiteral (Turtle.Integer -42)) ]
                      }
                    ]
                )
            ]
          )
        , ( ":alice :value +42 ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objectList = [ Turtle.ObjectLiteral (Turtle.NumericLiteral (Turtle.Integer 42)) ]
                      }
                    ]
                )
            ]
          )
        , ( ":alice :value 0.314e1 ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objectList = [ Turtle.ObjectLiteral (Turtle.NumericLiteral (Turtle.Double 3.14)) ]
                      }
                    ]
                )
            ]
          )
        , ( ":alice :value 3.14 ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objectList = [ Turtle.ObjectLiteral (Turtle.NumericLiteral (Turtle.Decimal "3.14")) ]
                      }
                    ]
                )
            ]
          )
        , ( ":alice :value true ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objectList = [ Turtle.ObjectLiteral (Turtle.BooleanLiteral Turtle.BooleanLiteralTrue) ]
                      }
                    ]
                )
            ]
          )
        , ( ":alice :value false ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objectList = [ Turtle.ObjectLiteral (Turtle.BooleanLiteral Turtle.BooleanLiteralFalse) ]
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
