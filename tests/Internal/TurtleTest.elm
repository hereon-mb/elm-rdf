module Internal.TurtleTest exposing (suite)

import Expect
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
          , [ Turtle.DirectivePrefixId "example" "http://example.org/"
            ]
          )
        , ( "@base <http://example.org/> ."
          , [ Turtle.DirectiveBase "http://example.org/"
            ]
          )
        , ( "PREFIX example: <http://example.org/>"
          , [ Turtle.DirectiveSparqlPrefix "example" "http://example.org/"
            ]
          )
        , ( "BASE <http://example.org/>"
          , [ Turtle.DirectiveSparqlBase "http://example.org/"
            ]
          )

        -- A
        , ( "<http://example.org/alice> a <http://example.org/#Person> ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.IriRef "http://example.org/alice"))
                    [ { verb = Turtle.A
                      , objects = [ Turtle.ObjectIri (Turtle.IriRef "http://example.org/#Person") ]
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
                      , objects = [ Turtle.ObjectIri (Turtle.IriRef "http://example.org/#bob") ]
                      }
                    ]
                )
            ]
          )
        , ( "example:alice example:knows example:bob ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "example" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "example" "knows")
                      , objects = [ Turtle.ObjectIri (Turtle.PrefixedName "example" "bob") ]
                      }
                    ]
                )
            ]
          )
        , ( "example: example: example: ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "example" ""))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "example" "")
                      , objects = [ Turtle.ObjectIri (Turtle.PrefixedName "example" "") ]
                      }
                    ]
                )
            ]
          )
        , ( ": : : ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" ""))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "")
                      , objects = [ Turtle.ObjectIri (Turtle.PrefixedName "" "") ]
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
                      , objects = [ Turtle.ObjectBlankNode (Turtle.BlankNodeLabel "bob") ]
                      }
                    ]
                )
            ]
          )
        , ( "[] example:knows [] ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectBlankNode Turtle.Anon)
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "example" "knows")
                      , objects = [ Turtle.ObjectBlankNode Turtle.Anon ]
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
                      , objects = [ Turtle.ObjectIri (Turtle.PrefixedName "example" "bob") ]
                      }
                    ]
                )
            , Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "example" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "example" "knows")
                      , objects = [ Turtle.ObjectIri (Turtle.PrefixedName "example" "cindi") ]
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
                      , objects = [ Turtle.ObjectIri (Turtle.PrefixedName "example" "bob") ]
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
                      , objects = [ Turtle.ObjectIri (Turtle.PrefixedName "example" "bob") ]
                      }
                    , { verb = Turtle.Predicate (Turtle.PrefixedName "example" "knows")
                      , objects = [ Turtle.ObjectIri (Turtle.PrefixedName "example" "cindi") ]
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
                      , objects = [ Turtle.ObjectIri (Turtle.PrefixedName "example" "bob") ]
                      }
                    , { verb = Turtle.Predicate (Turtle.PrefixedName "example" "knows")
                      , objects = [ Turtle.ObjectIri (Turtle.PrefixedName "example" "cindi") ]
                      }
                    ]
                )
            ]
          )
        , ( "[ example:knows example:bob ] example:knows example:cindi ."
          , [ Turtle.Triples
                (Turtle.TriplesBlankNodePropertyList
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "example" "knows")
                      , objects = [ Turtle.ObjectIri (Turtle.PrefixedName "example" "bob") ]
                      }
                    ]
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "example" "knows")
                      , objects = [ Turtle.ObjectIri (Turtle.PrefixedName "example" "cindi") ]
                      }
                    ]
                )
            ]
          )
        , ( "example:alice example:knows [ example:knows example:bob ] ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "example" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "example" "knows")
                      , objects =
                            [ Turtle.ObjectBlankNodePropertyList
                                [ { verb = Turtle.Predicate (Turtle.PrefixedName "example" "knows")
                                  , objects = [ Turtle.ObjectIri (Turtle.PrefixedName "example" "bob") ]
                                  }
                                ]
                            ]
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
                      , objects =
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
                      , objects =
                            [ Turtle.ObjectIri (Turtle.PrefixedName "example" "bob")
                            , Turtle.ObjectIri (Turtle.PrefixedName "example" "cindi")
                            ]
                      }
                    ]
                )
            ]
          )

        -- COLLECTION
        , ( "() a () ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectCollection [])
                    [ { verb = Turtle.A
                      , objects =
                            [ Turtle.ObjectCollection []
                            ]
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
                      , objects =
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
                      , objects = [ Turtle.ObjectLiteral (Turtle.LiteralString "Alice") ]
                      }
                    ]
                )
            ]
          )
        , ( ":alice :value \"Alice\"@en-US ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objects = [ Turtle.ObjectLiteral (Turtle.LiteralLangString "Alice" "en-US") ]
                      }
                    ]
                )
            ]
          )
        , ( ":alice :value \"Alice\"^^<http://example.org/#datatype> ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objects = [ Turtle.ObjectLiteral (Turtle.LiteralTyped "Alice" (Turtle.IriRef "http://example.org/#datatype")) ]
                      }
                    ]
                )
            ]
          )
        , ( ":alice :value 42 ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objects = [ Turtle.ObjectLiteral (Turtle.LiteralInteger 42) ]
                      }
                    ]
                )
            ]
          )
        , ( ":alice :value -42 ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objects = [ Turtle.ObjectLiteral (Turtle.LiteralInteger -42) ]
                      }
                    ]
                )
            ]
          )
        , ( ":alice :value +42 ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objects = [ Turtle.ObjectLiteral (Turtle.LiteralInteger 42) ]
                      }
                    ]
                )
            ]
          )
        , ( ":alice :value 0.314e1 ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objects = [ Turtle.ObjectLiteral (Turtle.LiteralDouble 3.14) ]
                      }
                    ]
                )
            ]
          )
        , ( ":alice :value 3.14 ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objects = [ Turtle.ObjectLiteral (Turtle.LiteralDecimal "3.14") ]
                      }
                    ]
                )
            ]
          )
        , ( ":alice :value true ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objects = [ Turtle.ObjectLiteral Turtle.LiteralTrue ]
                      }
                    ]
                )
            ]
          )
        , ( ":alice :value false ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objects = [ Turtle.ObjectLiteral Turtle.LiteralFalse ]
                      }
                    ]
                )
            ]
          )
        ]


testParse : List ( String, List Turtle.Statement ) -> Test
testParse tests =
    tests
        |> List.map (\( raw, parsed ) -> testParseCase raw parsed)
        |> describe "parse"


testParseCase : String -> List Turtle.Statement -> Test
testParseCase raw parsed =
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
                |> Turtle.parse
                |> Expect.equal (Ok parsed)
