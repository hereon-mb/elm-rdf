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
        , ( "@prefix : <http://example.org/> ."
          , [ Turtle.DirectivePrefixId "" "http://example.org/"
            ]
          )
        , ( "@prefix : <http://example.org/#> ."
          , [ Turtle.DirectivePrefixId "" "http://example.org/#"
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
        , ( "PREFIX example: <http://example.org/#>"
          , [ Turtle.DirectiveSparqlPrefix "example" "http://example.org/#"
            ]
          )
        , ( "PREFIX : <http://example.org/#>"
          , [ Turtle.DirectiveSparqlPrefix "" "http://example.org/#"
            ]
          )
        , ( "BASE <http://example.org/>"
          , [ Turtle.DirectiveSparqlBase "http://example.org/"
            ]
          )
        , ( [ "@base <http://purls.helmholtz-metadaten.de/herbie/pm/pre-cross-linking/1.0.0/> ."
            , "@prefix : <http://purls.helmholtz-metadaten.de/herbie/pm/pre-cross-linking/#> ."
            , "@prefix dash: <http://datashapes.org/dash#> ."
            ]
                |> String.join "\n"
          , [ Turtle.DirectiveBase "http://purls.helmholtz-metadaten.de/herbie/pm/pre-cross-linking/1.0.0/"
            , Turtle.DirectivePrefixId "" "http://purls.helmholtz-metadaten.de/herbie/pm/pre-cross-linking/#"
            , Turtle.DirectivePrefixId "dash" "http://datashapes.org/dash#"
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
        , ( "<http://example.org/\\u00F6> <http://example.org/#knows> <http://example.org/#bob> ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.IriRef "http://example.org/ö"))
                    [ { verb = Turtle.Predicate (Turtle.IriRef "http://example.org/#knows")
                      , objects = [ Turtle.ObjectIri (Turtle.IriRef "http://example.org/#bob") ]
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
        , ( "[] example:knows example:, example:cindi ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectBlankNode Turtle.Anon)
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "example" "knows")
                      , objects =
                            [ Turtle.ObjectIri (Turtle.PrefixedName "example" "")
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
        , ( ":alice :knows (_:a _:b) ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject
                    (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "knows")
                      , objects =
                            [ Turtle.ObjectCollection
                                [ Turtle.ObjectBlankNode (Turtle.BlankNodeLabel "a")
                                , Turtle.ObjectBlankNode (Turtle.BlankNodeLabel "b")
                                ]
                            ]
                      }
                    ]
                )
            ]
          )
        , ( ":alice :knows (:cindi :bob) ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject
                    (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "knows")
                      , objects =
                            [ Turtle.ObjectCollection
                                [ Turtle.ObjectIri (Turtle.PrefixedName "" "cindi")
                                , Turtle.ObjectIri (Turtle.PrefixedName "" "bob")
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
        , ( ":alice :value \"\"\"Alice\"\"\" ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objects = [ Turtle.ObjectLiteral (Turtle.LiteralString "Alice") ]
                      }
                    ]
                )
            ]
          )
        , ( [ ":alice :value \"\"\"Alice"
            , "Wonderland\"\"\" ."
            ]
                |> String.join "\n"
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objects = [ Turtle.ObjectLiteral (Turtle.LiteralString "Alice\nWonderland") ]
                      }
                    ]
                )
            ]
          )
        , ( [ ":alice :value \"\"\"Alice\\\"Wonderland\"\"\" ."
            ]
                |> String.join "\n"
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objects = [ Turtle.ObjectLiteral (Turtle.LiteralString "Alice\"Wonderland") ]
                      }
                    ]
                )
            ]
          )
        , ( [ ":alice :value \"\"\"Alice\"Wonderland\"\"\" ."
            ]
                |> String.join "\n"
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objects = [ Turtle.ObjectLiteral (Turtle.LiteralString "Alice\"Wonderland") ]
                      }
                    ]
                )
            ]
          )
        , ( [ ":alice :value \"\"\"Alice\"\"Wonderland\"\"\" ."
            ]
                |> String.join "\n"
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objects = [ Turtle.ObjectLiteral (Turtle.LiteralString "Alice\"\"Wonderland") ]
                      }
                    ]
                )
            ]
          )
        , ( [ ":alice :value \"\"\"AliceWonderland\\\"\"\"\" ."
            ]
                |> String.join "\n"
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objects = [ Turtle.ObjectLiteral (Turtle.LiteralString "AliceWonderland\"") ]
                      }
                    ]
                )
            ]
          )
        , ( [ ":alice :value \"\"\"Alice\\nWonderland\"\"\" ."
            ]
                |> String.join "\n"
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objects = [ Turtle.ObjectLiteral (Turtle.LiteralString "Alice\nWonderland") ]
                      }
                    ]
                )
            ]
          )
        , ( [ ":alice :value \"\"\""
            , "  Alice"
            , "  Wonderland"
            , "\"\"\" ."
            ]
                |> String.join "\n"
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objects = [ Turtle.ObjectLiteral (Turtle.LiteralString "\n  Alice\n  Wonderland\n") ]
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
        , ( ":alice :value 42."
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
        , ( ":alice :value 0.314e+1 ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objects = [ Turtle.ObjectLiteral (Turtle.LiteralDouble 3.14) ]
                      }
                    ]
                )
            ]
          )
        , ( ":alice :value 0.314e-1 ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objects = [ Turtle.ObjectLiteral (Turtle.LiteralDouble 0.0314) ]
                      }
                    ]
                )
            ]
          )
        , ( ":alice :value 0.314e0 ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.PrefixedName "" "alice"))
                    [ { verb = Turtle.Predicate (Turtle.PrefixedName "" "value")
                      , objects = [ Turtle.ObjectLiteral (Turtle.LiteralDouble 0.314) ]
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

        -- COMMENTS
        , ( "# comment"
          , []
          )
        , ( [ "<http://example.org/alice>   # comment"
            , "<http://example.org/#knows>  # comment"
            , "<http://example.org/#bob> .  # comment"
            ]
                |> String.join "\n"
          , [ Turtle.Triples
                (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.IriRef "http://example.org/alice"))
                    [ { verb = Turtle.Predicate (Turtle.IriRef "http://example.org/#knows")
                      , objects = [ Turtle.ObjectIri (Turtle.IriRef "http://example.org/#bob") ]
                      }
                    ]
                )
            ]
          )
        , ( [ "[ example:knows    # comment"
            , "    example:bob ;  # comment"
            , "  example:knows    # comment"
            , "    example:cindi  # comment"
            , "] ."
            ]
                |> String.join "\n"
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
        , ( [ "[ example:knows"
            , "# comment"
            , "# comment"
            , "    example:bob ;"
            , " # comment"
            , " # comment"
            , "  example:knows"
            , "#comment"
            , "#comment"
            , "    example:cindi"
            , "# comment"
            , "# comment"
            , "] ."
            ]
                |> String.join "\n"
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
        , ( "[] example:knows [ ] ."
          , [ Turtle.Triples
                (Turtle.TriplesSubject
                    (Turtle.SubjectBlankNode Turtle.Anon)
                    [ { objects = [ Turtle.ObjectBlankNode Turtle.Anon ]
                      , verb = Turtle.Predicate (Turtle.PrefixedName "example" "knows")
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
