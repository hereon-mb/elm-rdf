module Internal.TurtleTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Internal.Turtle as Turtle
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Internal.Turtle"
        [ describe "parse"
            [ emptyDocument
            , directives
            , triples
            ]
        ]


emptyDocument : Test
emptyDocument =
    test "empty document" <|
        \_ ->
            ""
                |> Turtle.parse
                |> Expect.equal (Ok [])


directives : Test
directives =
    describe "directives"
        [ prefixId
        , base
        , sparqlPrefix
        , sparqlBase
        ]


prefixId : Test
prefixId =
    test "prefixId" <|
        \_ ->
            "@prefix example: <http://example.org/> ."
                |> Turtle.parse
                |> Expect.equal
                    (Ok
                        [ Turtle.Directive
                            (Turtle.PrefixId "example" "http://example.org/")
                        ]
                    )


base : Test
base =
    test "base" <|
        \_ ->
            "@base <http://example.org/> ."
                |> Turtle.parse
                |> Expect.equal
                    (Ok
                        [ Turtle.Directive
                            (Turtle.Base "http://example.org/")
                        ]
                    )


sparqlPrefix : Test
sparqlPrefix =
    test "sparqlPrefix" <|
        \_ ->
            "PREFIX example: <http://example.org/>"
                |> Turtle.parse
                |> Expect.equal
                    (Ok
                        [ Turtle.Directive
                            (Turtle.SparqlPrefix "example" "http://example.org/")
                        ]
                    )


sparqlBase : Test
sparqlBase =
    test "sparqlBase" <|
        \_ ->
            "BASE <http://example.org/>"
                |> Turtle.parse
                |> Expect.equal
                    (Ok
                        [ Turtle.Directive
                            (Turtle.SparqlBase "http://example.org/")
                        ]
                    )


triples : Test
triples =
    describe "triples"
        [ triplesSubject
        ]


triplesSubject : Test
triplesSubject =
    describe "triplesSubject"
        [ tripleIriAIri
        , tripleIriIriIri
        , tripleBlankNodeABlankNode
        , tripleCollectionACollection
        , tripleIriIriLiteralString
        , tripleIriIriLiteralLangString
        , tripleIriIriLiteralTyped
        ]


tripleIriAIri : Test
tripleIriAIri =
    test "triple: Iri a Iri" <|
        \_ ->
            "<http://example.org/alice> a <http://example.org/#Person> ."
                |> Turtle.parse
                |> Expect.equal
                    (Ok
                        [ Turtle.Triples
                            (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.IriRef "http://example.org/alice"))
                                [ { verb = Turtle.A
                                  , objectList = [ Turtle.ObjectIri (Turtle.IriRef "http://example.org/#Person") ]
                                  }
                                ]
                            )
                        ]
                    )


tripleIriIriIri : Test
tripleIriIriIri =
    test "triple: Iri Iri Iri" <|
        \_ ->
            "<http://example.org/alice> <http://example.org/#name> <http://example.org/#Person> ."
                |> Turtle.parse
                |> Expect.equal
                    (Ok
                        [ Turtle.Triples
                            (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.IriRef "http://example.org/alice"))
                                [ { verb = Turtle.Predicate (Turtle.IriRef "http://example.org/#name")
                                  , objectList = [ Turtle.ObjectIri (Turtle.IriRef "http://example.org/#Person") ]
                                  }
                                ]
                            )
                        ]
                    )


tripleBlankNodeABlankNode : Test
tripleBlankNodeABlankNode =
    test "triple: BlankNode A BlankNode" <|
        \_ ->
            "_:alice <http://example.org/#name> _:Person ."
                |> Turtle.parse
                |> Expect.equal
                    (Ok
                        [ Turtle.Triples
                            (Turtle.TriplesSubject (Turtle.SubjectBlankNode (Turtle.BlankNodeLabel "alice"))
                                [ { verb = Turtle.Predicate (Turtle.IriRef "http://example.org/#name")
                                  , objectList = [ Turtle.ObjectBlankNode (Turtle.BlankNodeLabel "Person") ]
                                  }
                                ]
                            )
                        ]
                    )


tripleCollectionACollection : Test
tripleCollectionACollection =
    test "triple: Collection A Collection" <|
        \_ ->
            "(_:a _:b) a (_:c _:d) ."
                |> Turtle.parse
                |> Expect.equal
                    (Ok
                        [ Turtle.Triples
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


tripleIriIriLiteralString : Test
tripleIriIriLiteralString =
    test "triple: Iri Iri LiteralString" <|
        \_ ->
            "<http://example.org/alice> <http://example.org/#name> \"Alice\" ."
                |> Turtle.parse
                |> Expect.equal
                    (Ok
                        [ Turtle.Triples
                            (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.IriRef "http://example.org/alice"))
                                [ { verb = Turtle.Predicate (Turtle.IriRef "http://example.org/#name")
                                  , objectList = [ Turtle.ObjectLiteral (Turtle.RdfLiteralString "Alice") ]
                                  }
                                ]
                            )
                        ]
                    )


tripleIriIriLiteralLangString : Test
tripleIriIriLiteralLangString =
    test "triple: Iri Iri LiteralLangString" <|
        \_ ->
            "<http://example.org/alice> <http://example.org/#name> \"Alice\"@en-US ."
                |> Turtle.parse
                |> Expect.equal
                    (Ok
                        [ Turtle.Triples
                            (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.IriRef "http://example.org/alice"))
                                [ { verb = Turtle.Predicate (Turtle.IriRef "http://example.org/#name")
                                  , objectList = [ Turtle.ObjectLiteral (Turtle.RdfLiteralLangString "Alice" "en-US") ]
                                  }
                                ]
                            )
                        ]
                    )


tripleIriIriLiteralTyped : Test
tripleIriIriLiteralTyped =
    test "triple: Iri Iri LiteralTyped" <|
        \_ ->
            "<http://example.org/alice> <http://example.org/#name> \"Alice\"^^<http://example.org/#datatype> ."
                |> Turtle.parse
                |> Expect.equal
                    (Ok
                        [ Turtle.Triples
                            (Turtle.TriplesSubject (Turtle.SubjectIri (Turtle.IriRef "http://example.org/alice"))
                                [ { verb = Turtle.Predicate (Turtle.IriRef "http://example.org/#name")
                                  , objectList = [ Turtle.ObjectLiteral (Turtle.RdfLiteralTyped "Alice" (Turtle.IriRef "http://example.org/#datatype")) ]
                                  }
                                ]
                            )
                        ]
                    )
