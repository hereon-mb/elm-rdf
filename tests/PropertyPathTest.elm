module PropertyPathTest exposing (normalize1, normalize2, normalize3, normalize4, normalize5)

import Expect
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Rdf exposing (IriOrPath)
import Rdf.Predicate as Predicate exposing (Predicate(..))
import Test exposing (Test, test)


normalize1 : Test
normalize1 =
    normalizeHelp "2-element smoke test"
        (Rdf.sequence
            (Rdf.iri "a")
            [ Rdf.iri "b"
            ]
        )
        (NonEmpty.fromCons (Predicate (Rdf.iri "a")) [ Predicate (Rdf.iri "b") ])


normalize2 : Test
normalize2 =
    normalizeHelp "eliminates superluous nesting"
        (Rdf.sequence
            (Rdf.sequence (Rdf.iri "a") [])
            []
        )
        (NonEmpty.fromCons (Predicate (Rdf.iri "a")) [])


normalize3 : Test
normalize3 =
    normalizeHelp "3-element smoke test"
        (Rdf.sequence
            (Rdf.iri "a")
            [ Rdf.iri "b"
            , Rdf.iri "c"
            ]
        )
        (NonEmpty.fromCons
            (Predicate (Rdf.iri "a"))
            [ Predicate (Rdf.iri "b")
            , Predicate (Rdf.iri "c")
            ]
        )


normalize4 : Test
normalize4 =
    normalizeHelp "eliminates recursive nesting"
        (Rdf.sequence
            (Rdf.iri "a")
            [ Rdf.sequence (Rdf.iri "b")
                [ Rdf.sequence (Rdf.iri "c") []
                ]
            ]
        )
        (NonEmpty.fromCons
            (Predicate (Rdf.iri "a"))
            [ Predicate (Rdf.iri "b")
            , Predicate (Rdf.iri "c")
            ]
        )


normalize5 : Test
normalize5 =
    normalizeHelp "eliminates superfluous recursive nesting"
        (Rdf.sequence
            (Rdf.sequence (Rdf.iri "a") [])
            [ Rdf.sequence (Rdf.sequence (Rdf.iri "b") [])
                [ Rdf.sequence (Rdf.sequence (Rdf.iri "c") []) []
                ]
            ]
        )
        (NonEmpty.fromCons
            (Predicate (Rdf.iri "a"))
            [ Predicate (Rdf.iri "b")
            , Predicate (Rdf.iri "c")
            ]
        )


normalizeHelp : String -> IriOrPath -> NonEmpty Predicate -> Test
normalizeHelp label propertyPath expected =
    test label <|
        \_ ->
            Predicate.fromPropertyPath propertyPath
                |> Expect.equal (Just expected)
