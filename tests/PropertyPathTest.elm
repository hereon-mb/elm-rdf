module PropertyPathTest exposing (normalize1, normalize2, normalize3, normalize4, normalize5)

import Expect
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Rdf
import Rdf.PropertyPath as Rdf
import Test exposing (Test, test)


normalize1 : Test
normalize1 =
    normalizeHelp "2-element smoke test"
        (Rdf.SequencePath
            (Rdf.PredicatePath (Rdf.iri "a"))
            [ Rdf.PredicatePath (Rdf.iri "b")
            ]
        )
        (NonEmpty.fromCons (Rdf.iri "a") [ Rdf.iri "b" ])


normalize2 : Test
normalize2 =
    normalizeHelp "eliminates superluous nesting"
        (Rdf.SequencePath
            (Rdf.SequencePath (Rdf.PredicatePath (Rdf.iri "a")) [])
            []
        )
        (NonEmpty.fromCons (Rdf.iri "a") [])


normalize3 : Test
normalize3 =
    normalizeHelp "3-element smoke test"
        (Rdf.SequencePath
            (Rdf.PredicatePath (Rdf.iri "a"))
            [ Rdf.PredicatePath (Rdf.iri "b")
            , Rdf.PredicatePath (Rdf.iri "c")
            ]
        )
        (NonEmpty.fromCons
            (Rdf.iri "a")
            [ Rdf.iri "b"
            , Rdf.iri "c"
            ]
        )


normalize4 : Test
normalize4 =
    normalizeHelp "eliminates recursive nesting"
        (Rdf.SequencePath
            (Rdf.PredicatePath (Rdf.iri "a"))
            [ Rdf.SequencePath (Rdf.PredicatePath (Rdf.iri "b"))
                [ Rdf.SequencePath (Rdf.PredicatePath (Rdf.iri "c")) []
                ]
            ]
        )
        (NonEmpty.fromCons
            (Rdf.iri "a")
            [ Rdf.iri "b"
            , Rdf.iri "c"
            ]
        )


normalize5 : Test
normalize5 =
    normalizeHelp "eliminates superfluous recursive nesting"
        (Rdf.SequencePath
            (Rdf.SequencePath (Rdf.PredicatePath (Rdf.iri "a")) [])
            [ Rdf.SequencePath (Rdf.SequencePath (Rdf.PredicatePath (Rdf.iri "b")) [])
                [ Rdf.SequencePath (Rdf.SequencePath (Rdf.PredicatePath (Rdf.iri "c")) []) []
                ]
            ]
        )
        (NonEmpty.fromCons
            (Rdf.iri "a")
            [ Rdf.iri "b"
            , Rdf.iri "c"
            ]
        )


normalizeHelp : String -> Rdf.PropertyPath -> NonEmpty Rdf.Iri -> Test
normalizeHelp label propertyPath expected =
    test label <|
        \_ ->
            Rdf.normalizePropertyPath propertyPath
                |> Expect.equal (Just expected)
