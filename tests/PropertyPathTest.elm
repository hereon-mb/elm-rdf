module PropertyPathTest exposing (normalize1, normalize2, normalize3, normalize4, normalize5)

import Expect
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import RDF
import RDF.PropertyPath as RDF
import Test exposing (Test, test)


normalize1 : Test
normalize1 =
    normalizeHelp "2-element smoke test"
        (RDF.SequencePath
            (RDF.PredicatePath (RDF.iri "a"))
            [ RDF.PredicatePath (RDF.iri "b")
            ]
        )
        (NonEmpty.fromCons (RDF.iri "a") [ RDF.iri "b" ])


normalize2 : Test
normalize2 =
    normalizeHelp "eliminates superluous nesting"
        (RDF.SequencePath
            (RDF.SequencePath (RDF.PredicatePath (RDF.iri "a")) [])
            []
        )
        (NonEmpty.fromCons (RDF.iri "a") [])


normalize3 : Test
normalize3 =
    normalizeHelp "3-element smoke test"
        (RDF.SequencePath
            (RDF.PredicatePath (RDF.iri "a"))
            [ RDF.PredicatePath (RDF.iri "b")
            , RDF.PredicatePath (RDF.iri "c")
            ]
        )
        (NonEmpty.fromCons
            (RDF.iri "a")
            [ RDF.iri "b"
            , RDF.iri "c"
            ]
        )


normalize4 : Test
normalize4 =
    normalizeHelp "eliminates recursive nesting"
        (RDF.SequencePath
            (RDF.PredicatePath (RDF.iri "a"))
            [ RDF.SequencePath (RDF.PredicatePath (RDF.iri "b"))
                [ RDF.SequencePath (RDF.PredicatePath (RDF.iri "c")) []
                ]
            ]
        )
        (NonEmpty.fromCons
            (RDF.iri "a")
            [ RDF.iri "b"
            , RDF.iri "c"
            ]
        )


normalize5 : Test
normalize5 =
    normalizeHelp "eliminates superfluous recursive nesting"
        (RDF.SequencePath
            (RDF.SequencePath (RDF.PredicatePath (RDF.iri "a")) [])
            [ RDF.SequencePath (RDF.SequencePath (RDF.PredicatePath (RDF.iri "b")) [])
                [ RDF.SequencePath (RDF.SequencePath (RDF.PredicatePath (RDF.iri "c")) []) []
                ]
            ]
        )
        (NonEmpty.fromCons
            (RDF.iri "a")
            [ RDF.iri "b"
            , RDF.iri "c"
            ]
        )


normalizeHelp : String -> RDF.PropertyPath -> NonEmpty RDF.Iri -> Test
normalizeHelp label propertyPath expected =
    test label <|
        \_ ->
            RDF.normalizePropertyPath propertyPath
                |> Expect.equal (Just expected)
