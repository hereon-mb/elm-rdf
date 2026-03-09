module Rdf.Predicate exposing
    ( Predicate(..)
    , fromPropertyPath, toIri
    )

{-| There are situations where one wants to only work with property paths of
fixed length, which means they only consist of predicate, inverse, or
sequence paths. This module provides a few helpers.

@docs Predicate
@docs fromPropertyPath, toIri

-}

import Internal.Term as Internal exposing (Term(..))
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Maybe.Extra as Maybe
import Rdf exposing (Iri, IsPath)


{-| A segment of a fixed length property path.
-}
type Predicate
    = Predicate Iri
    | Inverse Iri


{-| Extract the `Iri` from a [`Predicate`](#Predicate)
-}
toIri : Predicate -> Iri
toIri constructablePath =
    case constructablePath of
        Predicate iri ->
            iri

        Inverse iri ->
            iri


{-| Normalize a `Path` into a `NonEmpty Predicate`. This will return `Nothing`
for paths which are not of fixed length.

    import Rdf exposing
        ( iri, inverse, sequence, alternative
        , zeroOrMore, zeroOrOne, oneOrMore
        )

    fromPropertyPath (iri "http://example.org")
    --> Just ( Predicate (iri "http://example.org"), [] )

    fromPropertyPath (inverse (iri "http://example.org"))
    --> Just ( Inverse (iri "http://example.org"), [] )

    fromPropertyPath
        (sequence
            (iri "http://example.org#a")
            [ iri "http://example.org#b" ]
        )
    --> Just
    -->     ( Predicate (iri "http://example.org#a")
    -->     , [ Predicate (iri "http://example.org#b") ]
    -->     )

    fromPropertyPath
        (alternative
            (iri "http://example.org#a")
            [ iri "http://example.org#b" ]
        )
    --> Nothing

    fromPropertyPath (zeroOrMore (iri "http://example.org"))
    --> Nothing

    fromPropertyPath (zeroOrOne (iri "http://example.org"))
    --> Nothing

    fromPropertyPath (oneOrMore (iri "http://example.org"))
    --> Nothing

-}
fromPropertyPath : IsPath compatible -> Maybe (NonEmpty Predicate)
fromPropertyPath (Term variant) =
    case variant of
        (Internal.Iri _) as x ->
            Just (NonEmpty.singleton (Predicate (Term x)))

        Internal.Sequence firstPropertyPath otherPropertyPaths ->
            Maybe.map NonEmpty.concat
                (Maybe.map2 Tuple.pair
                    (fromPropertyPath (Term firstPropertyPath))
                    (Maybe.combine (List.map (fromPropertyPath << Term) otherPropertyPaths))
                )

        Internal.Alternative _ _ ->
            Nothing

        Internal.Inverse ((Internal.Iri _) as x) ->
            Just (NonEmpty.singleton (Inverse (Term x)))

        Internal.Inverse _ ->
            Nothing

        Internal.ZeroOrMore _ ->
            Nothing

        Internal.OneOrMore _ ->
            Nothing

        Internal.ZeroOrOne _ ->
            Nothing

        _ ->
            Nothing
