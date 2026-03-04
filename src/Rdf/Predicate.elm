module Rdf.Predicate exposing
    ( Predicate(..)
    , fromPropertyPath, toIri
    )

{-|

@docs Predicate
@docs fromPropertyPath, toIri

-}

import Internal.Term as Internal exposing (Term(..))
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Maybe.Extra as Maybe
import Rdf exposing (Iri, IsIriOrPath)


{-| TODO Add documentation
-}
type Predicate
    = Predicate Iri
    | Inverse Iri


{-| TODO Add documentation
-}
toIri : Predicate -> Iri
toIri constructablePath =
    case constructablePath of
        Predicate iri ->
            iri

        Inverse iri ->
            iri


{-| XXX Normalizes a `PropertyPath` into a `NonEmpty Predicate`. Ideally, we
distinguish between `PropertyPath` and `NonEmpty Predicate` in our code-base.
But since we use the super-class `PropertyPath` everywhere, we have to special
case the impossible variants, cf. `property` which is a no-op if it encounters
a such special property path.
-}
fromPropertyPath : IsIriOrPath compatible -> Maybe (NonEmpty Predicate)
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
