module Rdf.Predicate exposing
    ( Predicate(..)
    , fromPropertyPath
    , toIri
    )

import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Maybe.Extra as Maybe
import Rdf exposing (Iri, serializeNode)
import Rdf.PropertyPath as PropertyPath exposing (PropertyPath)


type Predicate
    = Predicate Iri
    | Inverse Iri


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
fromPropertyPath : PropertyPath -> Maybe (NonEmpty Predicate)
fromPropertyPath propertyPath =
    case propertyPath of
        PropertyPath.PredicatePath x ->
            Just (NonEmpty.singleton (Predicate x))

        PropertyPath.SequencePath firstPropertyPath otherPropertyPaths ->
            Maybe.map NonEmpty.concat
                (Maybe.map2 Tuple.pair
                    (fromPropertyPath firstPropertyPath)
                    (Maybe.combine (List.map fromPropertyPath otherPropertyPaths))
                )

        PropertyPath.AlternativePath _ _ ->
            Nothing

        PropertyPath.InversePath (PropertyPath.PredicatePath x) ->
            Just (NonEmpty.singleton (Inverse x))

        PropertyPath.InversePath _ ->
            Nothing

        PropertyPath.ZeroOrMorePath _ ->
            Nothing

        PropertyPath.OneOrMorePath _ ->
            Nothing

        PropertyPath.ZeroOrOnePath _ ->
            Nothing
