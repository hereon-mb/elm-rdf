module Rdf.PropertyPath exposing
    ( PropertyPath(..)
    , serializePropertyPath
    , normalizePropertyPath
    )

{-|

@docs PropertyPath
@docs serializePropertyPath
@docs normalizePropertyPath

-}

import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Maybe.Extra as Maybe
import Rdf exposing (Iri, serializeNode)


{-| TODO Add documentation
-}
type PropertyPath
    = PredicatePath Iri
    | SequencePath PropertyPath (List PropertyPath)
    | AlternativePath PropertyPath (List PropertyPath)
    | InversePath PropertyPath
    | ZeroOrMorePath PropertyPath
    | OneOrMorePath PropertyPath
    | ZeroOrOnePath PropertyPath


{-| TODO Add documentation
-}
serializePropertyPath : PropertyPath -> String
serializePropertyPath propertyPath =
    case propertyPath of
        PredicatePath iri ->
            serializeNode iri

        SequencePath first rest ->
            (serializePropertyPath first :: List.map serializePropertyPath rest)
                |> String.join " / "

        AlternativePath first rest ->
            (serializePropertyPath first :: List.map serializePropertyPath rest)
                |> String.join " | "

        InversePath nested ->
            "^" ++ serializePropertyPath nested

        ZeroOrMorePath nested ->
            serializePropertyPath nested ++ "*"

        OneOrMorePath nested ->
            serializePropertyPath nested ++ "+"

        ZeroOrOnePath nested ->
            serializePropertyPath nested ++ "?"


{-| XXX Normalizes a `PropertyPath` into a `NonEmpty Iri`. Ideally, we distinguish between `PropertyPath` and `NonEmpty Iri` in our code-base. But since we use the super-class `PropertyPath` everywhere, we have to special case the impossible variants, cf. `property` which is a no-op if it encounters a such special property path.
-}
normalizePropertyPath : PropertyPath -> Maybe (NonEmpty Iri)
normalizePropertyPath propertyPath =
    case propertyPath of
        PredicatePath x ->
            Just (NonEmpty.singleton x)

        SequencePath firstPropertyPath otherPropertyPaths ->
            Maybe.map NonEmpty.concat
                (Maybe.map2 Tuple.pair
                    (normalizePropertyPath firstPropertyPath)
                    (Maybe.combine (List.map normalizePropertyPath otherPropertyPaths))
                )

        AlternativePath _ _ ->
            Nothing

        InversePath _ ->
            Nothing

        ZeroOrMorePath _ ->
            Nothing

        OneOrMorePath _ ->
            Nothing

        ZeroOrOnePath _ ->
            Nothing
