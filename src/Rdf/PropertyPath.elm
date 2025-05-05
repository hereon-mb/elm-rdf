module Rdf.PropertyPath exposing
    ( PropertyPath(..)
    , serializePropertyPath
    )

{-|

@docs PropertyPath
@docs serializePropertyPath

-}

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
