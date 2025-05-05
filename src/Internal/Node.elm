module Internal.Node exposing
    ( Node(..)
    , Variant(..), DataLiteral
    , serializeVariant
    )

{-|

@docs Node
@docs Variant, DataLiteral

@docs serializeVariant

-}


type Node compatible
    = Node Variant


type Variant
    = BlankNode String
    | Iri String
    | Literal DataLiteral


{-| TODO Add documentation
-}
type alias DataLiteral =
    { value : String
    , datatype : String
    , languageTag : Maybe String
    }


serialize : Node compatible -> String
serialize (Node node) =
    serializeVariant node


serializeVariant : Variant -> String
serializeVariant node =
    case node of
        BlankNode value ->
            "_:" ++ value

        Iri value ->
            "<" ++ value ++ ">"

        Literal data ->
            let
                replaceLineBreaks : String -> String
                replaceLineBreaks =
                    String.replace "\n" "\\n"

                replaceQuotes : String -> String
                replaceQuotes =
                    String.replace "\"" "\\\""
            in
            [ "\""
            , data.value
                |> replaceLineBreaks
                |> replaceQuotes
            , "\""
            , case data.languageTag of
                Nothing ->
                    "^^" ++ serialize (Node (Iri data.datatype))

                Just languageTag ->
                    "@" ++ languageTag
            ]
                |> String.concat
