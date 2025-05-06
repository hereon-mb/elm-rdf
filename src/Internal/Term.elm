module Internal.Term exposing
    ( Term(..)
    , Variant(..), DataLiteral
    , serializeVariant
    )

{-|

@docs Term
@docs Variant, DataLiteral

@docs serializeVariant

-}


type Term compatible
    = Term Variant


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


serialize : Term compatible -> String
serialize (Term variant) =
    serializeVariant variant


serializeVariant : Variant -> String
serializeVariant variant =
    case variant of
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
                    "^^" ++ serialize (Term (Iri data.datatype))

                Just languageTag ->
                    "@" ++ languageTag
            ]
                |> String.concat
