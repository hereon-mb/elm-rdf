{- Copyright 2024-2026 Helmholtz-Zentrum hereon GmbH

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}


module Internal.Term exposing
    ( Term(..)
    , Variant(..), DataLiteral
    , serializeVariant
    , toVariant
    )

{-|

@docs Term
@docs Variant, DataLiteral

@docs serializeVariant
@docs toVariant

-}


type Term compatible
    = Term Variant


type Variant
    = BlankNode String
    | Iri String
    | Literal DataLiteral
    | VarQ String
    | VarD String
    | Sequence Variant (List Variant)
    | Alternative Variant (List Variant)
    | Inverse Variant
    | ZeroOrMore Variant
    | OneOrMore Variant
    | ZeroOrOne Variant


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

        VarQ name ->
            "?" ++ name

        VarD name ->
            "$" ++ name

        Sequence first rest ->
            serializeVariant first
                ++ " / "
                ++ String.join " / " (List.map serializeVariant rest)

        Alternative first rest ->
            serializeVariant first
                ++ " | "
                ++ String.join " | " (List.map serializeVariant rest)

        Inverse nested ->
            "^" ++ serializeVariant nested

        ZeroOrMore nested ->
            serializeVariant nested ++ "*"

        OneOrMore nested ->
            serializeVariant nested ++ "+"

        ZeroOrOne nested ->
            serializeVariant nested ++ "?"


toVariant : Term compatible -> Variant
toVariant (Term variant) =
    variant
