module Rdf exposing
    ( Triple
    , Term, Yes, No
    , Iri, BlankNode, Literal
    , BlankNodeOrIri, AnyLiteral, BlankNodeOrIriOrAnyLiteral
    , asIri, asBlankNode, asLiteral
    , asBlankNodeOrIri, asAnyLiteral, asBlankNodeOrIriOrAnyLiteral
    , IsIri, IsBlankNode
    , IsBlankNodeOrIri, IsAnyLiteral, IsBlankNodeOrIriOrAnyLiteral
    , iri, blankNode
    , literal
    , string, langString
    , int, float, decimal
    , date, dateTime
    , bool
    , toIri, toBlankNode
    , toBlankNodeOrIri, toAnyLiteral, toBlankNodeOrIriOrAnyLiteral
    , toUrl
    , toValue
    , toString, toLangString
    , toInt, toFloat, toDecimal
    , toDate, toDateTime
    , toBool
    , appendPath, dropFragment, setFragment
    , serializeNode, serializeNodeTurtle, SerializeConfig, serializeTriple
    , encodeTriple
    , tripleDecoder
    , StringOrLangString(..)
    , localize, nonLocalized
    , stringOrLangStringFrom, stringOrLangStringFromList
    , mergeStringOrLangStrings
    , stringOrLangStringInfo
    )

{-| This module defines the types and helper functions to work with the basic
buildings blocks of the RDF ecosystem. If you are new to RDF, you should take
a look at the [RDF 1.1 Primer](https://www.w3.org/TR/rdf11-primer/) for
a introduction on its data model.

@docs Triple


# RDF Terms

@docs Term, Yes, No

@docs Iri, BlankNode, Literal
@docs BlankNodeOrIri, AnyLiteral, BlankNodeOrIriOrAnyLiteral
@docs asIri, asBlankNode, asLiteral
@docs asBlankNodeOrIri, asAnyLiteral, asBlankNodeOrIriOrAnyLiteral

@docs IsIri, IsBlankNode
@docs IsBlankNodeOrIri, IsAnyLiteral, IsBlankNodeOrIriOrAnyLiteral


## Create

@docs iri, blankNode
@docs literal
@docs string, langString
@docs int, float, decimal
@docs date, dateTime
@docs bool


## Transform

@docs toIri, toBlankNode
@docs toBlankNodeOrIri, toAnyLiteral, toBlankNodeOrIriOrAnyLiteral

@docs toUrl
@docs toValue
@docs toString, toLangString
@docs toInt, toFloat, toDecimal
@docs toDate, toDateTime
@docs toBool

@docs appendPath, dropFragment, setFragment


## Serialize

@docs serializeNode, serializeNodeTurtle, SerializeConfig, serializeTriple


## Json

@docs encodeTriple
@docs tripleDecoder


## StringOrLangString

@docs StringOrLangString
@docs localize, nonLocalized
@docs stringOrLangStringFrom, stringOrLangStringFromList
@docs mergeStringOrLangStrings
@docs stringOrLangStringInfo

-}

import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Internal.Term as Internal
    exposing
        ( DataLiteral
        , Term(..)
        , Variant(..)
        )
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import Maybe.Extra as Maybe
import String.Extra as String
import Time exposing (Posix)


{-| This type represents an [RDF
Triple](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-rdf-triple).
-}
type alias Triple =
    { subject : BlankNodeOrIri
    , predicate : Iri
    , object : BlankNodeOrIriOrAnyLiteral
    }


{-| This type represents an [RDF
Term](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-rdf-triple),
which can be an
[IRI](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-iri),
a [literal](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-literal),
or a [blank
node](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-blank-node).
-}
type alias Term compatible =
    Internal.Term compatible


{-| TODO Add documentation
-}
type Yes
    = Yes Never


{-| TODO Add documentation
-}
type No
    = No Never


{-| TODO Add documentation
-}
type alias Iri =
    Term
        { isBlankNode : No
        , isIri : Yes
        , isAnyLiteral : No
        , isBlankNodeOrIri : Yes
        , isBlankNodeOrAnyLiteral : No
        , isIriOrAnyLiteral : Yes
        }


{-| TODO Add documentation
-}
type alias BlankNode =
    Term
        { isBlankNode : Yes
        , isIri : No
        , isAnyLiteral : No
        , isBlankNodeOrIri : Yes
        , isBlankNodeOrAnyLiteral : Yes
        , isIriOrAnyLiteral : No
        }


{-| TODO Add documentation
-}
type alias Literal a =
    Term
        { isLiteral : a
        , isBlankNode : No
        , isIri : No
        , isAnyLiteral : Yes
        , isBlankNodeOrIri : No
        , isBlankNodeOrAnyLiteral : Yes
        , isIriOrAnyLiteral : Yes
        }


{-| TODO Add documentation
-}
type alias BlankNodeOrIri =
    Term
        { isBlankNode : No
        , isIri : No
        , isAnyLiteral : No
        , isBlankNodeOrIri : Yes
        , isBlankNodeOrAnyLiteral : No
        , isIriOrAnyLiteral : No
        }


{-| TODO Add documentation
-}
type alias AnyLiteral =
    Term
        { isBlankNode : No
        , isIri : No
        , isAnyLiteral : Yes
        , isBlankNodeOrIri : No
        , isBlankNodeOrAnyLiteral : Yes
        , isIriOrAnyLiteral : Yes
        }


{-| TODO Add documentation
-}
type alias BlankNodeOrIriOrAnyLiteral =
    Term
        { isBlankNode : No
        , isIri : No
        , isAnyLiteral : No
        , isBlankNodeOrIri : No
        , isBlankNodeOrAnyLiteral : No
        , isIriOrAnyLiteral : No
        }


{-| TODO Add documentation
-}
type alias IsIri compatible =
    Term { compatible | isIri : Yes }


{-| TODO Add documentation
-}
type alias IsBlankNode compatible =
    Term { compatible | isBlankNode : Yes }


{-| TODO Add documentation
-}
type alias IsBlankNodeOrIri compatible =
    Term { compatible | isBlankNodeOrIri : Yes }


{-| TODO Add documentation
-}
type alias IsAnyLiteral compatible =
    Term { compatible | isAnyLiteral : Yes }


{-| TODO Add documentation
-}
type alias IsBlankNodeOrIriOrAnyLiteral compatible =
    Term compatible



-- CREATE


{-| TODO Add documentation
-}
iri : String -> Iri
iri value =
    Term (Iri value)


{-| TODO Add documentation
-}
blankNode : String -> BlankNode
blankNode value =
    Term (BlankNode value)


{-| TODO Add documentation
-}
literal : Iri -> String -> Literal a
literal datatype value =
    Term
        (Literal
            { value = value
            , datatype = toUrl datatype
            , languageTag = Nothing
            }
        )


{-| TODO Add documentation
-}
string : String -> Literal String
string value =
    Term
        (Literal
            { value = value
            , datatype = urlXsdString
            , languageTag = Nothing
            }
        )


{-| TODO Add documentation
-}
langString : String -> String -> Literal a
langString languageTag value =
    Term
        (Literal
            { value = value
            , datatype = urlRdfLangString
            , languageTag = Just languageTag
            }
        )


{-| TODO Add documentation
-}
int : Int -> Literal Int
int value =
    Term
        (Literal
            { value = String.fromInt value
            , datatype = urlXsdInteger
            , languageTag = Nothing
            }
        )


{-| TODO Add documentation
-}
float : Float -> Literal Float
float value =
    Term
        (Literal
            { value = String.fromFloat value
            , datatype = urlXsdDouble
            , languageTag = Nothing
            }
        )


{-| TODO Add documentation
-}
decimal : Decimal -> Literal Decimal
decimal value =
    Term
        (Literal
            { value = Decimal.toString value
            , datatype = urlXsdDecimal
            , languageTag = Nothing
            }
        )


{-| TODO Add documentation
-}
date : Posix -> Literal Posix
date value =
    Term
        (Literal
            { value = String.left (4 + 1 + 2 + 1 + 2) (Iso8601.fromTime value)
            , datatype = urlXsdDate
            , languageTag = Nothing
            }
        )


{-| TODO Add documentation
-}
dateTime : Posix -> Literal Posix
dateTime value =
    Term
        (Literal
            { value = Iso8601.fromTime value
            , datatype = urlXsdDateTime
            , languageTag = Nothing
            }
        )


{-| TODO Add documentation
-}
bool : Bool -> Literal Bool
bool value =
    Term
        (Literal
            { value =
                if value then
                    "true"

                else
                    "false"
            , datatype = urlXsdBoolean
            , languageTag = Nothing
            }
        )



-- TRANSFORM


{-| TODO Add documentation
-}
toIri : Term compatible -> Maybe Iri
toIri (Term node) =
    case node of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Just (Term node)

        Literal _ ->
            Nothing


{-| TODO Add documentation
-}
toBlankNode : Term compatible -> Maybe BlankNode
toBlankNode (Term variant) =
    case variant of
        BlankNode _ ->
            Just (Term variant)

        Iri _ ->
            Nothing

        Literal _ ->
            Nothing


{-| TODO Add documentation
-}
toBlankNodeOrIri : Term compatible -> Maybe BlankNodeOrIri
toBlankNodeOrIri (Term variant) =
    case variant of
        BlankNode _ ->
            Just (Term variant)

        Iri _ ->
            Just (Term variant)

        Literal _ ->
            Nothing


{-| TODO Add documentation
-}
toAnyLiteral : Term compatible -> Maybe AnyLiteral
toAnyLiteral (Term variant) =
    case variant of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Nothing

        Literal _ ->
            Just (Term variant)


{-| TODO Add documentation
-}
toBlankNodeOrIriOrAnyLiteral : Term compatible -> Maybe BlankNodeOrIriOrAnyLiteral
toBlankNodeOrIriOrAnyLiteral (Term variant) =
    case variant of
        BlankNode _ ->
            Just (Term variant)

        Iri _ ->
            Just (Term variant)

        Literal _ ->
            Nothing


{-| TODO Add documentation
-}
asIri : IsIri compatible -> Iri
asIri (Term variant) =
    Term variant


{-| TODO Add documentation
-}
asBlankNode : IsBlankNode compatible -> BlankNode
asBlankNode (Term variant) =
    Term variant


{-| TODO Add documentation
-}
asLiteral : IsAnyLiteral compatible -> Literal a
asLiteral (Term variant) =
    Term variant


{-| TODO Add documentation
-}
asBlankNodeOrIri : IsBlankNodeOrIri compatible -> BlankNodeOrIri
asBlankNodeOrIri (Term variant) =
    Term variant


{-| TODO Add documentation
-}
asBlankNodeOrIriOrAnyLiteral : IsBlankNodeOrIriOrAnyLiteral compatible -> BlankNodeOrIriOrAnyLiteral
asBlankNodeOrIriOrAnyLiteral (Term variant) =
    Term variant


{-| TODO Add documentation
-}
asAnyLiteral : IsAnyLiteral compatible -> AnyLiteral
asAnyLiteral (Term variant) =
    Term variant


{-| TODO Add documentation
-}
toUrl : Iri -> String
toUrl (Term variant) =
    case variant of
        BlankNode _ ->
            ""

        Iri url ->
            url

        Literal _ ->
            ""


{-| TODO Add documentation
-}
toValue : AnyLiteral -> String
toValue (Term variant) =
    case variant of
        BlankNode _ ->
            ""

        Iri _ ->
            ""

        Literal { value } ->
            value


{-| TODO Add documentation
-}
toString : Term compatible -> Maybe String
toString (Term variant) =
    case variant of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Nothing

        Literal data ->
            if data.datatype == urlXsdString then
                Just data.value

            else
                Nothing


{-| TODO Add documentation
-}
toLangString : Term compatible -> Maybe ( String, String )
toLangString (Term variant) =
    case variant of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Nothing

        Literal data ->
            if data.datatype == urlRdfLangString then
                Maybe.map2 Tuple.pair data.languageTag (Just data.value)

            else
                Nothing


{-| TODO Add documentation
-}
toInt : Term compatible -> Maybe Int
toInt (Term variant) =
    case variant of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Nothing

        Literal data ->
            if data.datatype == urlXsdInt then
                String.toInt data.value

            else if data.datatype == urlXsdInteger then
                String.toInt data.value

            else
                Nothing


{-| TODO Add documentation
-}
toFloat : Term compatible -> Maybe Float
toFloat (Term variant) =
    case variant of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Nothing

        Literal data ->
            if data.datatype == urlXsdDouble then
                String.toFloat data.value

            else
                Nothing


{-| TODO Add documentation
-}
toDecimal : Term compatible -> Maybe Decimal
toDecimal (Term variant) =
    case variant of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Nothing

        Literal data ->
            if data.datatype == urlXsdDecimal then
                Decimal.fromString data.value

            else
                Nothing


{-| TODO Add documentation
-}
toDate : Term compatible -> Maybe Posix
toDate (Term variant) =
    case variant of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Nothing

        Literal data ->
            if data.datatype == urlXsdDate then
                (data.value ++ "T00:00:00.000Z")
                    |> Iso8601.toTime
                    |> Result.toMaybe

            else
                Nothing


{-| TODO Add documentation
-}
toDateTime : Term compatible -> Maybe Posix
toDateTime (Term variant) =
    case variant of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Nothing

        Literal data ->
            if data.datatype == urlXsdDateTime then
                data.value
                    |> Iso8601.toTime
                    |> Result.toMaybe

            else
                Nothing


{-| TODO Add documentation
-}
toBool : Term compatible -> Maybe Bool
toBool (Term variant) =
    case variant of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Nothing

        Literal data ->
            if data.datatype == urlXsdBoolean then
                case data.value of
                    "true" ->
                        Just True

                    "false" ->
                        Just False

                    _ ->
                        Nothing

            else
                Nothing


{-| TODO Add documentation
-}
appendPath : String -> IsIri compatible -> Iri
appendPath segment (Term variant) =
    case variant of
        BlankNode _ ->
            Term variant

        Iri url ->
            case String.split "?" url of
                [ _ ] ->
                    case String.split "#" url of
                        [ _ ] ->
                            Term (Iri (url ++ segment))

                        [ beforeFragment, fragment ] ->
                            Term (Iri (beforeFragment ++ segment ++ "#" ++ fragment))

                        _ ->
                            Term (Iri (url ++ segment))

                [ beforeQuery, rest ] ->
                    case String.split "#" rest of
                        [ _ ] ->
                            Term (Iri (beforeQuery ++ segment ++ "?" ++ rest))

                        [ query, fragment ] ->
                            Term (Iri (beforeQuery ++ segment ++ "?" ++ query ++ "#" ++ fragment))

                        _ ->
                            Term (Iri (url ++ segment))

                _ ->
                    Term (Iri (url ++ segment))

        Literal _ ->
            Term variant


{-| TODO Add documentation
-}
dropFragment : IsIri compatible -> Iri
dropFragment (Term variant) =
    case variant of
        BlankNode _ ->
            Term variant

        Iri url ->
            case String.split "#" url of
                [ _ ] ->
                    Term variant

                [ beforeFragment, _ ] ->
                    Term (Iri beforeFragment)

                _ ->
                    Term variant

        Literal _ ->
            Term variant


{-| TODO Add documentation
-}
setFragment : String -> IsIri compatible -> Iri
setFragment fragment (Term variant) =
    case variant of
        BlankNode _ ->
            Term variant

        Iri url ->
            case String.split "#" url of
                [ _ ] ->
                    Term (Iri (url ++ "#" ++ fragment))

                [ beforeFragment, _ ] ->
                    Term (Iri (beforeFragment ++ "#" ++ fragment))

                _ ->
                    Term variant

        Literal _ ->
            Term variant



-- SERIALIZE


{-| TODO Add documentation
-}
serializeNode : Term compatible -> String
serializeNode (Term variant) =
    Internal.serializeVariant variant


{-| TODO Add documentation
-}
type alias SerializeConfig =
    { base : Maybe String
    , prefixes : List ( String, String )
    }


{-| TODO Add documentation
-}
serializeNodeTurtle : SerializeConfig -> Term compatible -> String
serializeNodeTurtle config (Term variant) =
    serializeNodeTurtleHelp config variant


{-| TODO Add documentation
-}
serializeNodeTurtleHelp : SerializeConfig -> Variant -> String
serializeNodeTurtleHelp config variant =
    case variant of
        BlankNode value ->
            "_:" ++ value

        Iri url ->
            if url == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" then
                "a"

            else
                case config.base of
                    Nothing ->
                        case List.find (\( _, value ) -> String.startsWith value url) config.prefixes of
                            Nothing ->
                                "<" ++ url ++ ">"

                            Just ( prefix, value ) ->
                                prefix ++ ":" ++ String.rightOf value url

                    Just base ->
                        if String.startsWith base url then
                            "<" ++ String.rightOf base url ++ ">"

                        else
                            case List.find (\( _, value ) -> String.startsWith value url) config.prefixes of
                                Nothing ->
                                    "<" ++ url ++ ">"

                                Just ( prefix, value ) ->
                                    prefix ++ ":" ++ String.rightOf value url

        Literal data ->
            let
                replaceLineBreaks : String -> String
                replaceLineBreaks =
                    String.replace "\n" "\\n"

                replaceQuotes : String -> String
                replaceQuotes =
                    String.replace "\"" "\\\""
            in
            if data.datatype == urlXsdString then
                [ "\""
                , data.value
                    |> replaceLineBreaks
                    |> replaceQuotes
                , "\""
                , case data.languageTag of
                    Nothing ->
                        ""

                    Just languageTag ->
                        "@" ++ languageTag
                ]
                    |> String.concat

            else if
                (data.datatype == urlXsdInteger)
                    || (data.datatype == urlXsdInt)
            then
                data.value

            else
                [ "\""
                , data.value
                    |> replaceLineBreaks
                    |> replaceQuotes
                , "\""
                , case data.languageTag of
                    Nothing ->
                        "^^" ++ serializeNodeTurtle config (iri data.datatype)

                    Just languageTag ->
                        "@" ++ languageTag
                ]
                    |> String.concat


{-| TODO Add documentation
-}
serializeTriple : Triple -> String
serializeTriple { subject, predicate, object } =
    [ serializeNode subject
    , serializeNode predicate
    , serializeNode object
    , "."
    ]
        |> String.join " "



-- STRING OR LANG STRING


{-| TODO Add documentation
-}
type StringOrLangString
    = StringOrLangString
        { string : Maybe String
        , langStrings : Dict String String
        }


{-| TODO Add documention
-}
stringOrLangStringInfo : StringOrLangString -> { string : Maybe String, langStrings : Dict String String }
stringOrLangStringInfo (StringOrLangString stringOrLangString) =
    stringOrLangString


{-| TODO Add documentation
-}
localize : String -> StringOrLangString -> Maybe String
localize locale (StringOrLangString stringOrLangString) =
    [ Dict.get locale stringOrLangString.langStrings
    , Dict.get "en" stringOrLangString.langStrings
    , stringOrLangString.string
    ]
        |> Maybe.orList


{-| TODO Add documentation
-}
nonLocalized : StringOrLangString -> Maybe String
nonLocalized (StringOrLangString stringOrLangString) =
    stringOrLangString.string


{-| TODO Add documentation
-}
stringOrLangStringFrom : Maybe String -> List ( String, String ) -> StringOrLangString
stringOrLangStringFrom maybeString langStrings =
    StringOrLangString
        { string = maybeString
        , langStrings = Dict.fromList langStrings
        }


{-| TODO Add documentation
-}
stringOrLangStringFromList : List ( String, String ) -> StringOrLangString
stringOrLangStringFromList langStrings =
    StringOrLangString
        { string = Nothing
        , langStrings = Dict.fromList langStrings
        }


{-| TODO Add documentation
-}
mergeStringOrLangStrings : List StringOrLangString -> Maybe StringOrLangString
mergeStringOrLangStrings stringOrLangStrings =
    if List.isEmpty stringOrLangStrings then
        Nothing

    else
        { string =
            stringOrLangStrings
                |> List.filterMap (\(StringOrLangString stringOrLangString) -> stringOrLangString.string)
                |> List.head
        , langStrings =
            stringOrLangStrings
                |> List.map (\(StringOrLangString stringOrLangString) -> stringOrLangString.langStrings)
                |> List.foldr Dict.union Dict.empty
        }
            |> StringOrLangString
            |> Just



-- JSON


{-| TODO Add documentation
-}
tripleDecoder : Decoder Triple
tripleDecoder =
    Decode.succeed Triple
        |> Decode.required "subject" subjectDecoder
        |> Decode.required "predicate" predicateDecoder
        |> Decode.required "object" objectDecoder


subjectDecoder : Decoder BlankNodeOrIri
subjectDecoder =
    [ blankNodeDecoder
    , iriDecoder
    ]
        |> Decode.oneOf
        |> Decode.map Term


predicateDecoder : Decoder Iri
predicateDecoder =
    iriDecoder
        |> Decode.map Term


objectDecoder : Decoder BlankNodeOrIriOrAnyLiteral
objectDecoder =
    [ blankNodeDecoder
    , literalDecoder
    , iriDecoder
    ]
        |> Decode.oneOf
        |> Decode.map Term


blankNodeDecoder : Decoder Variant
blankNodeDecoder =
    Decode.string
        |> Decode.field "termType"
        |> Decode.andThen
            (\termType ->
                if termType == "BlankNode" then
                    Decode.string
                        |> Decode.field "value"
                        |> Decode.map BlankNode

                else
                    Decode.fail "not a blank node"
            )


iriDecoder : Decoder Variant
iriDecoder =
    Decode.map Iri urlDecoder


urlDecoder : Decoder String
urlDecoder =
    Decode.string
        |> Decode.field "termType"
        |> Decode.andThen
            (\termType ->
                if termType == "NamedNode" then
                    Decode.string
                        |> Decode.field "value"

                else
                    Decode.fail "not a named node"
            )


literalDecoder : Decoder Variant
literalDecoder =
    Decode.string
        |> Decode.field "termType"
        |> Decode.andThen
            (\termType ->
                if termType == "Literal" then
                    Decode.succeed DataLiteral
                        |> Decode.required "value" Decode.string
                        |> Decode.required "datatype" urlDecoder
                        |> Decode.required "language"
                            (Decode.oneOf
                                [ Decode.null Nothing
                                , Decode.string
                                    |> Decode.map
                                        (\languageTag ->
                                            if languageTag == "" then
                                                Nothing

                                            else
                                                Just languageTag
                                        )
                                ]
                            )
                        |> Decode.map Literal

                else
                    Decode.fail "not a literal node"
            )


{-| TODO Add documentation
-}
encodeTriple : Triple -> Value
encodeTriple triple =
    [ ( "subject", encodeSubject triple.subject )
    , ( "predicate", encodePredicate triple.predicate )
    , ( "object", encodeObject triple.object )
    ]
        |> Encode.object


encodeSubject : BlankNodeOrIri -> Value
encodeSubject (Term variant) =
    case variant of
        BlankNode name ->
            encodeBlankNode name

        Iri url ->
            encodeIri url

        Literal _ ->
            Encode.null


encodePredicate : Iri -> Value
encodePredicate (Term variant) =
    case variant of
        BlankNode _ ->
            Encode.null

        Iri url ->
            encodeIri url

        Literal _ ->
            Encode.null


encodeObject : BlankNodeOrIriOrAnyLiteral -> Value
encodeObject (Term variant) =
    case variant of
        BlankNode name ->
            encodeBlankNode name

        Iri url ->
            encodeIri url

        Literal data ->
            encodeLiteral data


encodeBlankNode : String -> Value
encodeBlankNode name =
    [ ( "termType", Encode.string "BlankNode" )
    , ( "value", Encode.string name )
    ]
        |> Encode.object


encodeIri : String -> Value
encodeIri url =
    [ ( "termType", Encode.string "NamedNode" )
    , ( "value", Encode.string url )
    ]
        |> Encode.object


encodeLiteral : DataLiteral -> Value
encodeLiteral data =
    [ ( "termType", Encode.string "Literal" )
    , ( "value", Encode.string data.value )
    , ( "datatype", encodeIri data.datatype )
    , ( "language"
      , case data.languageTag of
            Nothing ->
                Encode.null

            Just languageTag ->
                Encode.string languageTag
      )
    ]
        |> Encode.object



-- CONSTANTS
-- URLS


urlXsdString : String
urlXsdString =
    "http://www.w3.org/2001/XMLSchema#string"


urlXsdInt : String
urlXsdInt =
    "http://www.w3.org/2001/XMLSchema#int"


urlXsdInteger : String
urlXsdInteger =
    "http://www.w3.org/2001/XMLSchema#integer"


urlXsdDouble : String
urlXsdDouble =
    "http://www.w3.org/2001/XMLSchema#double"


urlXsdDecimal : String
urlXsdDecimal =
    "http://www.w3.org/2001/XMLSchema#decimal"


urlXsdDate : String
urlXsdDate =
    "http://www.w3.org/2001/XMLSchema#date"


urlXsdDateTime : String
urlXsdDateTime =
    "http://www.w3.org/2001/XMLSchema#dateTime"


urlXsdBoolean : String
urlXsdBoolean =
    "http://www.w3.org/2001/XMLSchema#boolean"


urlRdfLangString : String
urlRdfLangString =
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString"
