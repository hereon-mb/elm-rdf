module Rdf exposing
    ( Node(..), Yes, No
    , Iri, BlankNode, Literal, LiteralData
    , BlankNodeOrIri, AnyLiteral, BlankNodeOrIriOrAnyLiteral
    , asIri, asBlankNode, asLiteral
    , asBlankNodeOrIri, asAnyLiteral, asBlankNodeOrIriOrAnyLiteral
    , IsIri, IsBlankNode
    , IsBlankNodeOrIri, IsAnyLiteral, IsBlankNodeOrIriOrAnyLiteral
    , NTriple
    , NodeInternal(..), unwrap
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
    , serializeNode, serializeNTriple, serializeNodeHelp
    , encodeNTriple
    , nTripleDecoder
    , StringOrLangString(..)
    , localize, nonLocalized
    , stringOrLangStringFrom, stringOrLangStringFromList
    , mergeStringOrLangStrings
    , stringOrLangStringInfo
    )

{-|


# Node

@docs Node, Yes, No

@docs Iri, BlankNode, Literal, LiteralData
@docs BlankNodeOrIri, AnyLiteral, BlankNodeOrIriOrAnyLiteral
@docs asIri, asBlankNode, asLiteral
@docs asBlankNodeOrIri, asAnyLiteral, asBlankNodeOrIriOrAnyLiteral

@docs IsIri, IsBlankNode
@docs IsBlankNodeOrIri, IsAnyLiteral, IsBlankNodeOrIriOrAnyLiteral

@docs NTriple

@docs NodeInternal, unwrap


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

@docs serializeNode, serializeNTriple, serializeNodeHelp


## Json

@docs encodeNTriple
@docs nTripleDecoder


## StringOrLangString

@docs StringOrLangString
@docs localize, nonLocalized
@docs stringOrLangStringFrom, stringOrLangStringFromList
@docs mergeStringOrLangStrings
@docs stringOrLangStringInfo

-}

import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Maybe.Extra as Maybe
import Time exposing (Posix)


{-| TODO Add documentation
-}
type Node compatible
    = Node NodeInternal


{-| -}
type Yes
    = Yes Never


{-| -}
type No
    = No Never


{-| FIXME internals exposed for benchmarks
-}
type NodeInternal
    = BlankNode String
    | Iri String
    | Literal LiteralData


{-| FIXME Remove this
-}
unwrap : Node compatible -> NodeInternal
unwrap (Node node) =
    node


{-| TODO Add documentation
-}
type alias LiteralData =
    { value : String
    , datatype : Iri
    , languageTag : Maybe String
    }


{-| -}
type alias Iri =
    Node
        { isBlankNode : No
        , isIri : Yes
        , isAnyLiteral : No
        , isBlankNodeOrIri : Yes
        , isBlankNodeOrAnyLiteral : No
        , isIriOrAnyLiteral : Yes
        }


{-| -}
type alias BlankNode =
    Node
        { isBlankNode : Yes
        , isIri : No
        , isAnyLiteral : No
        , isBlankNodeOrIri : Yes
        , isBlankNodeOrAnyLiteral : Yes
        , isIriOrAnyLiteral : No
        }


{-| -}
type alias Literal a =
    Node
        { isLiteral : a
        , isBlankNode : No
        , isIri : No
        , isAnyLiteral : Yes
        , isBlankNodeOrIri : No
        , isBlankNodeOrAnyLiteral : Yes
        , isIriOrAnyLiteral : Yes
        }


{-| -}
type alias BlankNodeOrIri =
    Node
        { isBlankNode : No
        , isIri : No
        , isAnyLiteral : No
        , isBlankNodeOrIri : Yes
        , isBlankNodeOrAnyLiteral : No
        , isIriOrAnyLiteral : No
        }


{-| -}
type alias AnyLiteral =
    Node
        { isBlankNode : No
        , isIri : No
        , isAnyLiteral : Yes
        , isBlankNodeOrIri : No
        , isBlankNodeOrAnyLiteral : Yes
        , isIriOrAnyLiteral : Yes
        }


{-| -}
type alias BlankNodeOrIriOrAnyLiteral =
    Node
        { isBlankNode : No
        , isIri : No
        , isAnyLiteral : No
        , isBlankNodeOrIri : No
        , isBlankNodeOrAnyLiteral : No
        , isIriOrAnyLiteral : No
        }


{-| -}
type alias IsIri compatible =
    Node { compatible | isIri : Yes }


{-| -}
type alias IsBlankNode compatible =
    Node { compatible | isBlankNode : Yes }


{-| -}
type alias IsBlankNodeOrIri compatible =
    Node { compatible | isBlankNodeOrIri : Yes }


{-| -}
type alias IsAnyLiteral compatible =
    Node { compatible | isAnyLiteral : Yes }


{-| -}
type alias IsBlankNodeOrIriOrAnyLiteral compatible =
    Node compatible


{-| TODO Add documentation
-}
type alias NTriple =
    { subject : BlankNodeOrIri
    , predicate : Iri
    , object : BlankNodeOrIriOrAnyLiteral
    }



-- CREATE


{-| -}
iri : String -> Iri
iri value =
    Node (Iri value)


{-| -}
blankNode : String -> BlankNode
blankNode value =
    Node (BlankNode value)


{-| -}
literal : Iri -> String -> Literal a
literal datatype value =
    Node
        (Literal
            { value = value
            , datatype = datatype
            , languageTag = Nothing
            }
        )


{-| -}
string : String -> Literal String
string value =
    Node
        (Literal
            { value = value
            , datatype = xsdString
            , languageTag = Nothing
            }
        )


{-| -}
langString : String -> String -> Literal a
langString languageTag value =
    Node
        (Literal
            { value = value
            , datatype = rdfLangString
            , languageTag = Just languageTag
            }
        )


{-| -}
int : Int -> Literal Int
int value =
    Node
        (Literal
            { value = String.fromInt value
            , datatype = xsdInteger
            , languageTag = Nothing
            }
        )


{-| -}
float : Float -> Literal Float
float value =
    Node
        (Literal
            { value = String.fromFloat value
            , datatype = xsdDouble
            , languageTag = Nothing
            }
        )


{-| -}
decimal : Decimal -> Literal Decimal
decimal value =
    Node
        (Literal
            { value = Decimal.toString value
            , datatype = xsdDecimal
            , languageTag = Nothing
            }
        )


{-| -}
date : Posix -> Literal Posix
date value =
    Node
        (Literal
            { value = String.left (4 + 1 + 2 + 1 + 2) (Iso8601.fromTime value)
            , datatype = xsdDate
            , languageTag = Nothing
            }
        )


{-| -}
dateTime : Posix -> Literal Posix
dateTime value =
    Node
        (Literal
            { value = Iso8601.fromTime value
            , datatype = xsdDateTime
            , languageTag = Nothing
            }
        )


{-| -}
bool : Bool -> Literal Bool
bool value =
    Node
        (Literal
            { value =
                if value then
                    "true"

                else
                    "false"
            , datatype = xsdBoolean
            , languageTag = Nothing
            }
        )



-- TRANSFORM


{-| -}
toIri : Node compatible -> Maybe Iri
toIri (Node node) =
    case node of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Just (Node node)

        Literal _ ->
            Nothing


{-| -}
toBlankNode : Node compatible -> Maybe BlankNode
toBlankNode (Node node) =
    case node of
        BlankNode _ ->
            Just (Node node)

        Iri _ ->
            Nothing

        Literal _ ->
            Nothing


{-| -}
toBlankNodeOrIri : Node compatible -> Maybe BlankNodeOrIri
toBlankNodeOrIri (Node node) =
    case node of
        BlankNode _ ->
            Just (Node node)

        Iri _ ->
            Just (Node node)

        Literal _ ->
            Nothing


{-| -}
toAnyLiteral : Node compatible -> Maybe AnyLiteral
toAnyLiteral (Node node) =
    case node of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Nothing

        Literal _ ->
            Just (Node node)


{-| -}
toBlankNodeOrIriOrAnyLiteral : Node compatible -> Maybe BlankNodeOrIriOrAnyLiteral
toBlankNodeOrIriOrAnyLiteral (Node node) =
    case node of
        BlankNode _ ->
            Just (Node node)

        Iri _ ->
            Just (Node node)

        Literal _ ->
            Nothing


{-| -}
asIri : IsIri compatible -> Iri
asIri (Node node) =
    Node node


{-| -}
asBlankNode : IsIri compatible -> BlankNode
asBlankNode (Node node) =
    Node node


{-| -}
asLiteral : IsIri compatible -> Literal a
asLiteral (Node node) =
    Node node


{-| -}
asBlankNodeOrIri : IsBlankNodeOrIri compatible -> BlankNodeOrIri
asBlankNodeOrIri (Node node) =
    Node node


{-| -}
asBlankNodeOrIriOrAnyLiteral : Node compatible -> BlankNodeOrIriOrAnyLiteral
asBlankNodeOrIriOrAnyLiteral (Node node) =
    Node node


{-| -}
asAnyLiteral : Node compatible -> AnyLiteral
asAnyLiteral (Node node) =
    Node node


{-| -}
toUrl : Iri -> String
toUrl (Node node) =
    case node of
        BlankNode _ ->
            ""

        Iri url ->
            url

        Literal _ ->
            ""


{-| -}
toValue : AnyLiteral -> String
toValue (Node node) =
    case node of
        BlankNode _ ->
            ""

        Iri _ ->
            ""

        Literal { value } ->
            value


{-| -}
toString : Node compatible -> Maybe String
toString (Node node) =
    case node of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Nothing

        Literal data ->
            if data.datatype == xsdString then
                Just data.value

            else
                Nothing


{-| -}
toLangString : Node compatible -> Maybe ( String, String )
toLangString (Node node) =
    case node of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Nothing

        Literal data ->
            if data.datatype == rdfLangString then
                Maybe.map2 Tuple.pair data.languageTag (Just data.value)

            else
                Nothing


{-| -}
toInt : Node compatible -> Maybe Int
toInt (Node node) =
    case node of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Nothing

        Literal data ->
            if data.datatype == xsdInt then
                String.toInt data.value

            else if data.datatype == xsdInteger then
                String.toInt data.value

            else
                Nothing


{-| -}
toFloat : Node compatible -> Maybe Float
toFloat (Node node) =
    case node of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Nothing

        Literal data ->
            if data.datatype == xsdDouble then
                String.toFloat data.value

            else
                Nothing


{-| -}
toDecimal : Node compatible -> Maybe Decimal
toDecimal (Node node) =
    case node of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Nothing

        Literal data ->
            if data.datatype == xsdDecimal then
                Decimal.fromString data.value

            else
                Nothing


{-| -}
toDate : Node compatible -> Maybe Posix
toDate (Node node) =
    case node of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Nothing

        Literal data ->
            if data.datatype == xsdDate then
                (data.value ++ "T00:00:00.000Z")
                    |> Iso8601.toTime
                    |> Result.toMaybe

            else
                Nothing


{-| -}
toDateTime : Node compatible -> Maybe Posix
toDateTime (Node node) =
    case node of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Nothing

        Literal data ->
            if data.datatype == xsdDateTime then
                data.value
                    |> Iso8601.toTime
                    |> Result.toMaybe

            else
                Nothing


{-| -}
toBool : Node compatible -> Maybe Bool
toBool (Node node) =
    case node of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Nothing

        Literal data ->
            if data.datatype == xsdBoolean then
                case data.value of
                    "true" ->
                        Just True

                    "false" ->
                        Just False

                    _ ->
                        Nothing

            else
                Nothing


{-| -}
appendPath : String -> IsIri compatible -> Iri
appendPath segment (Node node) =
    case node of
        BlankNode stuff ->
            Node node

        Iri url ->
            case String.split "?" url of
                [ _ ] ->
                    case String.split "#" url of
                        [ _ ] ->
                            Node (Iri (url ++ segment))

                        [ beforeFragment, fragment ] ->
                            Node (Iri (beforeFragment ++ segment ++ "#" ++ fragment))

                        _ ->
                            Node (Iri (url ++ segment))

                [ beforeQuery, rest ] ->
                    case String.split "#" rest of
                        [ _ ] ->
                            Node (Iri (beforeQuery ++ segment ++ "?" ++ rest))

                        [ query, fragment ] ->
                            Node (Iri (beforeQuery ++ segment ++ "?" ++ query ++ "#" ++ fragment))

                        _ ->
                            Node (Iri (url ++ segment))

                _ ->
                    Node (Iri (url ++ segment))

        Literal stuff ->
            Node node


{-| -}
dropFragment : IsIri compatible -> Iri
dropFragment (Node node) =
    case node of
        BlankNode stuff ->
            Node node

        Iri url ->
            case String.split "#" url of
                [ _ ] ->
                    Node node

                [ beforeFragment, fragment ] ->
                    Node (Iri beforeFragment)

                _ ->
                    Node node

        Literal stuff ->
            Node node


{-| -}
setFragment : String -> IsIri compatible -> Iri
setFragment fragment (Node node) =
    case node of
        BlankNode stuff ->
            Node node

        Iri url ->
            case String.split "#" url of
                [ _ ] ->
                    Node (Iri (url ++ "#" ++ fragment))

                [ beforeFragment, _ ] ->
                    Node (Iri (beforeFragment ++ "#" ++ fragment))

                _ ->
                    Node node

        Literal stuff ->
            Node node



-- SERIALIZE


{-| TODO Add documentation
-}
serializeNode : Node compatible -> String
serializeNode (Node node) =
    serializeNodeHelp node


{-| TODO Add documentation
-}
serializeNodeHelp : NodeInternal -> String
serializeNodeHelp node =
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
                    "^^" ++ serializeNode data.datatype

                Just languageTag ->
                    "@" ++ languageTag
            ]
                |> String.concat


{-| TODO Add documentation
-}
serializeNTriple : NTriple -> String
serializeNTriple { subject, predicate, object } =
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
nTripleDecoder : Decoder NTriple
nTripleDecoder =
    Decode.succeed NTriple
        |> Decode.required "subject" subjectDecoder
        |> Decode.required "predicate" predicateDecoder
        |> Decode.required "object" objectDecoder


subjectDecoder : Decoder BlankNodeOrIri
subjectDecoder =
    [ blankNodeDecoder
    , iriDecoder
    ]
        |> Decode.oneOf
        |> Decode.map Node


predicateDecoder : Decoder Iri
predicateDecoder =
    iriDecoder
        |> Decode.map Node


objectDecoder : Decoder BlankNodeOrIriOrAnyLiteral
objectDecoder =
    [ blankNodeDecoder
    , literalDecoder
    , iriDecoder
    ]
        |> Decode.oneOf
        |> Decode.map Node


blankNodeDecoder : Decoder NodeInternal
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


iriDecoder : Decoder NodeInternal
iriDecoder =
    Decode.string
        |> Decode.field "termType"
        |> Decode.andThen
            (\termType ->
                if termType == "NamedNode" then
                    Decode.string
                        |> Decode.field "value"
                        |> Decode.map Iri

                else
                    Decode.fail "not a named node"
            )


literalDecoder : Decoder NodeInternal
literalDecoder =
    Decode.string
        |> Decode.field "termType"
        |> Decode.andThen
            (\termType ->
                if termType == "Literal" then
                    Decode.succeed LiteralData
                        |> Decode.required "value" Decode.string
                        |> Decode.required "datatype" (Decode.map Node iriDecoder)
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
encodeNTriple : NTriple -> Value
encodeNTriple nTriple =
    [ ( "subject", encodeSubject nTriple.subject )
    , ( "predicate", encodePredicate nTriple.predicate )
    , ( "object", encodeObject nTriple.object )
    ]
        |> Encode.object


encodeSubject : BlankNodeOrIri -> Value
encodeSubject (Node node) =
    case node of
        BlankNode name ->
            encodeBlankNode name

        Iri url ->
            encodeIri url

        Literal _ ->
            Encode.null


encodePredicate : Iri -> Value
encodePredicate (Node node) =
    case node of
        BlankNode _ ->
            Encode.null

        Iri url ->
            encodeIri url

        Literal _ ->
            Encode.null


encodeObject : BlankNodeOrIriOrAnyLiteral -> Value
encodeObject (Node node) =
    case node of
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


encodeLiteral : LiteralData -> Value
encodeLiteral data =
    [ ( "termType", Encode.string "Literal" )
    , ( "value", Encode.string data.value )
    , ( "datatype"
      , case data.datatype of
            Node (Iri url) ->
                encodeIri url

            _ ->
                Encode.null
      )
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


xsdString : Iri
xsdString =
    xsd "string"


rdfLangString : Iri
rdfLangString =
    rdf "langString"


xsdInt : Iri
xsdInt =
    xsd "int"


xsdInteger : Iri
xsdInteger =
    xsd "integer"


xsdDouble : Iri
xsdDouble =
    xsd "double"


xsdDecimal : Iri
xsdDecimal =
    xsd "decimal"


xsdDate : Iri
xsdDate =
    xsd "date"


xsdDateTime : Iri
xsdDateTime =
    xsd "dateTime"


xsdBoolean : Iri
xsdBoolean =
    xsd "boolean"


xsd : String -> Iri
xsd name =
    Node (Iri ("http://www.w3.org/2001/XMLSchema#" ++ name))


rdf : String -> Iri
rdf name =
    Node (Iri ("http://www.w3.org/1999/02/22-rdf-syntax-ns#" ++ name))
