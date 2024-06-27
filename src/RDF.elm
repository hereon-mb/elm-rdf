module RDF exposing
    ( Iri, BlankNode, Literal, LiteralData
    , BlankNodeOrIri, BlankNodeOrIriOrAnyLiteral, AnyLiteral
    , IsBlankNodeOrIri, IsIri
    , NTriple
    , Node(..), Yes, No, NodeInternal(..)
    , forgetCompatible
    , unwrap
    , blankNode, iriAbsolute, literalWithDatatype, literal, bool, string, int
    , toIri, toBlankNodeOrIri, toBlankNodeOrIriOrAnyLiteral, toAnyLiteral
    , toUrl
    , toString, toLangString
    , toInt, toFloat, toDecimal
    , toDate, toDateTime
    , toBool
    , serializeNode, serializeNTriple, serializeNodeHelp
    , encodeNTriple
    , nTripleDecoder
    , toValue
    , StringOrLangString(..), localize, nonLocalized, stringOrLangStringFrom, stringOrLangStringFromList, mergeStringOrLangStrings
    )

{-|


# Node

@docs Iri, BlankNode, Literal, LiteralData
@docs BlankNodeOrIri, BlankNodeOrIriOrAnyLiteral, AnyLiteral
@docs IsBlankNodeOrIri, IsIri
@docs NTriple
@docs Node, Yes, No, NodeInternal
@docs forgetCompatible
@docs unwrap


## Create

@docs blankNode, iriAbsolute, literalWithDatatype, literal, bool, string, int


## Transform

@docs toIri, toBlankNodeOrIri, toBlankNodeOrIriOrAnyLiteral, toAnyLiteral
@docs toUrl
@docs toString, toLangString
@docs toInt, toFloat, toDecimal
@docs toDate, toDateTime
@docs toBool


## Serialize

@docs serializeNode, serializeNTriple, serializeNodeHelp
@docs encodeNTriple
@docs nTripleDecoder


## Retrieve


## Filter

@docs toValue

@docs StringOrLangString, localize, nonLocalized, stringOrLangStringFrom, stringOrLangStringFromList, mergeStringOrLangStrings

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
type Yes
    = Yes Never


{-| TODO Add documentation
-}
type No
    = No Never


{-| FIXME internals exposed for benchmarks
-}
type Node compatible
    = Node NodeInternal


{-| FIXME internals exposed for benchmarks
-}
type NodeInternal
    = BlankNode String
    | Iri String
    | Literal LiteralData


{-| TODO Add documentation
-}
type alias LiteralData =
    { value : String
    , datatype : Iri
    , languageTag : Maybe String
    }


{-| TODO Add documentation
-}
type alias Iri =
    Node
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
    Node
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
    Node
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
    Node
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
    Node
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
    Node
        { isBlankNode : No
        , isIri : No
        , isAnyLiteral : No
        , isBlankNodeOrIri : No
        , isBlankNodeOrAnyLiteral : No
        , isIriOrAnyLiteral : No
        }


{-| TODO Add documentation
-}
type alias NTriple =
    { subject : BlankNodeOrIri
    , predicate : Iri
    , object : BlankNodeOrIriOrAnyLiteral
    }


{-| TODO Add documentation
-}
type alias IsBlankNodeOrIri compatible =
    Node { compatible | isBlankNodeOrIri : Yes }


{-| TODO Add documentation
-}
type alias IsIri compatible =
    Node { compatible | isIri : Yes }


{-| TODO Add documentation
-}
blankNode : String -> BlankNode
blankNode value =
    Node (BlankNode value)


{-| TODO Add documentation
-}
iriAbsolute : String -> Iri
iriAbsolute value =
    Node (Iri value)


{-| TODO Add documentation
-}
literalWithDatatype : Iri -> String -> Literal a
literalWithDatatype datatype value =
    Node
        (Literal
            { value = value
            , datatype = datatype
            , languageTag = Nothing
            }
        )


{-| TODO Add documentation
-}
literal : Iri -> Maybe String -> String -> Literal a
literal datatype languageTag value =
    -- FIXME check datatype iri when languageTag is present
    Node
        (Literal
            { value = value
            , datatype = datatype
            , languageTag = languageTag
            }
        )


{-| TODO Add documentation
-}
bool : Bool -> Literal Bool
bool value =
    Node
        (Literal
            { value =
                if value then
                    "true"

                else
                    "false"
            , datatype = xsd "boolean"
            , languageTag = Nothing
            }
        )


{-| TODO Add documentation
-}
string : String -> Literal String
string value =
    Node
        (Literal
            { value = value
            , datatype = xsd "string"
            , languageTag = Nothing
            }
        )


{-| TODO Add documentation
-}
int : Int -> Literal Int
int value =
    Node
        (Literal
            { value = String.fromInt value
            , datatype = xsd "integer"
            , languageTag = Nothing
            }
        )


{-| TODO Add documentation
-}
toBlankNodeOrIri : IsBlankNodeOrIri compatible -> BlankNodeOrIri
toBlankNodeOrIri (Node node) =
    Node node


{-| TODO Add documentation
-}
toBlankNodeOrIriOrAnyLiteral : Node compatible -> BlankNodeOrIriOrAnyLiteral
toBlankNodeOrIriOrAnyLiteral (Node node) =
    Node node


{-| TODO Add documentation
-}
toIri : Node compatible -> Maybe Iri
toIri (Node node) =
    case node of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Just (Node node)

        Literal _ ->
            Nothing


{-| TODO Add documentation
-}
toUrl : Iri -> String
toUrl (Node node) =
    case node of
        BlankNode _ ->
            ""

        Iri url ->
            url

        Literal _ ->
            ""


{-| TODO Add documentation
-}
toAnyLiteral : Node compatible -> Maybe AnyLiteral
toAnyLiteral (Node node) =
    case node of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Nothing

        Literal _ ->
            Just (Node node)


{-| TODO Add documentation
-}
toValue : AnyLiteral -> String
toValue (Node node) =
    case node of
        BlankNode _ ->
            ""

        Iri _ ->
            ""

        Literal { value } ->
            value


{-| TODO Add documentation
-}
toBool : Node compatible -> Maybe Bool
toBool (Node node) =
    case node of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Nothing

        Literal data ->
            if data.datatype == xsd "boolean" then
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
toString : Node compatible -> Maybe String
toString (Node node) =
    case node of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Nothing

        Literal data ->
            if data.datatype == xsd "string" then
                Just data.value

            else
                Nothing


{-| TODO Add documentation
-}
toInt : Node compatible -> Maybe Int
toInt (Node node) =
    case node of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Nothing

        Literal data ->
            if data.datatype == xsd "int" then
                String.toInt data.value

            else if data.datatype == xsd "integer" then
                String.toInt data.value

            else
                Nothing


{-| TODO Add documentation
-}
toFloat : Node compatible -> Maybe Float
toFloat (Node node) =
    case node of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Nothing

        Literal data ->
            if data.datatype == xsd "float" then
                String.toFloat data.value

            else
                Nothing


{-| TODO Add documentation
-}
toDecimal : Node compatible -> Maybe Decimal
toDecimal (Node node) =
    case node of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Nothing

        Literal data ->
            if data.datatype == xsd "decimal" then
                Decimal.fromString data.value

            else
                Nothing


{-| TODO Add documentation
-}
toDate : Node compatible -> Maybe Posix
toDate (Node node) =
    case node of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Nothing

        Literal data ->
            if data.datatype == xsd "date" then
                (data.value ++ "T00:00:00.000Z")
                    |> Iso8601.toTime
                    |> Result.toMaybe

            else
                Nothing


{-| TODO Add documentation
-}
toDateTime : Node compatible -> Maybe Posix
toDateTime (Node node) =
    case node of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Nothing

        Literal data ->
            if data.datatype == xsd "dateTime" then
                data.value
                    |> Iso8601.toTime
                    |> Result.toMaybe

            else
                Nothing


{-| TODO Add documentation
-}
toLangString : Node compatible -> Maybe ( String, String )
toLangString (Node node) =
    case node of
        BlankNode _ ->
            Nothing

        Iri _ ->
            Nothing

        Literal data ->
            if data.datatype == rdf "langString" then
                Maybe.map2 Tuple.pair data.languageTag (Just data.value)

            else
                Nothing


{-| TODO Add documentation
-}
forgetCompatible : Node compatible1 -> Node compatible2
forgetCompatible (Node node) =
    Node node


{-| TODO Add documentation
-}
unwrap : Node compatible -> NodeInternal
unwrap (Node node) =
    node


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
            in
            [ "\""
            , replaceLineBreaks data.value
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
type StringOrLangString
    = StringOrLangString
        { string : Maybe String
        , langStrings : Dict String String
        }


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

        Iri iri ->
            encodeIri iri

        Literal _ ->
            Encode.null


encodePredicate : Iri -> Value
encodePredicate (Node node) =
    case node of
        BlankNode _ ->
            Encode.null

        Iri iri ->
            encodeIri iri

        Literal _ ->
            Encode.null


encodeObject : BlankNodeOrIriOrAnyLiteral -> Value
encodeObject (Node node) =
    case node of
        BlankNode name ->
            encodeBlankNode name

        Iri iri ->
            encodeIri iri

        Literal data ->
            encodeLiteral data


encodeBlankNode : String -> Value
encodeBlankNode name =
    [ ( "termType", Encode.string "BlankNode" )
    , ( "value", Encode.string name )
    ]
        |> Encode.object


encodeIri : String -> Value
encodeIri iri =
    [ ( "termType", Encode.string "NamedNode" )
    , ( "value", Encode.string iri )
    ]
        |> Encode.object


encodeLiteral : LiteralData -> Value
encodeLiteral data =
    [ ( "termType", Encode.string "Literal" )
    , ( "value", Encode.string data.value )
    , ( "datatype"
      , case data.datatype of
            Node (Iri iriDatatype) ->
                encodeIri iriDatatype

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


xsd : String -> Iri
xsd name =
    Node (Iri ("http://www.w3.org/2001/XMLSchema#" ++ name))


rdf : String -> Iri
rdf name =
    Node (Iri ("http://www.w3.org/1999/02/22-rdf-syntax-ns#" ++ name))
