module RDF exposing
    ( Iri, BlankNode, Literal, LiteralData
    , BlankNodeOrIri, BlankNodeOrIriOrAnyLiteral, AnyLiteral
    , IsBlankNodeOrIri, IsIri
    , NTriple
    , Node(..), Yes, No, NodeInternal(..)
    , forgetCompatible
    , unwrap
    , blankNode, iriAbsolute, literalWithDatatype, literal, bool, string, int
    , dash, dcterms, owl, qudt, rdf, rdfs, sh, xsd, a, prov
    , toIri, toUrl, toBlankNodeOrIri, toString, toLangString, toInt, toFloat, toDecimal, toDate, toDateTime, toAnyLiteral, toBool
    , serializeNode, serializeNTriple, serializeNodeHelp
    , encodeNTriple
    , nTripleDecoder
    , parseNTriples
    , Error
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


### Namespaces

@docs dash, dcterms, owl, qudt, rdf, rdfs, sh, xsd, a, prov


## Transform

@docs toIri, toUrl, toBlankNodeOrIri, toString, toLangString, toInt, toFloat, toDecimal, toDate, toDateTime, toAnyLiteral, toBool


## Serialize

@docs serializeNode, serializeNTriple, serializeNodeHelp
@docs encodeNTriple
@docs nTripleDecoder
@docs parseNTriples

@docs Error


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
import Parser exposing ((|.), (|=), Parser)
import Set
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
                case value of
                    True ->
                        "true"

                    False ->
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
type alias Error =
    { row : Int
    , line : String
    , deadEnds : List Parser.DeadEnd
    }


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


{-| Get all N-Triples from a n-triple text file.
-}
parseNTriples : String -> Result (List Error) (List NTriple)
parseNTriples raw =
    let
        collect line ( row, result ) =
            ( row + 1
            , if line == "" then
                result

              else
                case result of
                    Ok collected ->
                        case Parser.run nTripleParser line of
                            Err deadEnds ->
                                Err
                                    [ { row = row
                                      , line = line
                                      , deadEnds = deadEnds
                                      }
                                    ]

                            Ok nTriple ->
                                Ok (nTriple :: collected)

                    Err collected ->
                        case Parser.run nTripleParser line of
                            Err deadEnds ->
                                Err
                                    ({ row = row
                                     , line = line
                                     , deadEnds = deadEnds
                                     }
                                        :: collected
                                    )

                            Ok _ ->
                                result
            )
    in
    raw
        |> String.lines
        |> List.foldl collect ( 1, Ok [] )
        |> Tuple.second
        |> Result.map List.reverse



-- PARSER


nTripleParser : Parser NTriple
nTripleParser =
    Parser.succeed NTriple
        |= subjectParser
        |. whitespace
        |= Parser.map Node iriParser
        |. whitespace
        |= objectParser
        |. whitespace
        |. Parser.symbol "."


subjectParser : Parser BlankNodeOrIri
subjectParser =
    [ iriParser
    , blankNodeParser
    ]
        |> Parser.oneOf
        |> Parser.map Node


objectParser : Parser BlankNodeOrIriOrAnyLiteral
objectParser =
    [ iriParser
    , blankNodeParser
    , literalParser
    ]
        |> Parser.oneOf
        |> Parser.map Node


whitespace : Parser ()
whitespace =
    Parser.chompWhile
        (\char -> char == ' ' || char == '\t')


iriParser : Parser NodeInternal
iriParser =
    Parser.succeed Iri
        |. Parser.symbol "<"
        |= iriAbsoluteParserHelp
        |. Parser.symbol ">"


iriAbsoluteParserHelp : Parser String
iriAbsoluteParserHelp =
    Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompWhile allowedIriChar


allowedIriChar : Char -> Bool
allowedIriChar char =
    (char /= '<')
        && (char /= '>')
        && (char /= '{')
        && (char /= '}')
        && (char /= '|')
        && (char /= '^')
        && (char /= '`')
        && (char /= '\\')
        && (Char.toCode char > 0x20)


blankNodeParser : Parser NodeInternal
blankNodeParser =
    Parser.succeed BlankNode
        |. Parser.symbol "_:"
        |= Parser.andThen checkForTrailingDot blankNodeLabelParser


checkForTrailingDot : String -> Parser String
checkForTrailingDot value =
    if String.endsWith "." value then
        Parser.problem "Blank node must not end with '.'"

    else
        Parser.succeed value


blankNodeLabelParser : Parser String
blankNodeLabelParser =
    Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompIf (\char -> Char.isDigit char || isPnCharsU char)
            |. Parser.chompWhile (\char -> isPnChars char || char == '.')


isPnCharsBase : Char -> Bool
isPnCharsBase char =
    let
        code =
            Char.toCode char
    in
    Char.isAlpha char
        || (code >= 0xC0 && code <= 0xD6)
        || (code >= 0xD8 && code <= 0xF6)
        || (code >= 0xF8 && code <= 0x02FF)
        || (code >= 0x0370 && code <= 0x037D)
        || (code >= 0x037F && code <= 0x1FFF)
        || (code >= 0x200C && code <= 0x200D)
        || (code >= 0x2070 && code <= 0x218F)
        || (code >= 0x2C00 && code <= 0x2FEF)
        || (code >= 0x3001 && code <= 0xD7FF)
        || (code >= 0xF900 && code <= 0xFDCF)
        || (code >= 0xFDF0 && code <= 0xFFFD)
        || (code >= 0x00010000 && code <= 0x000EFFFF)


isPnCharsU : Char -> Bool
isPnCharsU char =
    isPnCharsBase char || char == '_' || char == ':'


isPnChars : Char -> Bool
isPnChars char =
    let
        code =
            Char.toCode char
    in
    isPnCharsU char
        || (char == '-')
        || Char.isDigit char
        || (code == 0xB7)
        || (code >= 0x0300 && code <= 0x036F)
        || (code >= 0x203F && code <= 0x2040)


literalParser : Parser NodeInternal
literalParser =
    Parser.succeed (\value ( datatype, languageTag ) -> LiteralData value datatype languageTag)
        |. Parser.symbol "\""
        |= stringLiteralQuoteParser
        |= Parser.oneOf
            [ Parser.succeed (\datatype -> ( Node datatype, Nothing ))
                |. Parser.symbol "^^"
                |= iriParser
            , Parser.succeed
                (\left maybeRight ->
                    case maybeRight of
                        Nothing ->
                            ( rdf "langString", Just left )

                        Just right ->
                            ( rdf "langString", Just (left ++ "-" ++ right) )
                )
                |. Parser.symbol "@"
                |= Parser.variable
                    { start = Char.isAlpha
                    , inner = Char.isAlpha
                    , reserved = Set.empty
                    }
                |= Parser.oneOf
                    [ Parser.succeed Just
                        |. Parser.symbol "-"
                        |= Parser.variable
                            { start = Char.isAlphaNum
                            , inner = Char.isAlphaNum
                            , reserved = Set.empty
                            }
                    , Parser.succeed Nothing
                    ]
            , Parser.succeed ( xsd "string", Nothing )
            ]
        |> Parser.map Literal


stringLiteralQuoteParser : Parser String
stringLiteralQuoteParser =
    Parser.loop [] stringLiteralQuoteParserHelp


stringLiteralQuoteParserHelp : List String -> Parser (Parser.Step (List String) String)
stringLiteralQuoteParserHelp revChunks =
    Parser.oneOf
        [ Parser.succeed (\chunk -> Parser.Loop (chunk :: revChunks))
            |. Parser.token "\\"
            |= Parser.oneOf
                [ Parser.map (\_ -> "\n") (Parser.token "n")
                , Parser.map (\_ -> "\t") (Parser.token "t")
                , Parser.map (\_ -> "\"") (Parser.token "\"")
                , Parser.map (\_ -> "\\") (Parser.token "\\")
                , Parser.map (\_ -> "\u{000D}") (Parser.token "r")
                , Parser.succeed String.fromChar
                    |. Parser.token "u{"
                    |= unicode
                    |. Parser.token "}"
                ]
        , Parser.token "\""
            |> Parser.map (\_ -> Parser.Done (String.join "" (List.reverse revChunks)))
        , Parser.chompWhile isUninteresting
            |> Parser.getChompedString
            |> Parser.map (\chunk -> Parser.Loop (chunk :: revChunks))
        ]


isUninteresting : Char -> Bool
isUninteresting char =
    char /= '\\' && char /= '"'



-- UNICODE


unicode : Parser Char
unicode =
    Parser.getChompedString (Parser.chompWhile Char.isHexDigit)
        |> Parser.andThen codeToChar


codeToChar : String -> Parser Char
codeToChar str =
    let
        length =
            String.length str

        code =
            String.foldl addHex 0 str
    in
    if 4 <= length && length <= 6 then
        Parser.problem "code point must have between 4 and 6 digits"

    else if 0 <= code && code <= 0x0010FFFF then
        Parser.succeed (Char.fromCode code)

    else
        Parser.problem "code point must be between 0 and 0x10FFFF"


addHex : Char -> Int -> Int
addHex char total =
    let
        code =
            Char.toCode char
    in
    if 0x30 <= code && code <= 0x39 then
        16 * total + (code - 0x30)

    else if 0x41 <= code && code <= 0x46 then
        16 * total + (10 + code - 0x41)

    else
        16 * total + (10 + code - 0x61)



-- NAMESPACES


{-| TODO Add documentation
-}
dash : String -> Iri
dash name =
    Node (Iri ("http://datashapes.org/dash#" ++ name))


{-| TODO Add documentation
-}
dcterms : String -> Iri
dcterms name =
    Node (Iri ("http://purl.org/dc/terms/" ++ name))


{-| TODO Add documentation
-}
owl : String -> Iri
owl name =
    Node (Iri ("http://www.w3.org/2002/07/owl#" ++ name))


{-| TODO Add documentation
-}
qudt : String -> Iri
qudt name =
    Node (Iri ("http://qudt.org/schema/qudt/" ++ name))


{-| TODO Add documentation
-}
rdf : String -> Iri
rdf name =
    Node (Iri ("http://www.w3.org/1999/02/22-rdf-syntax-ns#" ++ name))


{-| TODO Add documentation
-}
rdfs : String -> Iri
rdfs name =
    Node (Iri ("http://www.w3.org/2000/01/rdf-schema#" ++ name))


{-| TODO Add documentation
-}
sh : String -> Iri
sh name =
    Node (Iri ("http://www.w3.org/ns/shacl#" ++ name))


{-| TODO Add documentation
-}
xsd : String -> Iri
xsd name =
    Node (Iri ("http://www.w3.org/2001/XMLSchema#" ++ name))


{-| TODO Add documentation
-}
prov : String -> Iri
prov name =
    Node (Iri ("http://www.w3.org/ns/prov#" ++ name))


{-| TODO Add documentation
-}
a : Iri
a =
    rdf "type"
