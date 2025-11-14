module Rdf.Decode exposing
    ( Decoder
    , iri
    , blankNodeOrIri, anyLiteral, blankNodeOrIriOrAnyLiteral
    , object
    , literal
    , string, stringOrLangString
    , bool, int, float, number
    , date, dateTime
    , subject
    , from, fromSubject
    , blankNode
    , predicate, property, anyPredicate
    , noProperty
    , predicates
    , list, nonEmpty
    , at
    , decode
    , Error(..), errorToString
    , map
    , map2
    , combine
    , required, requiredAt
    , optional, optionalAt
    , hardcoded
    , custom
    , succeed, fail, failWith
    , andThen
    , oneOf
    , many, indexedMany
    , lazy
    )

{-| The `DefaultValue` API is a parser combinator for functions `Node -> a` that supports querying.

The `Rdf` functions, say, `toIri`, are functions `Node -> a`, but do not allow for combination or querying.

The `Rdf.Query` functions are functions `Graph -> a`, that do support querying (duh!), but no also combination.

So there _is_ some value there, but I think inlining the module out-of-existence might not be too bad of code duplication, either.


# Basic Decoders

@docs Decoder

@docs iri
@docs blankNodeOrIri, anyLiteral, blankNodeOrIriOrAnyLiteral
@docs object

@docs literal
@docs string, stringOrLangString
@docs bool, int, float, number
@docs date, dateTime

@docs subject


# Finding Values

@docs from, fromSubject
@docs blankNode
@docs predicate, property, anyPredicate
@docs noProperty
@docs predicates
@docs list, nonEmpty
@docs at


# Running Decoders

@docs decode
@docs Error, errorToString


# Transforming Values

@docs map
@docs map2
@docs combine


## Pipeline

@docs required, requiredAt
@docs optional, optionalAt
@docs hardcoded
@docs custom


# Combining Decoders

@docs succeed, fail, failWith
@docs andThen
@docs oneOf
@docs many, indexedMany
@docs lazy

-}

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Internal.Graph exposing (Graph(..))
import Internal.Term exposing (DataLiteral, Term(..), Variant(..))
import List.Extra as List
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Maybe.Extra as Maybe
import Rdf
    exposing
        ( BlankNodeOrIri
        , BlankNodeOrIriOrAnyLiteral
        , Iri
        , IsBlankNodeOrIri
        , IsBlankNodeOrIriOrAnyLiteral
        , IsIri
        )
import Rdf.Namespaces as Rdf exposing (rdf)
import Rdf.PropertyPath as Rdf exposing (PropertyPath)
import Result.Extra as Result
import Time


{-| Decode a list of focus nodes by the given decoder.

Note that the position of `many` within a decode expression matters. Say, a focus node has multiple classes.

The following decoder works as expected:

    property a (many class)

Hoever, the following (type-valid) decoder does NOT work:

    many (property a class)

Note that `property a class` alone fails (since there are many classes), and so `many (property a class)` fails as a whole!

-}
many : Decoder a -> Decoder (List a)
many (Decoder f) =
    Decoder
        (\graph ->
            Result.andThen
                (\nodes ->
                    Result.combine (List.map (f graph << Ok << List.singleton) nodes)
                )
        )


{-| FIXME We also want a indexedManySafe which always succeeds by filtering out
the elements for which the provided decoder fails, and which re-uses an index
if the element failed decoding.
-}
indexedMany : (Int -> Decoder a) -> Decoder (List a)
indexedMany toDecoder =
    Decoder
        (\graph ->
            Result.andThen
                (\nodes ->
                    Result.combine
                        (List.indexedMap
                            (\index ->
                                let
                                    (Decoder f) =
                                        toDecoder index
                                in
                                List.singleton
                                    >> Ok
                                    >> f graph
                            )
                            nodes
                        )
                )
        )


{-| A way to specify what kind of thing you want to decode into.
-}
type Decoder a
    = Decoder (Graph -> Result Error (List BlankNodeOrIriOrAnyLiteral) -> Result Error a)


{-| TODO Add documentation
-}
at : List BlankNodeOrIriOrAnyLiteral -> Decoder a -> Decoder a
at nodes (Decoder f) =
    Decoder (\graph _ -> f graph (Ok nodes))


{-| TODO Add documentation
-}
object : Decoder BlankNodeOrIriOrAnyLiteral
object =
    Decoder
        (\_ ->
            Result.andThen
                (\nodes ->
                    case nodes of
                        [ node ] ->
                            Ok node

                        _ ->
                            Err (TooManyNodes nodes)
                )
        )


{-| TODO Add documentation
-}
from : IsBlankNodeOrIri compatible -> Decoder a -> Decoder a
from nodeFocus (Decoder f) =
    Decoder
        (\graph _ ->
            if
                hasTripleWithSubject nodeFocus graph
                    || hasTripleWithObject nodeFocus graph
            then
                f graph (Ok [ Rdf.asBlankNodeOrIriOrAnyLiteral nodeFocus ])

            else
                Err (NodeDoesNotExist (Rdf.asBlankNodeOrIri nodeFocus))
        )


{-| TODO Add documentation
-}
fromSubject : Decoder a -> Decoder a
fromSubject (Decoder f) =
    Decoder
        (\graph _ ->
            graph
                |> getSubjects
                |> List.map Rdf.asBlankNodeOrIriOrAnyLiteral
                |> Ok
                |> f graph
        )


{-| Run a [`Decoder`](#Decoder) on an actual `Graph` starting at the provided
`Node`'s.
-}
decode : Decoder a -> Graph -> Result Error a
decode (Decoder f) graph =
    f graph (Err NoFocusNode)


{-| When the decoding fails, we get on of these. Use
[`errorToString`](#errorToString) to turn this into a human-friendly form.
-}
type Error
    = InvalidDate BlankNodeOrIriOrAnyLiteral
    | InvalidDateTime BlankNodeOrIriOrAnyLiteral
    | ExpectedBlankNode BlankNodeOrIriOrAnyLiteral
    | ExpectedIri BlankNodeOrIriOrAnyLiteral
    | ExpectedBlankNodeOrIri BlankNodeOrIriOrAnyLiteral
    | ExpectedAnyLiteral BlankNodeOrIriOrAnyLiteral
    | ExpectedBool String
    | ExpectedInt String
    | ExpectedFloat String
    | ExpectedLiteralDatatype Iri Iri
    | UnknownProperty BlankNodeOrIri PropertyPath
    | PropertyPresent BlankNodeOrIri PropertyPath
    | NoProperty BlankNodeOrIri
    | UnexpectedEmptyList
    | CustomError String
    | Batch (List Error)
    | TooManyNodes (List Rdf.BlankNodeOrIriOrAnyLiteral)
    | MissingLangString Rdf.AnyLiteral
    | TooManyStrings (List String)
    | NodeDoesNotExist BlankNodeOrIri
    | NoFocusNode


{-| Turn a decoding error into a human friendly string. E.g.

    errorToString (ExpectedBool "42")
    --> "Expected a boolean, but found 42."

-}
errorToString : Error -> String
errorToString error_ =
    case error_ of
        InvalidDate _ ->
            "Not a date"

        InvalidDateTime _ ->
            "Not a date time"

        ExpectedLiteralDatatype datatypeExpected datatypeFound ->
            "Expected a literal of type "
                ++ Rdf.toUrl datatypeExpected
                ++ ", but found a literal of type "
                ++ Rdf.toUrl datatypeFound
                ++ "."

        ExpectedBlankNode nodeFound ->
            "Expected a blank node, but found " ++ Rdf.serializeNode nodeFound ++ "."

        ExpectedIri nodeFound ->
            "Expected an IRI, but found " ++ Rdf.serializeNode nodeFound ++ "."

        ExpectedBlankNodeOrIri nodeFound ->
            "Expected a blank node or an IRI, but found " ++ Rdf.serializeNode nodeFound ++ "."

        ExpectedAnyLiteral nodeFound ->
            "Expected a literal, but found " ++ Rdf.serializeNode nodeFound ++ "."

        ExpectedBool found ->
            "Expected a boolean, but found " ++ found ++ "."

        ExpectedInt found ->
            "Expected an integer, but found " ++ found ++ "."

        ExpectedFloat found ->
            "Expected a float, but found " ++ found ++ "."

        UnknownProperty nodeFocus pathExpected ->
            "No such property "
                ++ Rdf.serializePropertyPath pathExpected
                ++ " found at "
                ++ Rdf.serializeNode nodeFocus
                ++ "."

        PropertyPresent nodeFocus pathExpected ->
            "Found object for property "
                ++ Rdf.serializePropertyPath pathExpected
                ++ " at "
                ++ Rdf.serializeNode nodeFocus
                ++ "."

        NoProperty nodeFocus ->
            "No property found at " ++ Rdf.serializeNode nodeFocus ++ "."

        UnexpectedEmptyList ->
            "Expected a non-empty list, but found an empty list."

        CustomError s ->
            s

        Batch errors ->
            "Decoding failed in one of the following ways:\n"
                ++ indent
                    (String.join "\n"
                        (List.map
                            (\errorNested ->
                                "- " ++ errorToString errorNested
                            )
                            errors
                        )
                    )

        TooManyNodes nodesFound ->
            "I expected a single node, but I found multiple:\n"
                ++ indent
                    (String.join "\n"
                        (List.map Rdf.serializeNode nodesFound)
                    )

        MissingLangString nodeFound ->
            "I expected a lang string, but I found "
                ++ Rdf.serializeNode nodeFound
                ++ "."

        TooManyStrings stringsFound ->
            "I expected a single string, but I found multiple strings "
                ++ String.join ", " stringsFound
                ++ "."

        NodeDoesNotExist node ->
            "The node "
                ++ Rdf.serializeNode node
                ++ " does not exist."

        NoFocusNode ->
            "There is no focus node to start decoding from."


indent : String -> String
indent lines =
    lines
        |> String.lines
        |> List.map (\line -> "    " ++ line)
        |> String.join "\n"


{-| TODO
-}
blankNode : Decoder a -> Decoder a
blankNode (Decoder f) =
    Decoder
        (\graph ->
            Result.andThen
                (\nodes ->
                    case nodes of
                        [ node ] ->
                            case Rdf.toBlankNode node of
                                Just nodeNext ->
                                    Ok [ Rdf.asBlankNodeOrIriOrAnyLiteral nodeNext ]
                                        |> f graph

                                Nothing ->
                                    Err (ExpectedBlankNode node)

                        _ ->
                            Err (TooManyNodes nodes)
                )
        )


{-| TODO
-}
subject : Decoder Rdf.BlankNodeOrIriOrAnyLiteral
subject =
    Decoder
        (\_ ->
            Result.andThen
                (\nodes ->
                    case nodes of
                        [ node ] ->
                            Ok node

                        _ ->
                            Err (TooManyNodes nodes)
                )
        )


{-| Decode a [Blank
Node](https://www.w3.org/TR/rdf11-concepts/#section-blank-nodes) or
[IRI](https://www.w3.org/TR/rdf11-concepts/#section-IRIs).

    import Rdf
    import Rdf.Graph as Rdf exposing (Graph)
    import Rdf.Namespaces exposing (a)

    graph : Graph
    graph =
        """
        @base <http://example.org/> .
        <alice> <#knows> <bob> .
        """
            |> Rdf.parse
            |> Result.withDefault Rdf.emptyGraph

    decode
        (from
            (Rdf.iri "http://example.org/alice")
            (predicate (Rdf.iri "http://example.org/#knows") blankNodeOrIri)
        )
        graph
    --> Ok (Rdf.asBlankNodeOrIri (Rdf.iri "http://example.org/bob"))

-}
blankNodeOrIri : Decoder BlankNodeOrIri
blankNodeOrIri =
    oneOf
        [ map Rdf.asBlankNodeOrIri iri
        , map Rdf.asBlankNodeOrIri
            (blankNode
                (subject
                    |> andThen
                        (\node ->
                            Maybe.unwrap
                                (error (ExpectedBlankNode node))
                                succeed
                                (Rdf.toBlankNode node)
                        )
                )
            )
        ]


{-| TODO Add documentation
-}
blankNodeOrIriOrAnyLiteral : Decoder BlankNodeOrIriOrAnyLiteral
blankNodeOrIriOrAnyLiteral =
    oneOf
        [ map Rdf.asBlankNodeOrIriOrAnyLiteral iri
        , map Rdf.asBlankNodeOrIriOrAnyLiteral
            (blankNode
                (subject
                    |> andThen
                        (\node ->
                            Maybe.unwrap
                                (error (ExpectedBlankNode node))
                                succeed
                                (Rdf.toBlankNode node)
                        )
                )
            )
        , map Rdf.asBlankNodeOrIriOrAnyLiteral anyLiteral
        ]


{-| Decode a Literal of type `xsd:boolean` into a `Bool`.

    import Rdf
    import Rdf.Graph as Rdf exposing (Graph)
    import Rdf.Namespaces exposing (a)

    graph : Graph
    graph =
        """
        @base <http://example.org/> .
        <alice> <#isAdmin> true .
        """
            |> Rdf.parse
            |> Result.withDefault Rdf.emptyGraph

    decode
        (from
            (Rdf.iri "http://example.org/alice")
            (predicate (Rdf.iri "http://example.org/#isAdmin") bool)
        )
        graph
    --> Ok True

-}
bool : Decoder Bool
bool =
    literal (Rdf.xsd "boolean")
        |> andThen
            (\stringLiteral ->
                case stringLiteral of
                    "true" ->
                        succeed True

                    "false" ->
                        succeed False

                    _ ->
                        error (ExpectedBool stringLiteral)
            )


{-| Decode a Literal of type `xsd:integer` into a `Int`.

    import Rdf
    import Rdf.Graph as Rdf exposing (Graph)
    import Rdf.Namespaces exposing (a)

    graph : Graph
    graph =
        """
        @base <http://example.org/> .
        <question> <#hasAnswer> 42 .
        """
            |> Rdf.parse
            |> Result.withDefault Rdf.emptyGraph

    decode
        (from
            (Rdf.iri "http://example.org/question")
            (predicate (Rdf.iri "http://example.org/#hasAnswer") int)
        )
        graph
    --> Ok 42

-}
int : Decoder Int
int =
    oneOf
        [ literal (Rdf.xsd "integer")
        , literal (Rdf.xsd "int")
        ]
        |> andThen
            (\intLiteral ->
                case String.toInt intLiteral of
                    Nothing ->
                        error (ExpectedInt intLiteral)

                    Just intValue ->
                        succeed intValue
            )


{-| Decode a Literal of type `xsd:double` into a `Float`.

    import Rdf
    import Rdf.Graph as Rdf exposing (Graph)
    import Rdf.Namespaces exposing (a)

    graph : Graph
    graph =
        """
        @base <http://example.org/> .
        <question> <#hasAnswer> 314E-2 .
        """
            |> Rdf.parse
            |> Result.withDefault Rdf.emptyGraph

    decode
        (from
            (Rdf.iri "http://example.org/question")
            (predicate (Rdf.iri "http://example.org/#hasAnswer") float)
        )
        graph
    --> Ok 3.14

-}
float : Decoder Float
float =
    literal (Rdf.xsd "double")
        |> andThen
            (\floatLiteral ->
                case String.toFloat floatLiteral of
                    Nothing ->
                        error (ExpectedFloat floatLiteral)

                    Just floatValue ->
                        succeed floatValue
            )


{-| TODO
-}
number : Decoder Float
number =
    oneOf
        [ map toFloat int
        , float
        ]


{-| Decode a Literal of type `xsd:date` into a `Time.Posix`.

    import Rdf
    import Rdf.Graph as Rdf exposing (Graph)
    import Rdf.Namespaces exposing (a)
    import Time

    graph : Graph
    graph =
        """
        @base <http://example.org/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        <alice> <#birthDate> "1970-01-01"^^xsd:date .
        """
            |> Rdf.parse
            |> Result.withDefault Rdf.emptyGraph

    decode
        (from
            (Rdf.iri "http://example.org/alice")
            (predicate (Rdf.iri "http://example.org/#birthDate") date)
        )
        graph
    --> Ok (Time.millisToPosix 0)

-}
date : Decoder Time.Posix
date =
    Decoder
        (\_ ->
            Result.andThen
                (\nodes ->
                    case nodes of
                        [ node ] ->
                            Rdf.toDate node
                                |> Result.fromMaybe (InvalidDate node)

                        _ ->
                            Err (TooManyNodes nodes)
                )
        )


{-| Decode a Literal of type `xsd:date` into a `Time.Posix`.

    import Rdf
    import Rdf.Graph as Rdf exposing (Graph)
    import Rdf.Namespaces exposing (a)
    import Time

    graph : Graph
    graph =
        """
        @base <http://example.org/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        <alice> <#birthTime> "1970-01-01T00:00:00"^^xsd:dateTime .
        """
            |> Rdf.parse
            |> Result.withDefault Rdf.emptyGraph

    decode
        (from
            (Rdf.iri "http://example.org/alice")
            (predicate (Rdf.iri "http://example.org/#birthTime") dateTime)
        )
        graph
    --> Ok (Time.millisToPosix 0)

-}
dateTime : Decoder Time.Posix
dateTime =
    Decoder
        (\_ ->
            Result.andThen
                (\nodes ->
                    case nodes of
                        [ node ] ->
                            Rdf.toDateTime node
                                |> Result.fromMaybe (InvalidDateTime node)

                        _ ->
                            Err (TooManyNodes nodes)
                )
        )


{-| Decode an [IRI](https://www.w3.org/TR/rdf11-concepts/#section-IRIs).

    import Rdf
    import Rdf.Graph as Rdf exposing (Graph)
    import Rdf.Namespaces exposing (a)

    graph : Graph
    graph =
        """
        @base <http://example.org/> .
        <alice> a <#Person> .
        """
            |> Rdf.parse
            |> Result.withDefault Rdf.emptyGraph

    decode
        (from
            (Rdf.iri "http://example.org/alice")
            (predicate a iri)
        )
        graph
    --> Ok (Rdf.iri "http://example.org/#Person")

-}
iri : Decoder Iri
iri =
    Decoder
        (\_ ->
            Result.andThen
                (\nodes ->
                    case nodes of
                        [ node ] ->
                            case node of
                                Term (BlankNode _) ->
                                    Err (ExpectedIri node)

                                Term ((Iri _) as variant) ->
                                    Ok (Term variant)

                                Term (Literal _) ->
                                    Err (ExpectedIri node)

                        _ ->
                            Err (TooManyNodes nodes)
                )
        )


{-| TODO

    import Rdf
    import Rdf.Graph as Rdf exposing (Graph)
    import Rdf.Namespaces exposing (a)

    graph : Graph
    graph =
        """
        @base <http://example.org/> .
        <alice> <#knows> (
            <bob>
            <cindi>
        ) .
        """
            |> Rdf.parse
            |> Result.withDefault Rdf.emptyGraph

    decode
        (from
            (Rdf.iri "http://example.org/alice")
            (predicate (Rdf.iri "http://example.org/#knows") (list iri))
        )
        graph
    --> Ok [ Rdf.iri "http://example.org/bob", Rdf.iri "http://example.org/cindi" ]

    decode
        (from
            (Rdf.iri "http://example.org/alice")
            (predicate (Rdf.iri "http://example.org/#knows") (many (list iri)))
        )
        graph
    --> Ok [ [ Rdf.iri "http://example.org/bob", Rdf.iri "http://example.org/cindi" ] ]

-}
list : Decoder a -> Decoder (List a)
list (Decoder f) =
    Decoder
        (\graph ->
            Result.andThen
                (\nodes ->
                    case nodes of
                        [ node ] ->
                            Ok node

                        _ ->
                            Err (TooManyNodes nodes)
                )
                >> Result.andThen
                    (\node ->
                        case node of
                            Term ((BlankNode _) as variant) ->
                                Ok (Term variant)

                            Term (Iri _) ->
                                Err (ExpectedBlankNode node)

                            Term (Literal _) ->
                                Err (ExpectedBlankNode node)
                    )
                >> Result.andThen
                    (\focusNode ->
                        focusNode
                            |> objectToList graph
                            |> Maybe.withDefault []
                            |> List.map
                                (\nodeChild ->
                                    Ok [ nodeChild ]
                                        |> f graph
                                )
                            |> Result.combine
                    )
        )


{-| TODO
-}
nonEmpty : Decoder a -> Decoder (NonEmpty a)
nonEmpty =
    list
        >> andThen
            (NonEmpty.fromList
                >> Maybe.unwrap (error UnexpectedEmptyList) succeed
            )


{-| Decode a Literal of type `xsd:string` into a `String`.

    import Rdf
    import Rdf.Graph as Rdf exposing (Graph)
    import Rdf.Namespaces exposing (a)

    graph : Graph
    graph =
        """
        @base <http://example.org/> .
        <alice> <#name> "Alice Wonderland" .
        """
            |> Rdf.parse
            |> Result.withDefault Rdf.emptyGraph

    decode
        (from
            (Rdf.iri "http://example.org/alice")
            (predicate (Rdf.iri "http://example.org/#name") string)
        )
        graph
    --> Ok "Alice Wonderland"

-}
string : Decoder String
string =
    literal (Rdf.xsd "string")


{-| TODO
-}
langString : Decoder ( String, String )
langString =
    literalData (Rdf.rdf "langString")
        |> andThen
            (\({ value, languageTag } as node) ->
                Maybe.unwrap (error (MissingLangString (Term (Literal node))))
                    (succeed << flip Tuple.pair value)
                    languageTag
            )


{-| TODO
-}
stringOrLangString : Decoder Rdf.StringOrLangString
stringOrLangString =
    many
        (oneOf
            [ map Err langString
            , map Ok string
            ]
        )
        |> andThen
            (\stringsOrLangStrings ->
                case Result.partition stringsOrLangStrings of
                    ( [], langStrings ) ->
                        succeed (Rdf.stringOrLangStringFrom Nothing langStrings)

                    ( [ string_ ], langStrings ) ->
                        succeed (Rdf.stringOrLangStringFrom (Just string_) langStrings)

                    ( strings, _ ) ->
                        error (TooManyStrings strings)
            )


{-| TODO
-}
literal : Iri -> Decoder String
literal datatype =
    map .value (literalData datatype)


literalData : Iri -> Decoder DataLiteral
literalData datatype =
    Decoder
        (\_ ->
            Result.andThen
                (\nodes ->
                    case nodes of
                        [ node ] ->
                            case node of
                                Term (BlankNode _) ->
                                    Err (ExpectedAnyLiteral node)

                                Term (Iri _) ->
                                    Err (ExpectedAnyLiteral node)

                                Term (Literal literalData_) ->
                                    if literalData_.datatype /= Rdf.toUrl datatype then
                                        Err (ExpectedLiteralDatatype datatype (Rdf.iri literalData_.datatype))

                                    else
                                        Ok literalData_

                        _ ->
                            Err (TooManyNodes nodes)
                )
        )


{-| TODO
-}
anyLiteral : Decoder Rdf.AnyLiteral
anyLiteral =
    Decoder
        (\_ ->
            Result.andThen
                (\nodes ->
                    case nodes of
                        [ node ] ->
                            case node of
                                Term (BlankNode _) ->
                                    Err (ExpectedAnyLiteral node)

                                Term (Iri _) ->
                                    Err (ExpectedAnyLiteral node)

                                Term ((Literal _) as variant) ->
                                    Ok (Term variant)

                        _ ->
                            Err (TooManyNodes nodes)
                )
        )


{-| TODO
-}
property : PropertyPath -> Decoder a -> Decoder a
property path (Decoder f) =
    Decoder
        (\graph ->
            Result.andThen
                (\nodes ->
                    Result.combine
                        (List.map
                            (\node ->
                                case node of
                                    Term ((BlankNode _) as variant) ->
                                        Ok (Term variant)

                                    Term ((Iri _) as variant) ->
                                        Ok (Term variant)

                                    Term (Literal _) ->
                                        Err (ExpectedBlankNodeOrIri node)
                            )
                            nodes
                        )
                )
                >> Result.andThen
                    (\focusNodes ->
                        let
                            nodeChilds : Result Error (List Rdf.BlankNodeOrIriOrAnyLiteral)
                            nodeChilds =
                                Result.map List.concat <|
                                    Result.combine
                                        (List.map
                                            (\focusNode ->
                                                case getObjectsAt focusNode path graph of
                                                    [] ->
                                                        Err (UnknownProperty focusNode path)

                                                    nodeChilds_ ->
                                                        Ok nodeChilds_
                                            )
                                            focusNodes
                                        )
                        in
                        nodeChilds
                            |> f graph
                    )
        )


{-| TODO
-}
noProperty : PropertyPath -> Decoder ()
noProperty path =
    Decoder
        (\graph ->
            Result.andThen
                (\nodes ->
                    Result.combine
                        (List.map
                            (\node ->
                                case node of
                                    Term ((BlankNode _) as variant) ->
                                        Ok (Term variant)

                                    Term ((Iri _) as variant) ->
                                        Ok (Term variant)

                                    Term (Literal _) ->
                                        Err (ExpectedAnyLiteral node)
                            )
                            nodes
                        )
                )
                >> Result.andThen
                    (\focusNodes ->
                        Result.map (\_ -> ()) <|
                            Result.combine
                                (List.map
                                    (\focusNode ->
                                        case getObjectsAt focusNode path graph of
                                            [] ->
                                                Ok ()

                                            _ ->
                                                Err (PropertyPresent focusNode path)
                                    )
                                    focusNodes
                                )
                    )
        )


{-| TODO
-}
predicate : IsIri compatible -> Decoder a -> Decoder a
predicate =
    property << Rdf.PredicatePath << Rdf.asIri


{-| TODO
-}
anyPredicate : Decoder a -> Decoder a
anyPredicate (Decoder f) =
    Decoder
        (\graph ->
            Result.andThen
                (\nodes ->
                    Result.combine
                        (List.map
                            (\node ->
                                case node of
                                    Term ((BlankNode _) as variant) ->
                                        Ok (Term variant)

                                    Term ((Iri _) as variant) ->
                                        Ok (Term variant)

                                    Term (Literal _) ->
                                        Err (ExpectedBlankNodeOrIri node)
                            )
                            nodes
                        )
                )
                >> Result.andThen
                    (\focusNodes ->
                        let
                            nodeChilds : Result Error (List Rdf.BlankNodeOrIriOrAnyLiteral)
                            nodeChilds =
                                Result.map List.concat <|
                                    Result.combine
                                        (List.map
                                            (\focusNode ->
                                                case getObjects focusNode graph of
                                                    [] ->
                                                        Err (NoProperty focusNode)

                                                    nodeChilds_ ->
                                                        Ok nodeChilds_
                                            )
                                            focusNodes
                                        )
                        in
                        nodeChilds
                            |> f graph
                    )
        )


{-| TODO
-}
predicates : Decoder (List Iri)
predicates =
    Decoder
        (\graph ->
            Result.andThen
                (\nodes ->
                    Result.combine
                        (List.map
                            (\node ->
                                case node of
                                    Term ((BlankNode _) as variant) ->
                                        Ok (Term variant)

                                    Term ((Iri _) as variant) ->
                                        Ok (Term variant)

                                    Term (Literal _) ->
                                        Err (ExpectedBlankNodeOrIri node)
                            )
                            nodes
                        )
                )
                >> Result.andThen
                    (\focusNodes ->
                        Result.map List.concat <|
                            Result.combine
                                (List.map
                                    (\focusNode ->
                                        case getPredicates focusNode graph of
                                            [] ->
                                                Err (NoProperty focusNode)

                                            nodeChilds_ ->
                                                Ok nodeChilds_
                                    )
                                    focusNodes
                                )
                    )
        )


{-| A decoder which always succeeds with the given value.
-}
succeed : a -> Decoder a
succeed x =
    Decoder (\_ _ -> Ok x)


{-| A decoder which always fails with the given message.
-}
fail : String -> Decoder a
fail msg =
    failWith (always msg)


{-| A decoder which always fails with the given message.
-}
failWith : (List BlankNodeOrIriOrAnyLiteral -> String) -> Decoder a
failWith toMsg =
    Decoder
        (\_ ->
            Result.andThen
                (\nodes ->
                    Err (CustomError (toMsg nodes))
                )
        )


{-| TODO
-}
map : (a -> b) -> Decoder a -> Decoder b
map f (Decoder g) =
    Decoder (\graph -> Result.map f << g graph)


{-| TODO
-}
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen f (Decoder g) =
    Decoder
        (\graph x ->
            case g graph x of
                Ok y ->
                    let
                        (Decoder h) =
                            f y
                    in
                    h graph x

                Err e ->
                    Err e
        )


{-| TODO
-}
map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 f g h =
    apply (map f g) h


apply : Decoder (a -> b) -> Decoder a -> Decoder b
apply (Decoder f) (Decoder g) =
    Decoder (\graph x -> Result.map2 (<|) (f graph x) (g graph x))


error : Error -> Decoder a
error e =
    Decoder (\_ _ -> Err e)


{-| TODO
-}
oneOf : List (Decoder a) -> Decoder a
oneOf fs =
    Decoder
        (\graph node ->
            case
                List.foldl
                    (\(Decoder f) a ->
                        case a of
                            Ok x ->
                                Ok x

                            Err es ->
                                case f graph node of
                                    Ok x ->
                                        Ok x

                                    Err e ->
                                        Err (e :: es)
                    )
                    (Err [])
                    fs
            of
                Ok x ->
                    Ok x

                Err es ->
                    Err (Batch (List.reverse es))
        )


{-| TODO

    import Rdf
    import Rdf.Graph as Rdf exposing (Graph)
    import Rdf.Namespaces exposing (a)
    import Time

    graph : Graph
    graph =
        """
        @base <http://example.org/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        <alice>
            <#name> "Alice Wonderland" ;
            <#birthDate> "1970-01-01"^^xsd:date .
        """
            |> Rdf.parse
            |> Result.withDefault Rdf.emptyGraph

    type alias Person =
        { name : String
        , birthDate : Time.Posix
        }

    decode
        (from
            (Rdf.iri "http://example.org/alice")
            (succeed Person
                |> required (Rdf.iri "http://example.org/#name") string
                |> required (Rdf.iri "http://example.org/#birthDate") date
            )
        )
        graph
    --> Ok
    -->     { name = "Alice Wonderland"
    -->     , birthDate = Time.millisToPosix 0
    -->     }

-}
required : IsIri compatible -> Decoder a -> Decoder (a -> b) -> Decoder b
required iriPredicate decoderA decoderF =
    map2 (\f a -> f a) decoderF (predicate iriPredicate decoderA)


{-| TODO

    import Rdf
    import Rdf.Graph as Rdf exposing (Graph)
    import Rdf.Namespaces exposing (a)
    import Rdf.PropertyPath exposing (PropertyPath(..))
    import Time

    graph : Graph
    graph =
        """
        @base <http://example.org/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        <alice> <#knows> [ <#name> "Bob Builder" ] .
        """
            |> Rdf.parse
            |> Result.withDefault Rdf.emptyGraph

    type alias Person =
        { name : String
        }

    decode
        (from
            (Rdf.iri "http://example.org/alice")
            (succeed Person
                |> requiredAt
                    (SequencePath
                        (PredicatePath (Rdf.iri "http://example.org/#knows"))
                        [ PredicatePath (Rdf.iri "http://example.org/#name") ]
                    )
                    string
            )
        )
        graph
    --> Ok
    -->     { name = "Bob Builder"
    -->     }

-}
requiredAt : PropertyPath -> Decoder a -> Decoder (a -> b) -> Decoder b
requiredAt path decoderA decoderF =
    map2 (\f a -> f a) decoderF (property path decoderA)


{-| TODO

    import Rdf
    import Rdf.Graph as Rdf exposing (Graph)
    import Rdf.Namespaces exposing (a)
    import Time

    graph : Graph
    graph =
        """
        @base <http://example.org/> .
        <alice> <#name> "Alice Wonderland" .
        """
            |> Rdf.parse
            |> Result.withDefault Rdf.emptyGraph

    type alias Person =
        { name : String
        , birthDate : Maybe Time.Posix
        }

    decode
        (from
            (Rdf.iri "http://example.org/alice")
            (succeed Person
                |> required (Rdf.iri "http://example.org/#name") string
                |> optional (Rdf.iri "http://example.org/#birthDate") (map Just date) Nothing
            )
        )
        graph
    --> Ok
    -->     { name = "Alice Wonderland"
    -->     , birthDate = Nothing
    -->     }

-}
optional : IsIri compatible -> Decoder a -> a -> Decoder (a -> b) -> Decoder b
optional iriPredicate decoderA defaultA decoderF =
    map2 (\f a -> f a)
        decoderF
        (oneOf
            [ predicate iriPredicate decoderA
            , succeed defaultA
            ]
        )


{-| TODO

    import Rdf
    import Rdf.Graph as Rdf exposing (Graph)
    import Rdf.Namespaces exposing (a)
    import Rdf.PropertyPath exposing (PropertyPath(..))
    import Time

    graph : Graph
    graph =
        """
        @base <http://example.org/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        <alice> <#name> "Alice Wonderland" .
        """
            |> Rdf.parse
            |> Result.withDefault Rdf.emptyGraph

    type alias Person =
        { name : String
        }

    decode
        (from
            (Rdf.iri "http://example.org/alice")
            (succeed Person
                |> optionalAt
                    (SequencePath
                        (PredicatePath (Rdf.iri "http://example.org/#knows"))
                        [ PredicatePath (Rdf.iri "http://example.org/#name") ]
                    )
                    string
                    "Unknown"
            )
        )
        graph
    --> Ok
    -->     { name = "Unknown"
    -->     }

-}
optionalAt : PropertyPath -> Decoder a -> a -> Decoder (a -> b) -> Decoder b
optionalAt path decoderA defaultA decoderF =
    map2 (\f a -> f a)
        decoderF
        (oneOf
            [ property path decoderA
            , succeed defaultA
            ]
        )


{-| TODO

    import Rdf
    import Rdf.Graph as Rdf exposing (Graph)
    import Rdf.Namespaces exposing (a)

    graph : Graph
    graph =
        """
        @base <http://example.org/> .
        <alice> <#name> "Alice Wonderland" .
        """
            |> Rdf.parse
            |> Result.withDefault Rdf.emptyGraph

    type alias Person =
        { name : String
        , admin : Bool
        }

    decode
        (from
            (Rdf.iri "http://example.org/alice")
            (succeed Person
                |> required (Rdf.iri "http://example.org/#name") string
                |> hardcoded True
            )
        )
        graph
    --> Ok
    -->     { name = "Alice Wonderland"
    -->     , admin = True
    -->     }

-}
hardcoded : a -> Decoder (a -> b) -> Decoder b
hardcoded a decoderF =
    map (\f -> f a) decoderF


{-| TODO Add documentation
-}
custom : Decoder a -> Decoder (a -> b) -> Decoder b
custom decoderA decoderF =
    map2 (\f a -> f a) decoderF decoderA


{-| TODO
-}
lazy : (() -> Decoder a) -> Decoder a
lazy f =
    andThen f (succeed ())


{-| TODO Add documentation
-}
combine : List (Decoder a) -> Decoder (List a)
combine =
    List.foldr (map2 (::)) (succeed [])



--  QUERY


hasTripleWithSubject : IsBlankNodeOrIri compatible -> Graph -> Bool
hasTripleWithSubject nodeFocusCompatible (Graph data) =
    let
        subjectMatches : Rdf.Triple -> Bool
        subjectMatches triple =
            triple.subject == nodeFocus

        nodeFocus : Rdf.BlankNodeOrIri
        nodeFocus =
            Rdf.asBlankNodeOrIri nodeFocusCompatible
    in
    data.triples
        |> List.filter subjectMatches
        |> List.isEmpty
        |> not


hasTripleWithObject : IsBlankNodeOrIriOrAnyLiteral compatible -> Graph -> Bool
hasTripleWithObject nodeFocusCompatible (Graph data) =
    let
        objectMatches : Rdf.Triple -> Bool
        objectMatches triple =
            triple.object == nodeFocus

        nodeFocus : Rdf.BlankNodeOrIriOrAnyLiteral
        nodeFocus =
            Rdf.asBlankNodeOrIriOrAnyLiteral nodeFocusCompatible
    in
    data.triples
        |> List.filter objectMatches
        |> List.isEmpty
        |> not


getSubjects : Graph -> List BlankNodeOrIri
getSubjects (Graph data) =
    data.triples
        |> List.map .subject
        -- FIXME Make sure, subjects are already unique when stored.
        |> List.unique


getObjectsAt : IsBlankNodeOrIri compatible -> PropertyPath -> Graph -> List BlankNodeOrIriOrAnyLiteral
getObjectsAt nodeFocus path (Graph data) =
    nodeFocus
        |> followPropertyPath data path
        |> List.unique


type alias GraphData rest =
    { rest
        | bySubjectByPredicate : Dict String (Dict String (List Rdf.Triple))
        , byPredicateBySubject : Dict String (Dict String (List Rdf.Triple))
    }


followPropertyPath :
    GraphData rest
    -> PropertyPath
    -> IsBlankNodeOrIri compatible
    -> List BlankNodeOrIriOrAnyLiteral
followPropertyPath data propertyPath nodeFocus =
    case propertyPath of
        Rdf.PredicatePath iriPredicate ->
            data.bySubjectByPredicate
                |> Dict.get (Rdf.serializeNode nodeFocus)
                |> Maybe.withDefault Dict.empty
                |> Dict.get (Rdf.serializeNode iriPredicate)
                |> Maybe.withDefault []
                |> List.map .object

        Rdf.InversePath (Rdf.PredicatePath iriPredicate) ->
            data.byPredicateBySubject
                |> Dict.get (Rdf.serializeNode iriPredicate)
                |> Maybe.withDefault Dict.empty
                |> Dict.values
                |> List.concatMap
                    (\triples ->
                        let
                            objectRaw : String
                            objectRaw =
                                Rdf.serializeNode nodeFocus
                        in
                        List.filterMap
                            (\triple ->
                                if Rdf.serializeNode triple.object == objectRaw then
                                    Just triple.subject

                                else
                                    Nothing
                            )
                            triples
                    )
                |> List.map Rdf.asBlankNodeOrIriOrAnyLiteral

        Rdf.SequencePath first rest ->
            let
                nodesAtFirst : List Rdf.BlankNodeOrIriOrAnyLiteral
                nodesAtFirst =
                    followPropertyPath data first nodeFocus

                step :
                    Rdf.PropertyPath
                    -> List Rdf.BlankNodeOrIriOrAnyLiteral
                    -> List Rdf.BlankNodeOrIriOrAnyLiteral
                step next nodes =
                    nodes
                        |> List.filterMap Rdf.toBlankNodeOrIri
                        |> List.map asBlankNodeOrIriCompatible
                        |> List.concatMap (followPropertyPath data next)

                asBlankNodeOrIriCompatible : Rdf.BlankNodeOrIri -> Rdf.IsBlankNodeOrIri compatible
                asBlankNodeOrIriCompatible (Term node) =
                    Term node
            in
            rest
                |> List.foldl step nodesAtFirst

        _ ->
            []


getObjects : IsBlankNodeOrIri compatible -> Graph -> List BlankNodeOrIriOrAnyLiteral
getObjects nodeFocus (Graph data) =
    data.bySubjectByPredicate
        |> Dict.get (Rdf.serializeNode nodeFocus)
        |> Maybe.withDefault Dict.empty
        |> Dict.values
        |> List.concat
        |> List.map .object
        -- FIXME Make sure, objects are already unique when stored.
        |> List.unique


getPredicates : IsBlankNodeOrIri compatible -> Graph -> List Iri
getPredicates nodeFocus (Graph data) =
    let
        toIri key =
            key
                |> String.dropLeft 1
                |> String.dropRight 1
                |> Iri
                |> Term
    in
    data.bySubjectByPredicate
        |> Dict.get (Rdf.serializeNode nodeFocus)
        |> Maybe.withDefault Dict.empty
        |> Dict.keys
        |> List.map toIri


objectToList : Graph -> BlankNodeOrIri -> Maybe (List BlankNodeOrIriOrAnyLiteral)
objectToList graph nodeFocus =
    if Rdf.toIri nodeFocus == Just rdfNil then
        Just []

    else
        Maybe.map2
            (\first rest ->
                first :: rest
            )
            (getFirst nodeFocus graph)
            (getRest nodeFocus graph
                |> Maybe.andThen (objectToList graph)
            )


getFirst : IsBlankNodeOrIri compatible -> Graph -> Maybe BlankNodeOrIriOrAnyLiteral
getFirst nodeFocus (Graph data) =
    case
        data.bySubjectByPredicate
            |> Dict.get (Rdf.serializeNode nodeFocus)
            |> Maybe.withDefault Dict.empty
            |> Dict.get (Rdf.serializeNode rdfFirst)
            |> Maybe.withDefault []
            |> List.map .object
            |> List.unique
    of
        [ value ] ->
            Just value

        _ ->
            Nothing


getRest : IsBlankNodeOrIri compatible -> Graph -> Maybe BlankNodeOrIri
getRest nodeFocus (Graph data) =
    case
        data.bySubjectByPredicate
            |> Dict.get (Rdf.serializeNode nodeFocus)
            |> Maybe.withDefault Dict.empty
            |> Dict.get (Rdf.serializeNode rdfRest)
            |> Maybe.withDefault []
            |> List.map .object
            |> List.filterMap Rdf.toBlankNodeOrIri
            |> List.unique
    of
        [ value ] ->
            Just value

        _ ->
            Nothing


rdfNil : Iri
rdfNil =
    rdf "nil"


rdfFirst : Iri
rdfFirst =
    rdf "first"


rdfRest : Iri
rdfRest =
    rdf "rest"
