module Rdf.Decode exposing
    ( Decoder
    , iri
    , blankNode
    , literal
    , blankNodeOrIri, blankNodeOrIriOrLiteral
    , string, langString, stringOrLangString
    , bool
    , decimal, int, float, number
    , date, dateTime
    , from, fromSubject
    , at
    , many, indexedMany
    , oneOf
    , list, nonEmpty
    , property
    , anyPredicate
    , noProperty
    , predicates
    , decode
    , Error, Problem(..), errorToString
    , inContext
    , succeed, fail, failWith
    , map, map2
    , andMap
    , andThen
    , combine
    , lazy
    , required
    , optional
    , hardcoded
    , custom
    )

{-| Extract Elm values from `Rdf.Graph`'s. This works similar to
[Json.Decode](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode)
with a few differences:

  - You have to specify the node at which decoding starts. The reason is, that
    there is no obvious choice within a graph, like it is for a JSON object where
    decoding always starts at the root.

  - RDF allows multiple objects for one predicate, unlike JSON where a field is
    assumed to appear once. Therefore one has to use [`many`](#many) to switch
    from decoding **exactly one** value to allowing **arbitrarily many** values
    at a certain predicate.


# Basic Decoders

@docs Decoder

@docs iri
@docs blankNode
@docs literal

@docs blankNodeOrIri, blankNodeOrIriOrLiteral


## Specific literals

@docs string, langString, stringOrLangString
@docs bool
@docs decimal, int, float, number
@docs date, dateTime


## Focus nodes

@docs from, fromSubject
@docs at


## Multiplicities

@docs many, indexedMany


## Inconsistent structure

@docs oneOf


## Containers

@docs list, nonEmpty


## Properties

@docs property
@docs anyPredicate
@docs noProperty
@docs predicates


# Running Decoders

@docs decode
@docs Error, Problem, errorToString
@docs inContext


# Transforming and chaining

@docs succeed, fail, failWith
@docs map, map2
@docs andMap
@docs andThen
@docs combine
@docs lazy


## Pipelines

@docs required
@docs optional
@docs hardcoded
@docs custom

-}

import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Internal.Graph exposing (Graph(..))
import Internal.Term exposing (Term(..), Variant(..))
import List.Extra as List
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Maybe.Extra as Maybe
import Rdf
    exposing
        ( BlankNodeOrIri
        , BlankNodeOrIriOrLiteral
        , Iri
        , IsBlankNodeOrIri
        , IsBlankNodeOrIriOrLiteral
        , IsPath
        , Literal
        , Path
        , StringOrLangString
        , Triple
        )
import Rdf.Namespaces exposing (rdf)
import Result.Extra as Result
import Time



-- BASIC DECODERS


{-| A way to specify what kind of thing you want to decode into.
-}
type Decoder a
    = Decoder
        (State
         -> Result Error (List BlankNodeOrIriOrLiteral)
         -> Result Error a
        )


type alias State =
    { graph : Graph
    , context : List String
    }


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
            (property a iri)
        )
        graph
    --> Ok (Rdf.iri "http://example.org/#Person")

-}
iri : Decoder Iri
iri =
    Decoder
        (\state ->
            Result.andThen
                (\nodes ->
                    case nodes of
                        [ node ] ->
                            case node of
                                Term ((Iri _) as variant) ->
                                    Ok (Term variant)

                                Term _ ->
                                    Err
                                        { error = ExpectedIri node
                                        , contextStack = state.context
                                        }

                        _ ->
                            Err
                                { error = ExpectedOneNode nodes
                                , contextStack = state.context
                                }
                )
        )


{-| Decode a [blank
node](https://www.w3.org/TR/rdf11-concepts/#section-blank-nodes).
-}
blankNode : Decoder a -> Decoder a
blankNode (Decoder f) =
    Decoder
        (\state ->
            Result.andThen
                (\nodes ->
                    case nodes of
                        [ node ] ->
                            case Rdf.toBlankNode node of
                                Just nodeNext ->
                                    Ok [ Rdf.asBlankNodeOrIriOrLiteral nodeNext ]
                                        |> f state

                                Nothing ->
                                    Err
                                        { error = ExpectedBlankNode node
                                        , contextStack = state.context
                                        }

                        _ ->
                            Err
                                { error = ExpectedOneNode nodes
                                , contextStack = state.context
                                }
                )
        )


{-| Decode a literal of any datatype.
-}
literal : Decoder Literal
literal =
    Decoder
        (\state ->
            Result.andThen
                (\nodes ->
                    case nodes of
                        [ node ] ->
                            case node of
                                Term ((Literal _) as variant) ->
                                    Ok (Term variant)

                                Term _ ->
                                    Err
                                        { error = ExpectedLiteral node
                                        , contextStack = state.context
                                        }

                        _ ->
                            Err
                                { error = ExpectedOneNode nodes
                                , contextStack = state.context
                                }
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
            (property (Rdf.iri "http://example.org/#knows") blankNodeOrIri)
        )
        graph
    --> Ok (Rdf.asBlankNodeOrIri (Rdf.iri "http://example.org/bob"))

-}
blankNodeOrIri : Decoder BlankNodeOrIri
blankNodeOrIri =
    Decoder
        (\state ->
            Result.andThen
                (\nodes ->
                    case nodes of
                        [ node ] ->
                            case node of
                                Term ((Iri _) as variant) ->
                                    Ok (Term variant)

                                Term ((BlankNode _) as variant) ->
                                    Ok (Term variant)

                                _ ->
                                    Err
                                        { error = ExpectedBlankNodeOrIri node
                                        , contextStack = state.context
                                        }

                        _ ->
                            Err
                                { error = ExpectedOneNode nodes
                                , contextStack = state.context
                                }
                )
        )


{-| Decode a [blank
node](https://www.w3.org/TR/rdf11-concepts/#section-blank-nodes), an
[IRI](https://www.w3.org/TR/rdf11-concepts/#section-IRIs), or
a [literal](https://www.w3.org/TR/rdf11-concepts/#section-literal).
-}
blankNodeOrIriOrLiteral : Decoder BlankNodeOrIriOrLiteral
blankNodeOrIriOrLiteral =
    Decoder
        (\state ->
            Result.andThen
                (\nodes ->
                    case nodes of
                        [ node ] ->
                            Ok node

                        _ ->
                            Err
                                { error = ExpectedOneNode nodes
                                , contextStack = state.context
                                }
                )
        )



-- SPECIFIC LITERALS


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
            (property (Rdf.iri "http://example.org/#name") string)
        )
        graph
    --> Ok "Alice Wonderland"

-}
string : Decoder String
string =
    andThen
        (\lit ->
            case Rdf.toString lit of
                Nothing ->
                    err (ExpectedLiteralOf [ iriXsdString ] lit)

                Just val ->
                    succeed val
        )
        literal


{-| Decode a literal of type `rdf:langString` into a `( String, String )` where
the first element is the language tag and the second the String value.
-}
langString : Decoder ( String, String )
langString =
    andThen
        (\lit ->
            case Rdf.toLangString lit of
                Nothing ->
                    err (ExpectedLiteralOf [ iriRdfLangString ] lit)

                Just val ->
                    succeed val
        )
        literal


{-| Decode a `StringOrLangString`.
-}
stringOrLangString : Decoder StringOrLangString
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
                        err (TooManyStrings strings)
            )


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
            (property (Rdf.iri "http://example.org/#isAdmin") bool)
        )
        graph
    --> Ok True

-}
bool : Decoder Bool
bool =
    andThen
        (\lit ->
            case Rdf.toBool lit of
                Nothing ->
                    err (ExpectedLiteralOf [ iriXsdBoolean ] lit)

                Just val ->
                    succeed val
        )
        literal


{-| Decode a literal of type `xsd:decimal` into a `Decimal`.
-}
decimal : Decoder Decimal
decimal =
    andThen
        (\lit ->
            case Rdf.toDecimal lit of
                Nothing ->
                    err
                        (ExpectedLiteralOf
                            [ iriXsdDecimal
                            ]
                            lit
                        )

                Just val ->
                    succeed val
        )
        literal


{-| Decode a Literal of type `xsd:integer` or `xsd:int` into a `Int`.

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
            (property (Rdf.iri "http://example.org/#hasAnswer") int)
        )
        graph
    --> Ok 42

-}
int : Decoder Int
int =
    andThen
        (\lit ->
            case Rdf.toInt lit of
                Nothing ->
                    err
                        (ExpectedLiteralOf
                            [ iriXsdInteger
                            , iriXsdInt
                            ]
                            lit
                        )

                Just val ->
                    succeed val
        )
        literal


{-| Decode a Literal of type `xsd:double` or `xsd:float` into a `Float`.

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
            (property (Rdf.iri "http://example.org/#hasAnswer") float)
        )
        graph
    --> Ok 3.14

-}
float : Decoder Float
float =
    andThen
        (\lit ->
            case Rdf.toFloat lit of
                Nothing ->
                    err
                        (ExpectedLiteralOf
                            [ iriXsdDouble
                            , iriXsdFloat
                            ]
                            lit
                        )

                Just val ->
                    succeed val
        )
        literal


{-| Decode a Literal of type `xsd:integer`, `xsd:int`, `xsd:double`, or
`xsd:float` into a `Float`.
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
            (property (Rdf.iri "http://example.org/#birthDate") date)
        )
        graph
    --> Ok (Time.millisToPosix 0)

-}
date : Decoder Time.Posix
date =
    andThen
        (\lit ->
            case Rdf.toDate lit of
                Nothing ->
                    err (ExpectedLiteralOf [ iriXsdDate ] lit)

                Just val ->
                    succeed val
        )
        literal


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
            (property (Rdf.iri "http://example.org/#birthTime") dateTime)
        )
        graph
    --> Ok (Time.millisToPosix 0)

-}
dateTime : Decoder Time.Posix
dateTime =
    andThen
        (\lit ->
            case Rdf.toDateTime lit of
                Nothing ->
                    err (ExpectedLiteralOf [ iriXsdDateTime ] lit)

                Just val ->
                    succeed val
        )
        literal



-- FOCUS NODES


{-| Start decoding from the provided (focus) node. You can also nest this
`Decoder` in other `Decoder`'s if you have to change the focus node.
-}
from : IsBlankNodeOrIri compatible -> Decoder a -> Decoder a
from nodeFocusCompatible (Decoder f) =
    let
        nodeFocus : BlankNodeOrIri
        nodeFocus =
            Rdf.asBlankNodeOrIri nodeFocusCompatible
    in
    Decoder
        (\state _ ->
            if
                hasTripleWithSubject nodeFocus state.graph
                    || hasTripleWithObject nodeFocus state.graph
            then
                f state (Ok [ Rdf.asBlankNodeOrIriOrLiteral nodeFocus ])

            else
                Err
                    { error = ExpectedNode (Rdf.asBlankNodeOrIri nodeFocus)
                    , contextStack = state.context
                    }
        )


{-| Start decoding from **all** subjects. If the graph contains more then one
triple, you have to combine this with [`many`](#many) to get a result.
-}
fromSubject : Decoder a -> Decoder a
fromSubject (Decoder f) =
    Decoder
        (\state _ ->
            state.graph
                |> getSubjects
                |> List.map Rdf.asBlankNodeOrIriOrLiteral
                |> Ok
                |> f state
        )


{-| Start decoding at each of the provided (focus) node. If more then one of
these nodes exist in your graph, you have to combine this with
[`many`](#many) to get a result.
-}
at : List BlankNodeOrIriOrLiteral -> Decoder a -> Decoder a
at nodes (Decoder f) =
    Decoder (\graph _ -> f graph (Ok nodes))



-- MULITIPLICITIES


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


{-| This is like [`many`](#many) but you also get the index of the currently
decoded value. Note that the order in which nodes are decoded is
non-deterministic.
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



-- INCONSISTENT STRUCTURE


{-| Try a bunch of different `Decoder`'s. It will succeed with the first one
which succeeded. When all fail, you get the error for all cases.
-}
oneOf : List (Decoder a) -> Decoder a
oneOf fs =
    Decoder
        (\state node ->
            case
                List.foldl
                    (\(Decoder f) a ->
                        case a of
                            Ok x ->
                                Ok x

                            Err es ->
                                case f state node of
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
                    Err
                        { error = ExpectedOneOf (List.reverse es)
                        , contextStack = state.context
                        }
        )



-- CONTAINERS


{-| Decode an [RDF
collections](https://www.w3.org/TR/rdf-schema/#ch_collectionvocab) into
a `List`.

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
            (property (Rdf.iri "http://example.org/#knows") (list iri))
        )
        graph
    --> Ok [ Rdf.iri "http://example.org/bob", Rdf.iri "http://example.org/cindi" ]

    decode
        (from
            (Rdf.iri "http://example.org/alice")
            (property (Rdf.iri "http://example.org/#knows") (many (list iri)))
        )
        graph
    --> Ok [ [ Rdf.iri "http://example.org/bob", Rdf.iri "http://example.org/cindi" ] ]

-}
list : Decoder a -> Decoder (List a)
list (Decoder f) =
    Decoder
        (\state ->
            Result.andThen
                (\nodes ->
                    case nodes of
                        [ node ] ->
                            Ok node

                        _ ->
                            Err
                                { error = ExpectedOneNode nodes
                                , contextStack = state.context
                                }
                )
                >> Result.andThen
                    (\node ->
                        case node of
                            Term ((BlankNode _) as variant) ->
                                Ok (Term variant)

                            Term _ ->
                                Err
                                    { error = ExpectedBlankNode node
                                    , contextStack = state.context
                                    }
                    )
                >> Result.andThen
                    (\focusNode ->
                        focusNode
                            |> objectToList state.graph
                            |> Maybe.withDefault []
                            |> List.map
                                (\nodeChild ->
                                    Ok [ nodeChild ]
                                        |> f state
                                )
                            |> Result.combine
                    )
        )


{-| Like [`list`](#list) but ensure that the resulting `List` is not empty.
-}
nonEmpty : Decoder a -> Decoder (NonEmpty a)
nonEmpty =
    list
        >> andThen
            (NonEmpty.fromList
                >> Maybe.unwrap (err UnexpectedEmptyList) succeed
            )



-- PREDICATES


{-| Run a decoder at the node which is obtained by following the provided
property path. Make sure to use [`many`](#many) if more then one node can be
found, otherwise decoding will fail.
-}
property : IsPath compatible -> Decoder a -> Decoder a
property path (Decoder f) =
    let
        expectBlankNodeOrIri :
            State
            -> BlankNodeOrIriOrLiteral
            -> Result Error BlankNodeOrIri
        expectBlankNodeOrIri state node =
            case node of
                Term ((BlankNode _) as variant) ->
                    Ok (Term variant)

                Term ((Iri _) as variant) ->
                    Ok (Term variant)

                Term _ ->
                    Err
                        { error = ExpectedBlankNodeOrIri node
                        , contextStack = state.context
                        }
    in
    Decoder
        (\state ->
            let
                get :
                    BlankNodeOrIri
                    -> Result Error (List BlankNodeOrIriOrLiteral)
                get focusNode =
                    case getObjectsAt focusNode path state.graph of
                        [] ->
                            Err
                                { error =
                                    ExpectedPath
                                        focusNode
                                        (Rdf.asPath path)
                                , contextStack = state.context
                                }

                        nodeChilds_ ->
                            Ok nodeChilds_
            in
            Result.andThen
                (List.map (expectBlankNodeOrIri state)
                    >> Result.combine
                )
                >> Result.andThen
                    (List.map get
                        >> Result.combine
                        >> Result.map List.concat
                        >> f state
                    )
        )


{-| This decoder succeeds if no nodes can be reached by following the provided
property path.
-}
noProperty : IsPath compatible -> Decoder ()
noProperty path =
    Decoder
        (\state ->
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

                                    Term _ ->
                                        Err
                                            { error = ExpectedLiteral node
                                            , contextStack = state.context
                                            }
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
                                        case getObjectsAt focusNode path state.graph of
                                            [] ->
                                                Ok ()

                                            objects ->
                                                Err
                                                    { error =
                                                        ExpectedNoPath
                                                            focusNode
                                                            (Rdf.asPath path)
                                                            objects
                                                    , contextStack = state.context
                                                    }
                                    )
                                    focusNodes
                                )
                    )
        )


{-| Run a decoder on the nodes which can be reached by any predicate from the
current focus node. Make sure to use [`many`](#many) if more then one node can
be reached in this way.
-}
anyPredicate : Decoder a -> Decoder a
anyPredicate (Decoder f) =
    Decoder
        (\state ->
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

                                    Term _ ->
                                        Err
                                            { error = ExpectedBlankNodeOrIri node
                                            , contextStack = state.context
                                            }
                            )
                            nodes
                        )
                )
                >> Result.andThen
                    (\focusNodes ->
                        let
                            nodeChilds : Result Error (List BlankNodeOrIriOrLiteral)
                            nodeChilds =
                                Result.map List.concat <|
                                    Result.combine
                                        (List.map
                                            (\focusNode ->
                                                case getObjects focusNode state.graph of
                                                    [] ->
                                                        Err
                                                            { error = ExpectedPredicates focusNode
                                                            , contextStack = state.context
                                                            }

                                                    nodeChilds_ ->
                                                        Ok nodeChilds_
                                            )
                                            focusNodes
                                        )
                        in
                        nodeChilds
                            |> f state
                    )
        )


{-| Decode the list of all predicates for which the current focus node is the
subject.
-}
predicates : Decoder (List Iri)
predicates =
    Decoder
        (\state ->
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

                                    Term _ ->
                                        Err
                                            { error = ExpectedBlankNodeOrIri node
                                            , contextStack = state.context
                                            }
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
                                        case getPredicates focusNode state.graph of
                                            [] ->
                                                Err
                                                    { error = ExpectedPredicates focusNode
                                                    , contextStack = state.context
                                                    }

                                            nodeChilds_ ->
                                                Ok nodeChilds_
                                    )
                                    focusNodes
                                )
                    )
        )



-- RUNNING DECODERS


{-| Run a [`Decoder`](#Decoder) on an actual `Graph` starting at the provided
`Node`'s.
-}
decode : Decoder a -> Graph -> Result Error a
decode (Decoder f) graph =
    f
        { graph = graph
        , context = []
        }
        (Err
            { error = NoFocusNode
            , contextStack = []
            }
        )


{-| When the decoding fails, we get on of these. Use
[`errorToString`](#errorToString) to turn this into a human-friendly form.
-}
type alias Error =
    { error : Problem
    , contextStack : List String
    }


{-| The problem which made decoding fail.
-}
type Problem
    = NoFocusNode
    | ExpectedNode BlankNodeOrIri
    | ExpectedIri BlankNodeOrIriOrLiteral
    | ExpectedBlankNode BlankNodeOrIriOrLiteral
    | ExpectedLiteral BlankNodeOrIriOrLiteral
    | ExpectedLiteralOf (List Iri) Literal
    | ExpectedBlankNodeOrIri BlankNodeOrIriOrLiteral
    | ExpectedOneNode (List BlankNodeOrIriOrLiteral)
    | ExpectedPath BlankNodeOrIri Path
    | ExpectedNoPath BlankNodeOrIri Path (List BlankNodeOrIriOrLiteral)
    | ExpectedPredicates BlankNodeOrIri
    | ExpectedOneOf (List Error)
      --
    | UnexpectedEmptyList
    | CustomError String
    | Failure String
    | FailureAt String (List BlankNodeOrIriOrLiteral)
    | TooManyStrings (List String)


{-| Turn a decoding error into a human friendly string.
-}
errorToString : Error -> String
errorToString { error, contextStack } =
    if List.isEmpty contextStack then
        problemToString error

    else
        String.join "\n"
            [ problemToString error
            , ""
            , "Context:"
            , ""
            , indent (String.join "\n" contextStack)
            ]


problemToString : Problem -> String
problemToString problem =
    case problem of
        NoFocusNode ->
            "This decoder did not specify any node to start decoding from."

        ExpectedNode node ->
            "I wanted to start decoding at the node "
                ++ Rdf.serialize node
                ++ " but it is not object or subject in any triple."

        ExpectedLiteral term ->
            "Expected a literal, but got " ++ Rdf.serialize term ++ "."

        ExpectedLiteralOf datatypesExpected lit ->
            String.join "\n"
                [ "Expected a literal of type"
                , indent
                    (String.join ", or\n"
                        (List.map Rdf.serialize datatypesExpected)
                    )
                , "but got " ++ Rdf.serialize lit ++ "."
                ]

        ExpectedBlankNode nodeFound ->
            "Expected a blank node, but found " ++ Rdf.serialize nodeFound ++ "."

        ExpectedIri nodeFound ->
            "Expected an IRI, but found " ++ Rdf.serialize nodeFound ++ "."

        ExpectedBlankNodeOrIri nodeFound ->
            "Expected a blank node or an IRI, but found " ++ Rdf.serialize nodeFound ++ "."

        ExpectedOneNode nodesFound ->
            String.join "\n"
                [ "I expected exactly one node, but I got"
                , indent
                    (String.join "\n"
                        (List.map Rdf.serialize nodesFound)
                    )
                ]

        ExpectedPath nodeFocus path ->
            "Starting at "
                ++ Rdf.serialize nodeFocus
                ++ " I expected nodes along "
                ++ Rdf.serialize path
                ++ "."

        ExpectedNoPath nodeFocus path objects ->
            "I expected that "
                ++ Rdf.serialize nodeFocus
                ++ " is not connected to any other node via "
                ++ Rdf.serialize path
                ++ " but I got"
                ++ indent
                    (String.join "\n"
                        (List.map Rdf.serialize objects)
                    )

        ExpectedPredicates nodeFocus ->
            "I expected that "
                ++ Rdf.serialize nodeFocus
                ++ " is the subject of any triple."

        ExpectedOneOf errors ->
            String.join "\n\n"
                [ "Decoding failed in one of the following ways:"
                , indent
                    (String.join "\n\n"
                        (List.map errorToString errors)
                    )
                ]

        UnexpectedEmptyList ->
            "Expected a non-empty list, but found an empty list."

        CustomError s ->
            s

        Failure msg ->
            msg

        FailureAt msg nodes ->
            String.join "\n"
                [ "Problem with the given value at"
                , ""
                , indent (String.join ",\n" (List.map Rdf.serialize nodes))
                , ""
                , msg
                ]

        TooManyStrings stringsFound ->
            "I expected a single string, but I found multiple strings "
                ++ String.join ", " stringsFound
                ++ "."


indent : String -> String
indent lines =
    lines
        |> String.lines
        |> List.map (\line -> "    " ++ line)
        |> String.join "\n"


{-| You can use this to mark that you are in a certain context. A stack of
these contexts will be provided with the `Problem` when decoding fails,
which you then can use to improve the reported error message.
-}
inContext : String -> Decoder a -> Decoder a
inContext context (Decoder f) =
    Decoder <|
        \state ->
            f { state | context = context :: state.context }



-- TRANSFORMING VALUES


{-| A decoder which always succeeds with the given value.
-}
succeed : a -> Decoder a
succeed x =
    Decoder (\_ _ -> Ok x)


{-| A decoder which always fails with the given message.
-}
fail : String -> Decoder a
fail msg =
    Decoder <|
        \state resultNodes ->
            case resultNodes of
                Err _ ->
                    Err
                        { error = Failure msg
                        , contextStack = state.context
                        }

                Ok nodes ->
                    Err
                        { error = FailureAt msg nodes
                        , contextStack = state.context
                        }


{-| A decoder which always fails with the given message. This is like
[`fail`](#fail), but you also get the list of current focus nodes when creating
the error message.
-}
failWith : (List BlankNodeOrIriOrLiteral -> String) -> Decoder a
failWith toMsg =
    Decoder
        (\state ->
            Result.andThen
                (\nodes ->
                    Err
                        { error = CustomError (toMsg nodes)
                        , contextStack = state.context
                        }
                )
        )


{-| Transform the decoded value by applying the provided function.
-}
map : (a -> b) -> Decoder a -> Decoder b
map f (Decoder g) =
    Decoder (\graph -> Result.map f << g graph)


{-| Like [`map`](#map) but it takes two `Decoder`'s.
-}
map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 f g h =
    apply (map f g) h


{-| A helper for combining `Decoder`'s in a pipeline.
-}
andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap decoderA decoderF =
    apply decoderF decoderA


{-| Run a `Decoder` and then run another one, where you can use the already
decoded value.
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


{-| Turn a list of `Decoder`'s into a `Decoder` of a list. This decoder will
fail whenever on of its elements fails.
-}
combine : List (Decoder a) -> Decoder (List a)
combine =
    List.foldr (map2 (::)) (succeed [])


{-| Use this if you need to decode recursive data structures.
-}
lazy : (() -> Decoder a) -> Decoder a
lazy f =
    andThen f (succeed ())



-- PIPELINES


{-| Decode a required property.

    import Rdf
    import Rdf.Graph as Rdf exposing (Graph)
    import Rdf.Namespaces exposing (a)
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
                |> required
                    (Rdf.sequence
                        (Rdf.iri "http://example.org/#knows")
                        [ Rdf.iri "http://example.org/#name" ]
                    )
                    string
            )
        )
        graph
    --> Ok
    -->     { name = "Bob Builder"
    -->     }

-}
required : IsPath compatible -> Decoder a -> Decoder (a -> b) -> Decoder b
required path decoderA decoderF =
    map2 (\f a -> f a) decoderF (property path decoderA)


{-| Decode an optional property.

    import Rdf
    import Rdf.Graph as Rdf exposing (Graph)
    import Rdf.Namespaces exposing (a)
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
                |> optional
                    (Rdf.sequence
                        (Rdf.iri "http://example.org/#knows")
                        [ Rdf.iri "http://example.org/#name" ]
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
optional : IsPath compatible -> Decoder a -> a -> Decoder (a -> b) -> Decoder b
optional path decoderA defaultA decoderF =
    map2 (\f a -> f a)
        decoderF
        (oneOf
            [ property path decoderA
            , succeed defaultA
            ]
        )


{-| Rather than decoding anything, use a fixed value for the next step in the
pipeline. `harcoded` does not look at the RDF graph at all.

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


{-| Run the given decoder and feed its result into the pipeline at this point.
-}
custom : Decoder a -> Decoder (a -> b) -> Decoder b
custom decoderA decoderF =
    map2 (\f a -> f a) decoderF decoderA



-- HELPER


apply : Decoder (a -> b) -> Decoder a -> Decoder b
apply (Decoder f) (Decoder g) =
    Decoder (\graph x -> Result.map2 (<|) (f graph x) (g graph x))


err : Problem -> Decoder a
err e =
    Decoder
        (\state _ ->
            Err
                { error = e
                , contextStack = state.context
                }
        )


hasTripleWithSubject : IsBlankNodeOrIri compatible -> Graph -> Bool
hasTripleWithSubject nodeFocusCompatible (Graph data) =
    let
        subjectMatches : Triple -> Bool
        subjectMatches triple =
            triple.subject == nodeFocus

        nodeFocus : BlankNodeOrIri
        nodeFocus =
            Rdf.asBlankNodeOrIri nodeFocusCompatible
    in
    data.triples
        |> List.filter subjectMatches
        |> List.isEmpty
        |> not


hasTripleWithObject : IsBlankNodeOrIriOrLiteral compatible -> Graph -> Bool
hasTripleWithObject nodeFocusCompatible (Graph data) =
    let
        objectMatches : Triple -> Bool
        objectMatches triple =
            triple.object == nodeFocus

        nodeFocus : BlankNodeOrIriOrLiteral
        nodeFocus =
            Rdf.asBlankNodeOrIriOrLiteral nodeFocusCompatible
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


getObjectsAt :
    IsBlankNodeOrIri compatible1
    -> IsPath compatible2
    -> Graph
    -> List BlankNodeOrIriOrLiteral
getObjectsAt nodeFocus path (Graph data) =
    nodeFocus
        |> followPropertyPath data path
        |> List.unique


type alias GraphData rest =
    { rest
        | bySubjectByPredicate : Dict String (Dict String (List Triple))
        , byPredicateBySubject : Dict String (Dict String (List Triple))
    }


followPropertyPath :
    GraphData rest
    -> IsPath compatible1
    -> IsBlankNodeOrIri compatible2
    -> List BlankNodeOrIriOrLiteral
followPropertyPath data ((Term variant) as path) nodeFocusCompatible =
    let
        nodeFocus : BlankNodeOrIri
        nodeFocus =
            Rdf.asBlankNodeOrIri nodeFocusCompatible
    in
    case variant of
        Iri _ ->
            data.bySubjectByPredicate
                |> Dict.get (Rdf.serialize nodeFocus)
                |> Maybe.withDefault Dict.empty
                |> Dict.get (Rdf.serialize path)
                |> Maybe.withDefault []
                |> List.map .object

        Inverse ((Iri _) as nested) ->
            data.byPredicateBySubject
                |> Dict.get (Rdf.serialize (Term nested))
                |> Maybe.withDefault Dict.empty
                |> Dict.values
                |> List.concatMap
                    (\triples ->
                        let
                            objectRaw : String
                            objectRaw =
                                Rdf.serialize nodeFocus
                        in
                        List.filterMap
                            (\triple ->
                                if Rdf.serialize triple.object == objectRaw then
                                    Just triple.subject

                                else
                                    Nothing
                            )
                            triples
                    )
                |> List.map Rdf.asBlankNodeOrIriOrLiteral

        Sequence first rest ->
            let
                nodesAtFirst : List BlankNodeOrIriOrLiteral
                nodesAtFirst =
                    followPropertyPath data (Term first) nodeFocusCompatible

                step :
                    Variant
                    -> List BlankNodeOrIriOrLiteral
                    -> List BlankNodeOrIriOrLiteral
                step next nodes =
                    nodes
                        |> List.filterMap Rdf.toBlankNodeOrIri
                        |> List.map asBlankNodeOrIriCompatible
                        |> List.concatMap (followPropertyPath data (Term next))
            in
            List.foldl step nodesAtFirst rest

        ZeroOrMore propertyPathNested ->
            let
                focusNodes : List BlankNodeOrIriOrLiteral
                focusNodes =
                    nodeFocusCompatible
                        |> followPropertyPath data (Term propertyPathNested)
                        |> List.filter ((/=) (Rdf.asBlankNodeOrIriOrLiteral nodeFocus))
            in
            if List.isEmpty focusNodes then
                [ Rdf.asBlankNodeOrIriOrLiteral nodeFocus ]

            else
                Rdf.asBlankNodeOrIriOrLiteral nodeFocus
                    :: (focusNodes
                            |> List.filterMap Rdf.toBlankNodeOrIri
                            |> List.map asBlankNodeOrIriCompatible
                            |> List.concatMap (followPropertyPath data path)
                       )

        OneOrMore propertyPathNested ->
            let
                focusNodes : List BlankNodeOrIriOrLiteral
                focusNodes =
                    nodeFocusCompatible
                        |> followPropertyPath data (Term propertyPathNested)
                        |> List.filter ((/=) (Rdf.asBlankNodeOrIriOrLiteral nodeFocus))
            in
            if List.isEmpty focusNodes then
                []

            else
                focusNodes
                    |> List.filterMap Rdf.toBlankNodeOrIri
                    |> List.map asBlankNodeOrIriCompatible
                    |> List.concatMap
                        (followPropertyPath data path)

        ZeroOrOne propertyPathNested ->
            Rdf.asBlankNodeOrIriOrLiteral nodeFocus
                :: (nodeFocusCompatible
                        |> followPropertyPath data (Term propertyPathNested)
                        |> List.filter ((/=) (Rdf.asBlankNodeOrIriOrLiteral nodeFocus))
                   )

        _ ->
            []


asBlankNodeOrIriCompatible : BlankNodeOrIri -> IsBlankNodeOrIri compatible
asBlankNodeOrIriCompatible (Term node) =
    Term node


getObjects : IsBlankNodeOrIri compatible -> Graph -> List BlankNodeOrIriOrLiteral
getObjects nodeFocus (Graph data) =
    data.bySubjectByPredicate
        |> Dict.get (Rdf.serialize nodeFocus)
        |> Maybe.withDefault Dict.empty
        |> Dict.values
        |> List.concat
        |> List.map .object
        -- FIXME Make sure, objects are already unique when stored.
        |> List.unique


getPredicates : IsBlankNodeOrIri compatible -> Graph -> List Iri
getPredicates nodeFocus (Graph data) =
    let
        toIri : String -> Iri
        toIri key =
            key
                |> String.dropLeft 1
                |> String.dropRight 1
                |> Iri
                |> Term
    in
    data.bySubjectByPredicate
        |> Dict.get (Rdf.serialize nodeFocus)
        |> Maybe.withDefault Dict.empty
        |> Dict.keys
        |> List.map toIri


objectToList : Graph -> BlankNodeOrIri -> Maybe (List BlankNodeOrIriOrLiteral)
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


getFirst : IsBlankNodeOrIri compatible -> Graph -> Maybe BlankNodeOrIriOrLiteral
getFirst nodeFocus (Graph data) =
    case
        data.bySubjectByPredicate
            |> Dict.get (Rdf.serialize nodeFocus)
            |> Maybe.withDefault Dict.empty
            |> Dict.get (Rdf.serialize rdfFirst)
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
            |> Dict.get (Rdf.serialize nodeFocus)
            |> Maybe.withDefault Dict.empty
            |> Dict.get (Rdf.serialize rdfRest)
            |> Maybe.withDefault []
            |> List.map .object
            |> List.filterMap Rdf.toBlankNodeOrIri
            |> List.unique
    of
        [ value ] ->
            Just value

        _ ->
            Nothing



-- CONSTANTS


rdfNil : Iri
rdfNil =
    rdf "nil"


rdfFirst : Iri
rdfFirst =
    rdf "first"


rdfRest : Iri
rdfRest =
    rdf "rest"


iriXsdString : Iri
iriXsdString =
    Rdf.iri "http://www.w3.org/2001/XMLSchema#string"


iriXsdInt : Iri
iriXsdInt =
    Rdf.iri "http://www.w3.org/2001/XMLSchema#int"


iriXsdInteger : Iri
iriXsdInteger =
    Rdf.iri "http://www.w3.org/2001/XMLSchema#integer"


iriXsdDouble : Iri
iriXsdDouble =
    Rdf.iri "http://www.w3.org/2001/XMLSchema#double"


iriXsdFloat : Iri
iriXsdFloat =
    Rdf.iri "http://www.w3.org/2001/XMLSchema#float"


iriXsdDecimal : Iri
iriXsdDecimal =
    Rdf.iri "http://www.w3.org/2001/XMLSchema#decimal"


iriXsdDate : Iri
iriXsdDate =
    Rdf.iri "http://www.w3.org/2001/XMLSchema#date"


iriXsdDateTime : Iri
iriXsdDateTime =
    Rdf.iri "http://www.w3.org/2001/XMLSchema#dateTime"


iriXsdBoolean : Iri
iriXsdBoolean =
    Rdf.iri "http://www.w3.org/2001/XMLSchema#boolean"


iriRdfLangString : Iri
iriRdfLangString =
    Rdf.iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString"
