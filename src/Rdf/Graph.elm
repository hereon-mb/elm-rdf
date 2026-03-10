module Rdf.Graph exposing
    ( Graph
    , empty, singleton
    , fromTriples, fromTriplesSafe
    , Seed, initialSeed, seedGenerator
    , parse, parseSafe, Error(..), errorToString
    , decoder, decoderSafe, encode
    , isEmpty
    , insert, insertProperty, generateBlankNode
    , mintBlankNodes
    , getBase, setBase, clearBase
    , getPrefixes, addPrefix, addPrefixes, clearPrefixes
    , union
    , serialize, serializeTurtle
    )

{-| This module defines the [`Graph`](#Graph) type which models an [RDF
graph](https://www.w3.org/TR/rdf11-concepts/#section-rdf-graph),
and helper functions for building, updating, parsing, serializing these. Take
a look at the `Rdf.Graph.Decode` module if you want to extract Elm values from
a `Graph`. On the other hand, the `Rdf.Graph.Encode` module deals with
conveniently building these `Graph`'s.

@docs Graph


# Build

@docs empty, singleton
@docs fromTriples, fromTriplesSafe


## Seeds

@docs Seed, initialSeed, seedGenerator


## Turtle and N-Triples format

@docs parse, parseSafe, Error, errorToString


## RDFJS

@docs decoder, decoderSafe, encode


# Query

@docs isEmpty


# Transform

@docs insert, insertProperty, generateBlankNode
@docs mintBlankNodes


## Base and prefixes

@docs getBase, setBase, clearBase
@docs getPrefixes, addPrefix, addPrefixes, clearPrefixes


# Combine

@docs union


# Export

@docs serialize, serializeTurtle

-}

import Dict exposing (Dict)
import Internal.Graph as Internal exposing (Data, Graph(..))
import Internal.Term exposing (DataLiteral, Term(..), Variant(..), toVariant)
import Internal.Turtle as Turtle
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import Maybe.Extra as Maybe
import Parser
import Random
import Rdf
    exposing
        ( BlankNode
        , BlankNodeOrIri
        , BlankNodeOrIriOrLiteral
        , Iri
        , IsBlankNodeOrIri
        , IsBlankNodeOrIriOrLiteral
        , IsIri
        , IsPath
        , Prologue
        , Triple
        , asBlankNodeOrIri
        , asBlankNodeOrIriOrLiteral
        , asIri
        , rdf
        , serializeTriple
        , serializeWith
        , toBlankNodeOrIri
        , xsd
        )
import Regex exposing (Regex)
import Set exposing (Set)
import Tuple.Extra as Tuple
import UUID


{-| This type represents an [RDF
Graph](https://www.w3.org/TR/rdf11-concepts/#section-rdf-graph).
-}
type alias Graph =
    Internal.Graph



-- BUILD


{-| Create an empty RDF graph.
-}
empty : Graph
empty =
    Graph
        { base = Nothing
        , prefixes = Dict.empty
        , triples = []
        , subjects = []
        , objects = []
        , bySubjectByPredicate = Dict.empty
        , byPredicateBySubject = Dict.empty
        }


{-| Create an RDF graph which only contains one triple.
-}
singleton :
    IsBlankNodeOrIri compatible1
    -> IsIri compatible2
    -> IsBlankNodeOrIriOrLiteral compatible3
    -> Graph
singleton subject predicate object =
    fromTriples
        [ { subject = asBlankNodeOrIri subject
          , predicate = asIri predicate
          , object = asBlankNodeOrIriOrLiteral object
          }
        ]


{-| Convert a list of RDF triples into an RDF graph.
-}
fromTriples : List Triple -> Graph
fromTriples triples =
    Graph
        { base = Nothing
        , prefixes = Dict.empty
        , triples = triples
        , subjects = List.map .subject triples
        , objects = List.map .object triples
        , bySubjectByPredicate =
            triples
                |> List.map annotateWithIriSubject
                |> groupByTupleFirst
                |> List.map
                    (\( ( iriSubject, tripleFirst ), rest ) ->
                        ( iriSubject
                        , (tripleFirst :: List.map Tuple.second rest)
                            |> List.map annotateWithIriPredicate
                            |> groupByTupleFirst
                            |> dictFromGroups
                        )
                    )
                |> Dict.fromList
        , byPredicateBySubject =
            triples
                |> List.map annotateWithIriPredicate
                |> groupByTupleFirst
                |> List.map
                    (\( ( iriPredicate, tripleFirst ), rest ) ->
                        ( iriPredicate
                        , (tripleFirst :: List.map Tuple.second rest)
                            |> List.map annotateWithIriSubject
                            |> groupByTupleFirst
                            |> dictFromGroups
                        )
                    )
                |> Dict.fromList
        }


annotateWithIriSubject : Triple -> ( String, Triple )
annotateWithIriSubject triple =
    ( Rdf.serialize triple.subject, triple )


annotateWithIriPredicate : Triple -> ( String, Triple )
annotateWithIriPredicate triple =
    ( Rdf.serialize triple.predicate, triple )


groupByTupleFirst :
    List ( comparable, a )
    -> List ( ( comparable, a ), List ( comparable, a ) )
groupByTupleFirst =
    List.sortBy Tuple.first
        >> List.groupWhile (\( left, _ ) ( right, _ ) -> left == right)


dictFromGroups : List ( ( comparable, a ), List ( comparable, a ) ) -> Dict comparable (List a)
dictFromGroups =
    List.map
        (\( ( key, first ), rest ) ->
            ( key
            , first :: List.map Tuple.second rest
            )
        )
        >> Dict.fromList


{-| Like [`fromTriples`](#fromTriples), this converts a list of RDF triples
into an RDF graph, while also minting new blank node identifiers using the
provided [`Seed`](#Seed). This can be useful if you plan to merge multiple
graphs with [`union`](#union) and need to ensure that there are no identifiers
used twice.
-}
fromTriplesSafe : Seed -> List Triple -> ( Graph, Seed )
fromTriplesSafe seed triples =
    triples
        |> renameBlankNodes seed
        |> Tuple.mapFirst fromTriples



-- SEEDS


{-| A `Seed` is used to mint unique blank node identifiers. Functions like
[`parseSafe`](#parseSafe), [`fromTriplesSafe`](#fromTriplesSafe), or
[`generateBlankNode`](#generateBlankNode) take a `Seed` and then return an
updated one. You should always use the returned `Seed` in subsequent calls to
ensure that you don't get blank node identifier collisions.
-}
type Seed
    = Seed UUID.Seeds


{-| A fixed [`Seed`](#Seed). This is useful for tests. For actual application
code, take a look at [`seedGenerator`](#seedGenerator).
-}
initialSeed : Seed
initialSeed =
    Seed
        { seed1 = Random.initialSeed 1
        , seed2 = Random.initialSeed 2
        , seed3 = Random.initialSeed 3
        , seed4 = Random.initialSeed 4
        }


{-| A `Random.Generator` for generating a random [`Seed`](#Seed).
-}
seedGenerator : Random.Generator Seed
seedGenerator =
    Random.map4
        (\seed1 seed2 seed3 seed4 ->
            Seed
                { seed1 = seed1
                , seed2 = seed2
                , seed3 = seed3
                , seed4 = seed4
                }
        )
        Random.independentSeed
        Random.independentSeed
        Random.independentSeed
        Random.independentSeed



-- TURTLE AND N-TRIPLES FORMAT


{-| Parse a [Turtle](https://www.w3.org/TR/turtle/) or
[N-Triples](https://www.w3.org/TR/n-triples/) file and put all RDF triples into
a [`Graph`](#Graph). This function will keep blank node identifiers and only
mint new ones if they are not given in the original.

**Important:** For minting the [`initialSeed`](#initialSeed) is being used.
Therefore, you will probably get identificer clashes when parsing multiple
graphs and combining them later. It is usually better to use
[`parseSafe`](#parseSafe) instead.

-}
parse : String -> Result Error Graph
parse raw =
    raw
        |> Turtle.parse
        |> Result.mapError ErrorParser
        |> Result.andThen (collectTriples initialSeed)
        |> Result.map
            (\state ->
                List.foldl (Tuple.apply addPrefix)
                    (state.triples
                        |> fromTriples
                        |> (case state.base of
                                Nothing ->
                                    identity

                                Just base ->
                                    setBase base
                           )
                    )
                    (Dict.toList state.prefixes)
            )


{-| Parse a [Turtle](https://www.w3.org/TR/turtle/) or
[N-Triples](https://www.w3.org/TR/n-triples/) file and put all RDF triples into
a [`Graph`](#Graph). This function will keep blank node identifiers and only
mint new ones if they are not given in the original. For minting it uses the
provided [`Seed`](#Seed). Make sure to reuse the returned [`Seed`](#Seed) in
subsequent calls to avoid blank node identifier collisions.
-}
parseSafe : Seed -> String -> Result Error ( Graph, Seed )
parseSafe seed raw =
    raw
        |> Turtle.parse
        |> Result.mapError ErrorParser
        |> Result.andThen (collectTriples seed)
        |> Result.map
            (\state ->
                ( List.foldl (Tuple.apply addPrefix)
                    (state.triples
                        |> fromTriples
                        |> (case state.base of
                                Nothing ->
                                    identity

                                Just base ->
                                    setBase base
                           )
                    )
                    (Dict.toList state.prefixes)
                , state.seed
                )
            )


{-| You get one of these if parsing fails.
-}
type Error
    = ErrorParser (List Parser.DeadEnd)
    | MissingSubject
    | MissingPredicate
    | MissingBase
    | CouldNotResolvePrefixedName String String


{-| Turn a [`Error`](#Error) into a more human friendly error message.
-}
errorToString : String -> Error -> String
errorToString raw error =
    case error of
        ErrorParser deadEnds ->
            "I ran into a syntax error: I "
                ++ deadEndsToString raw deadEnds

        MissingSubject ->
            "I tried to create a triple, but no subject was set.  This is likely a bug in the turtle parser."

        MissingPredicate ->
            "I tried to create a triple, but no predicate was set.  This is likely a bug in the turtle parser."

        MissingBase ->
            "I tried to resolve an IRI of the form <name>, but no base was set for the document."

        CouldNotResolvePrefixedName prefix name ->
            "I tried to resolve "
                ++ prefix
                ++ ":"
                ++ name
                ++ ", but the prefix "
                ++ prefix
                ++ ": was not set."


deadEndsToString : String -> List Parser.DeadEnd -> String
deadEndsToString raw deadEnds =
    let
        lines : List String
        lines =
            String.lines raw
    in
    [ deadEnds
        |> List.map deadEndToString
        |> String.join ", "
    , ":\n\n"
    , deadEnds
        |> List.filterMap (deadEndToCode lines)
        |> String.join "\n"
    ]
        |> String.concat


deadEndToString : Parser.DeadEnd -> String
deadEndToString { row, col, problem } =
    let
        position : String
        position =
            [ "row "
            , String.fromInt row
            , " column "
            , String.fromInt col
            ]
                |> String.concat
    in
    case problem of
        Parser.Expecting expect ->
            "expected '" ++ expect ++ "' at " ++ position

        Parser.ExpectingInt ->
            "expected an integer at " ++ position

        Parser.ExpectingHex ->
            "expected a hex at " ++ position

        Parser.ExpectingOctal ->
            "expected an octal at " ++ position

        Parser.ExpectingBinary ->
            "expected a binary at " ++ position

        Parser.ExpectingFloat ->
            "expected a float at " ++ position

        Parser.ExpectingNumber ->
            "expected a number at " ++ position

        Parser.ExpectingVariable ->
            "expected a variable at " ++ position

        Parser.ExpectingSymbol expect ->
            "expected the symbol '" ++ expect ++ "' at " ++ position

        Parser.ExpectingKeyword expect ->
            "expected the keyword '" ++ expect ++ "' at " ++ position

        Parser.ExpectingEnd ->
            "expected the end of the document at " ++ position

        Parser.UnexpectedChar ->
            "ran into an unexpected character at " ++ position

        Parser.Problem text ->
            text ++ " at " ++ position

        Parser.BadRepeat ->
            "bad repetition at " ++ position


deadEndToCode : List String -> Parser.DeadEnd -> Maybe String
deadEndToCode lines { row } =
    [ List.getAt (row - 3) lines
        |> Maybe.map (\line -> "  " ++ line)
    , List.getAt (row - 2) lines
        |> Maybe.map (\line -> "  " ++ line)
    , List.getAt (row - 1) lines
        |> Maybe.map (\line -> "> " ++ line)
    ]
        |> List.filterMap identity
        |> String.join "\n"
        |> Just


type alias State =
    { base : Maybe String
    , prefixes : Dict String String
    , triples : List Triple
    , subjects : List BlankNodeOrIri
    , predicates : List Iri
    , seed : Seed
    , blankNodes : Dict String BlankNode
    }


stateInitial : Seed -> State
stateInitial seed =
    { base = Nothing
    , prefixes = Dict.empty
    , triples = []
    , subjects = []
    , predicates = []
    , seed = seed
    , blankNodes = Dict.empty
    }


collectTriples : Seed -> List Turtle.Statement -> Result Error State
collectTriples seed statements =
    List.foldl
        (\statement ->
            Result.andThen (collectTriplesStep statement)
        )
        (Ok (stateInitial seed))
        statements


collectTriplesStep : Turtle.Statement -> State -> Result Error State
collectTriplesStep statement state =
    case statement of
        Turtle.DirectivePrefixId prefix value ->
            Ok { state | prefixes = Dict.insert prefix value state.prefixes }

        Turtle.DirectiveBase base ->
            Ok { state | base = Just base }

        Turtle.DirectiveSparqlPrefix prefix value ->
            Ok { state | prefixes = Dict.insert prefix value state.prefixes }

        Turtle.DirectiveSparqlBase base ->
            Ok { state | base = Just base }

        Turtle.Triples (Turtle.TriplesSubject subject predicateObjectList) ->
            collectTriplesSubject subject predicateObjectList state

        Turtle.Triples (Turtle.TriplesBlankNodePropertyList predicateObjectListSubject predicateObjectList) ->
            let
                ( node, seed ) =
                    generateBlankNode state.seed
            in
            (predicateObjectListSubject ++ predicateObjectList)
                |> List.foldl
                    (\predicateObject ->
                        Result.andThen
                            (collectPredicateObjectList predicateObject)
                    )
                    (Ok
                        { state
                            | subjects = asBlankNodeOrIri node :: state.subjects
                            , seed = seed
                        }
                    )
                |> Result.map dropSubject


collectTriplesSubject :
    Turtle.Subject
    -> List Turtle.PredicateObjectList
    -> State
    -> Result Error State
collectTriplesSubject subject predicateObjectList state =
    case subject of
        Turtle.SubjectIri iri ->
            case resolveIri state iri of
                Err error ->
                    Err error

                Ok url ->
                    predicateObjectList
                        |> List.foldl
                            (\predicateObject ->
                                Result.andThen (collectPredicateObjectList predicateObject)
                            )
                            (Ok
                                { state
                                    | subjects =
                                        asBlankNodeOrIri (Rdf.iri url)
                                            :: state.subjects
                                }
                            )
                        |> Result.map dropSubject

        Turtle.SubjectBlankNode (Turtle.BlankNodeLabel label) ->
            case Dict.get label state.blankNodes of
                Nothing ->
                    let
                        ( node, seed ) =
                            generateBlankNode state.seed
                    in
                    predicateObjectList
                        |> List.foldl
                            (\predicateObject ->
                                Result.andThen
                                    (collectPredicateObjectList predicateObject)
                            )
                            (Ok
                                { state
                                    | subjects =
                                        asBlankNodeOrIri node
                                            :: state.subjects
                                    , seed = seed
                                    , blankNodes =
                                        Dict.insert label
                                            node
                                            state.blankNodes
                                }
                            )
                        |> Result.map dropSubject

                Just node ->
                    predicateObjectList
                        |> List.foldl
                            (\predicateObject ->
                                Result.andThen
                                    (collectPredicateObjectList predicateObject)
                            )
                            (Ok
                                { state
                                    | subjects =
                                        asBlankNodeOrIri node
                                            :: state.subjects
                                }
                            )
                        |> Result.map dropSubject

        Turtle.SubjectBlankNode Turtle.Anon ->
            let
                ( node, seed ) =
                    generateBlankNode state.seed
            in
            predicateObjectList
                |> List.foldl
                    (\predicateObject ->
                        Result.andThen
                            (collectPredicateObjectList predicateObject)
                    )
                    (Ok
                        { state
                            | subjects = asBlankNodeOrIri node :: state.subjects
                            , seed = seed
                        }
                    )
                |> Result.map dropSubject

        Turtle.SubjectCollection objects ->
            if List.isEmpty objects then
                predicateObjectList
                    |> List.foldl
                        (\predicateObject ->
                            Result.andThen
                                (collectPredicateObjectList predicateObject)
                        )
                        (Ok
                            { state
                                | subjects =
                                    asBlankNodeOrIri (rdf "nil")
                                        :: state.subjects
                            }
                        )
                    |> Result.map dropSubject

            else
                let
                    ( node, seed ) =
                        generateBlankNode state.seed
                in
                predicateObjectList
                    |> List.foldl
                        (\predicateObject ->
                            Result.andThen
                                (collectPredicateObjectList predicateObject)
                        )
                        (Ok
                            { state
                                | subjects =
                                    asBlankNodeOrIri node
                                        :: state.subjects
                                , seed = seed
                            }
                        )
                    |> Result.andThen (addCollection node objects)
                    |> Result.map dropSubject


collectPredicateObjectList :
    Turtle.PredicateObjectList
    -> State
    -> Result Error State
collectPredicateObjectList { verb, objects } state =
    let
        predicate : Result Error String
        predicate =
            case verb of
                Turtle.Predicate iri ->
                    resolveIri state iri

                Turtle.A ->
                    Ok "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
    in
    case predicate of
        Err error ->
            Err error

        Ok url ->
            objects
                |> List.foldl (Result.andThen << collectObject)
                    (Ok { state | predicates = Rdf.iri url :: state.predicates })
                |> Result.map dropPredicate


collectObject : Turtle.Object -> State -> Result Error State
collectObject object state =
    case object of
        Turtle.ObjectIri iri ->
            resolveIri state iri
                |> Result.andThen
                    (\url ->
                        addTriple
                            (asBlankNodeOrIriOrLiteral (Rdf.iri url))
                            state
                    )

        Turtle.ObjectBlankNode (Turtle.BlankNodeLabel label) ->
            case Dict.get label state.blankNodes of
                Nothing ->
                    let
                        ( node, seed ) =
                            generateBlankNode state.seed
                    in
                    addTriple (asBlankNodeOrIriOrLiteral node)
                        { state
                            | seed = seed
                            , blankNodes =
                                Dict.insert label
                                    node
                                    state.blankNodes
                        }

                Just node ->
                    addTriple (asBlankNodeOrIriOrLiteral node) state

        Turtle.ObjectBlankNode Turtle.Anon ->
            let
                ( node, seed ) =
                    generateBlankNode state.seed
            in
            addTriple (asBlankNodeOrIriOrLiteral node) { state | seed = seed }

        Turtle.ObjectCollection objects ->
            if List.isEmpty objects then
                addTriple (asBlankNodeOrIriOrLiteral (rdf "nil")) state

            else
                let
                    ( nodeFirst, seedFirst ) =
                        generateBlankNode state.seed
                in
                { state | seed = seedFirst }
                    |> addTriple (asBlankNodeOrIriOrLiteral nodeFirst)
                    |> Result.andThen (addCollection nodeFirst objects)

        Turtle.ObjectBlankNodePropertyList predicateObjects ->
            let
                ( node, seed ) =
                    generateBlankNode state.seed
            in
            predicateObjects
                |> List.foldl (Result.andThen << collectPredicateObjectList)
                    (Ok
                        { state
                            | subjects = asBlankNodeOrIri node :: state.subjects
                            , seed = seed
                        }
                    )
                |> Result.map dropSubject
                |> Result.andThen (addTriple (asBlankNodeOrIriOrLiteral node))

        Turtle.ObjectLiteral (Turtle.LiteralString value) ->
            addTriple (asBlankNodeOrIriOrLiteral (Rdf.string value)) state

        Turtle.ObjectLiteral (Turtle.LiteralLangString value lang) ->
            addTriple
                (asBlankNodeOrIriOrLiteral (Rdf.langString lang value))
                state

        Turtle.ObjectLiteral (Turtle.LiteralTyped value datatype) ->
            resolveIri state datatype
                |> Result.andThen
                    (\url ->
                        addTriple
                            (asBlankNodeOrIriOrLiteral
                                (Rdf.literal (Rdf.iri url) value)
                            )
                            state
                    )

        Turtle.ObjectLiteral (Turtle.LiteralInteger value) ->
            addTriple
                (asBlankNodeOrIriOrLiteral (Rdf.integer value))
                state

        Turtle.ObjectLiteral (Turtle.LiteralDecimal value) ->
            addTriple
                (asBlankNodeOrIriOrLiteral (Rdf.literal (xsd "decimal") value))
                state

        Turtle.ObjectLiteral (Turtle.LiteralDouble value) ->
            addTriple
                (asBlankNodeOrIriOrLiteral
                    (Rdf.literal (xsd "double") (String.fromFloat value))
                )
                state

        Turtle.ObjectLiteral Turtle.LiteralTrue ->
            addTriple (asBlankNodeOrIriOrLiteral (Rdf.boolean True)) state

        Turtle.ObjectLiteral Turtle.LiteralFalse ->
            addTriple (asBlankNodeOrIriOrLiteral (Rdf.boolean False)) state


addTriple : BlankNodeOrIriOrLiteral -> State -> Result Error State
addTriple object state =
    Result.map3 Triple
        (Result.fromMaybe MissingSubject (List.head state.subjects))
        (Result.fromMaybe MissingPredicate (List.head state.predicates))
        (Ok object)
        |> Result.map (\triple -> { state | triples = triple :: state.triples })


addCollection : BlankNode -> List Turtle.Object -> State -> Result Error State
addCollection nodeFirst objects state =
    case List.reverse objects of
        [] ->
            addTriple (asBlankNodeOrIriOrLiteral (rdf "nil")) state

        last :: rest ->
            rest
                |> List.reverse
                |> List.foldl
                    (\obj result ->
                        result
                            |> Result.andThen
                                (\( nodePrevious, stateNext ) ->
                                    let
                                        ( nodeNext, seedNext ) =
                                            generateBlankNode stateNext.seed
                                    in
                                    { stateNext
                                        | subjects =
                                            asBlankNodeOrIri nodePrevious
                                                :: stateNext.subjects
                                        , predicates =
                                            rdf "first"
                                                :: stateNext.predicates
                                        , seed = seedNext
                                    }
                                        |> collectObject obj
                                        |> Result.map dropPredicate
                                        |> Result.map
                                            (\stateNextNext ->
                                                { stateNextNext
                                                    | predicates =
                                                        rdf "rest"
                                                            :: stateNextNext.predicates
                                                }
                                            )
                                        |> Result.andThen
                                            (addTriple
                                                (asBlankNodeOrIriOrLiteral nodeNext)
                                            )
                                        |> Result.map dropPredicate
                                        |> Result.map dropSubject
                                        |> Result.map (Tuple.pair nodeNext)
                                )
                    )
                    (Ok ( nodeFirst, state ))
                |> Result.andThen
                    (\( nodePrevious, stateNext ) ->
                        { stateNext
                            | subjects =
                                asBlankNodeOrIri nodePrevious
                                    :: stateNext.subjects
                            , predicates =
                                rdf "first"
                                    :: stateNext.predicates
                        }
                            |> collectObject last
                            |> Result.map dropPredicate
                            |> Result.map
                                (\stateNextNext ->
                                    { stateNextNext
                                        | predicates =
                                            rdf "rest"
                                                :: stateNextNext.predicates
                                    }
                                )
                            |> Result.andThen
                                (addTriple
                                    (asBlankNodeOrIriOrLiteral (rdf "nil"))
                                )
                            |> Result.map dropPredicate
                            |> Result.map dropSubject
                    )


resolveIri : State -> Turtle.Iri -> Result Error String
resolveIri state iri =
    case iri of
        Turtle.IriRef url ->
            if Regex.contains regexScheme url then
                Ok url

            else
                case state.base of
                    Nothing ->
                        Err MissingBase

                    Just base ->
                        Ok (base ++ url)

        Turtle.PrefixedName prefix name ->
            Dict.get prefix state.prefixes
                |> Maybe.map (\url -> url ++ name)
                |> Result.fromMaybe (CouldNotResolvePrefixedName prefix name)


regexScheme : Regex
regexScheme =
    "^[a-z][a-z0-9+-.]*:"
        |> Regex.fromStringWith
            { caseInsensitive = True
            , multiline = False
            }
        |> Maybe.withDefault Regex.never


dropSubject : State -> State
dropSubject state =
    { state
        | subjects =
            state.subjects
                |> List.tail
                |> Maybe.withDefault []
    }


dropPredicate : State -> State
dropPredicate state =
    { state
        | predicates =
            state.predicates
                |> List.tail
                |> Maybe.withDefault []
    }


type alias Step =
    { seed : Seed
    , blankNodes : Dict String BlankNode
    , triples : List Triple
    }


stepInitial : Seed -> Step
stepInitial seed =
    { seed = seed
    , blankNodes = Dict.empty
    , triples = []
    }


renameBlankNodes : Seed -> List Triple -> ( List Triple, Seed )
renameBlankNodes seed triples =
    let
        stepFinal : Step
        stepFinal =
            renameBlankNodesHelp triples (stepInitial seed)
    in
    ( stepFinal.triples
    , stepFinal.seed
    )


renameBlankNodesHelp : List Triple -> Step -> Step
renameBlankNodesHelp triples step =
    case triples of
        [] ->
            step

        triple :: rest ->
            case triple.subject of
                Term (BlankNode labelSubject) ->
                    let
                        ( stepAfterSubject, nodeSubject ) =
                            remintBlankNode labelSubject step
                    in
                    case triple.object of
                        Term (BlankNode labelObject) ->
                            let
                                ( stepAfterObject, nodeObject ) =
                                    remintBlankNode labelObject stepAfterSubject
                            in
                            renameBlankNodesHelp rest
                                { stepAfterObject
                                    | triples =
                                        { subject =
                                            Rdf.asBlankNodeOrIri nodeSubject
                                        , predicate = triple.predicate
                                        , object =
                                            Rdf.asBlankNodeOrIriOrLiteral nodeObject
                                        }
                                            :: stepAfterObject.triples
                                }

                        _ ->
                            renameBlankNodesHelp rest
                                { stepAfterSubject
                                    | triples =
                                        { subject =
                                            Rdf.asBlankNodeOrIri nodeSubject
                                        , predicate = triple.predicate
                                        , object = triple.object
                                        }
                                            :: stepAfterSubject.triples
                                }

                _ ->
                    case triple.object of
                        Term (BlankNode labelObject) ->
                            let
                                ( stepAfterObject, nodeObject ) =
                                    remintBlankNode labelObject step
                            in
                            renameBlankNodesHelp rest
                                { stepAfterObject
                                    | triples =
                                        { subject = triple.subject
                                        , predicate = triple.predicate
                                        , object = Rdf.asBlankNodeOrIriOrLiteral nodeObject
                                        }
                                            :: stepAfterObject.triples
                                }

                        _ ->
                            renameBlankNodesHelp rest
                                { step | triples = triple :: step.triples }


remintBlankNode : String -> Step -> ( Step, BlankNode )
remintBlankNode label step =
    case Dict.get label step.blankNodes of
        Nothing ->
            let
                ( node, seedAfterMint ) =
                    generateBlankNode step.seed
            in
            ( { step
                | seed = seedAfterMint
                , blankNodes = Dict.insert label node step.blankNodes
              }
            , node
            )

        Just node ->
            ( step, node )



-- RDFJS


{-| A JSON decoder for decoding a [`Graph`](#Graph) from a JSON value according
to the [RDFJS Specification](https://rdf.js.org/).
-}
decoder : Decoder Graph
decoder =
    Decode.list tripleDecoder
        |> Decode.map fromTriples


{-| Like [`decoder`](#decoder), but mint fresh blank node identifiers using the
provided [`Seed`](#Seed).
-}
decoderSafe : Seed -> Decoder ( Graph, Seed )
decoderSafe seed =
    Decode.list tripleDecoder
        |> Decode.map (fromTriplesSafe seed)


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


objectDecoder : Decoder BlankNodeOrIriOrLiteral
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


{-| Encode a [`Graph`](#Graph) into a JSON value according to the [RDFJS
Specification](https://rdf.js.org/).
-}
encode : Graph -> Value
encode (Graph data) =
    data.bySubjectByPredicate
        |> Dict.values
        |> List.concatMap Dict.values
        |> List.concat
        |> Encode.list encodeTriple


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

        _ ->
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

        _ ->
            Encode.null


encodeObject : BlankNodeOrIriOrLiteral -> Value
encodeObject (Term variant) =
    case variant of
        BlankNode name ->
            encodeBlankNode name

        Iri url ->
            encodeIri url

        Literal data ->
            encodeLiteral data

        _ ->
            Encode.null


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



-- QUERY


{-| Determine if a [`Graph`](#Graph) is empty.

    isEmpty empty
    --> True

-}
isEmpty : Graph -> Bool
isEmpty (Graph data) =
    List.isEmpty data.triples



-- TRANSFORM


{-| Insert an RDF triple into the [`Graph`](#Graph).
-}
insert :
    IsBlankNodeOrIri compatible1
    -> IsIri compatible2
    -> IsBlankNodeOrIriOrLiteral compatible3
    -> Graph
    -> Graph
insert subject predicate object (Graph graph) =
    let
        keySubject : String
        keySubject =
            Rdf.serialize subject

        keyPredicate : String
        keyPredicate =
            Rdf.serialize predicate

        triple : Triple
        triple =
            { subject = asBlankNodeOrIri subject
            , predicate = asIri predicate
            , object = Rdf.asBlankNodeOrIriOrLiteral object
            }
    in
    Graph
        { graph
            | triples = triple :: graph.triples
            , subjects = asBlankNodeOrIri subject :: graph.subjects
            , objects = asBlankNodeOrIriOrLiteral object :: graph.objects
            , bySubjectByPredicate =
                Dict.update keySubject
                    (Maybe.map
                        (Dict.update keyPredicate
                            (Maybe.map (\triples -> triple :: triples)
                                >> Maybe.withDefault [ triple ]
                                >> Just
                            )
                        )
                        >> Maybe.withDefault
                            (Dict.singleton keyPredicate [ triple ])
                        >> Just
                    )
                    graph.bySubjectByPredicate
            , byPredicateBySubject =
                Dict.update keyPredicate
                    (Maybe.map
                        (Dict.update keySubject
                            (Maybe.map (\triples -> triple :: triples)
                                >> Maybe.withDefault [ triple ]
                                >> Just
                            )
                        )
                        >> Maybe.withDefault (Dict.singleton keySubject [ triple ])
                        >> Just
                    )
                    graph.byPredicateBySubject
        }


{-| Insert a property into the graph. If you provide a property path which does
not just consist of a single IRI (or its inverse), intermediate blank nodes
will be minted using the provided [`Seed`](#Seed).
-}
insertProperty :
    IsBlankNodeOrIri compatible1
    -> IsPath compatible2
    -> IsBlankNodeOrIriOrLiteral compatible3
    -> Graph
    -> Seed
    -> ( Graph, Seed )
insertProperty subject (Term variant) object graph seed =
    case variant of
        (Iri _) as predicate ->
            ( insert subject (Term predicate) object graph, seed )

        Sequence ((Iri _) as predicate) propertyPaths ->
            case getBlankNodeOrIriObject subject (Term predicate) graph of
                Nothing ->
                    let
                        ( idFocusNodeNext, seedUpdated ) =
                            generateBlankNode seed
                    in
                    ( insert subject (Term predicate) idFocusNodeNext graph
                    , seedUpdated
                    )
                        |> Tuple.apply
                            (insertAtNext
                                (List.map Term propertyPaths)
                                object
                                (asBlankNodeOrIri idFocusNodeNext)
                            )

                Just idFocusNodeNext ->
                    insertAtNext
                        (List.map Term propertyPaths)
                        object
                        (asBlankNodeOrIri idFocusNodeNext)
                        graph
                        seed

        Inverse ((Iri _) as predicate) ->
            case toBlankNodeOrIri object of
                Nothing ->
                    -- FIXME We want to error here
                    ( graph, seed )

                Just blankNodeOrIri ->
                    ( insert
                        blankNodeOrIri
                        (Term predicate)
                        (Rdf.asBlankNodeOrIri subject)
                        graph
                    , seed
                    )

        _ ->
            ( graph, seed )


insertAtNext :
    List (IsPath compatible1)
    -> IsBlankNodeOrIriOrLiteral compatible2
    -> IsBlankNodeOrIri compatible3
    -> Graph
    -> Seed
    -> ( Graph, Seed )
insertAtNext propertyPaths object idFocusNodeNext graphNext seedNext =
    case propertyPaths of
        [] ->
            ( graphNext, seedNext )

        [ first ] ->
            insertProperty idFocusNodeNext first object graphNext seedNext

        (Term first) :: rest ->
            insertProperty idFocusNodeNext
                (Term (Sequence first (List.map toVariant rest)))
                object
                graphNext
                seedNext


getBlankNodeOrIriObject : IsBlankNodeOrIri compatible1 -> Iri -> Graph -> Maybe BlankNodeOrIri
getBlankNodeOrIriObject subject predicate (Graph graph) =
    case
        followPropertyPath graph predicate subject
            |> List.filterMap Rdf.toBlankNodeOrIri
            |> List.unique
    of
        [ object ] ->
            Just object

        _ ->
            Nothing


followPropertyPath :
    Data
    -> Iri
    -> IsBlankNodeOrIri compatible
    -> List BlankNodeOrIriOrLiteral
followPropertyPath data predicate subject =
    data.bySubjectByPredicate
        |> Dict.get (Rdf.serialize subject)
        |> Maybe.withDefault Dict.empty
        |> Dict.get (Rdf.serialize predicate)
        |> Maybe.withDefault []
        |> List.map .object


{-| Create a blank node with a freshy minted identifier using the provided
[`Seed`](#Seed).
-}
generateBlankNode : Seed -> ( BlankNode, Seed )
generateBlankNode (Seed seed) =
    let
        ( uuid, seedUpdated ) =
            UUID.step seed
    in
    ( Term (BlankNode (UUID.toString uuid))
    , Seed seedUpdated
    )


{-| Mint new identifiers for all blank nodes in the graph using the provided
[`Seed`](#Seed). This can be usefull before merging multiple
[`Graph`](#Graph)'s with [`union`](#union).
-}
mintBlankNodes : Seed -> Graph -> ( Graph, Seed )
mintBlankNodes seed (Graph data) =
    let
        ( triplesUpdated, seedUpdated ) =
            renameBlankNodes seed data.triples
    in
    ( Graph
        { data
            | triples = triplesUpdated
            , subjects = List.map .subject triplesUpdated
            , objects = List.map .object triplesUpdated
            , bySubjectByPredicate =
                triplesUpdated
                    |> List.map annotateWithIriSubject
                    |> groupByTupleFirst
                    |> List.map
                        (\( ( iriSubject, tripleFirst ), rest ) ->
                            ( iriSubject
                            , (tripleFirst :: List.map Tuple.second rest)
                                |> List.map annotateWithIriPredicate
                                |> groupByTupleFirst
                                |> dictFromGroups
                            )
                        )
                    |> Dict.fromList
            , byPredicateBySubject =
                triplesUpdated
                    |> List.map annotateWithIriPredicate
                    |> groupByTupleFirst
                    |> List.map
                        (\( ( iriPredicate, tripleFirst ), rest ) ->
                            ( iriPredicate
                            , (tripleFirst :: List.map Tuple.second rest)
                                |> List.map annotateWithIriSubject
                                |> groupByTupleFirst
                                |> dictFromGroups
                            )
                        )
                    |> Dict.fromList
        }
    , seedUpdated
    )



-- BASE AND PREFIXES


{-| Return the current base declaration.
-}
getBase : Graph -> Maybe String
getBase (Graph data) =
    data.base


{-| Set the base declaration to the provided URL.
-}
setBase : String -> Graph -> Graph
setBase base (Graph data) =
    Graph { data | base = Just base }


{-| Remove the base declaration.
-}
clearBase : Graph -> Graph
clearBase (Graph data) =
    Graph { data | base = Nothing }


{-| Return all defined prefix declarations.
-}
getPrefixes : Graph -> Dict String String
getPrefixes (Graph data) =
    data.prefixes


{-| Add one prefix declaration to the [`Graph`](#Graph). An existing
declaration with the same name will be overwritten.
-}
addPrefix : String -> String -> Graph -> Graph
addPrefix prefix value (Graph data) =
    Graph { data | prefixes = Dict.insert prefix value data.prefixes }


{-| Add some prefix declarations to the [`Graph`](#Graph). Existing prefixes
with the same name will be overwritten.
-}
addPrefixes : Dict String String -> Graph -> Graph
addPrefixes prefixes (Graph data) =
    Graph { data | prefixes = Dict.union prefixes data.prefixes }


{-| Remove all prefix declarations.
-}
clearPrefixes : Graph -> Graph
clearPrefixes (Graph data) =
    Graph { data | prefixes = Dict.empty }



-- COMBINE


{-| Merge two [`Graph`](#Graph)'s into one.

**Important:** This will not mint new blank node identifiers, so you might get
unwanted identifier collissions. Take a look at
[`mintBlankNodes`](#mintBlankNodes) for a safer approach.

-}
union : Graph -> Graph -> Graph
union (Graph g) (Graph h) =
    let
        merge :
            Dict String (Dict String (List Triple))
            -> Dict String (Dict String (List Triple))
            -> Dict String (Dict String (List Triple))
        merge dictLeft dictRight =
            Dict.merge
                Dict.insert
                (\keyOuter valueLeftOuter valueRightOuter ->
                    Dict.insert keyOuter
                        (Dict.merge Dict.insert
                            (\keyInner valueLeftInnter valueRightInner ->
                                Dict.insert keyInner
                                    (List.append valueLeftInnter valueRightInner)
                            )
                            Dict.insert
                            valueLeftOuter
                            valueRightOuter
                            Dict.empty
                        )
                )
                Dict.insert
                dictLeft
                dictRight
                Dict.empty
    in
    Graph
        { g
            | triples = g.triples ++ h.triples
            , subjects = g.subjects ++ h.subjects
            , objects = g.objects ++ h.objects
            , bySubjectByPredicate = merge g.bySubjectByPredicate h.bySubjectByPredicate
            , byPredicateBySubject = merge g.byPredicateBySubject h.byPredicateBySubject
        }



-- EXPORT


{-| Serialize a [`Graph`](#Graph) into the
[N-Triples](https://www.w3.org/TR/n-triples/) format.
-}
serialize : Graph -> String
serialize (Graph data) =
    data.bySubjectByPredicate
        |> Dict.values
        |> List.concatMap Dict.values
        |> List.concat
        |> List.unique
        |> List.map serializeTriple
        |> String.join "\n"


{-| Serialize a [`Graph`](#Graph) into the
[Turtle](https://www.w3.org/TR/turtle/) format. Take a look the [Base and
prefixes section](#base-and-prefixes) to adjust the serialization.
-}
serializeTurtle : Graph -> String
serializeTurtle (Graph data) =
    let
        config :
            { base : Maybe String
            , prefixes : List ( String, String )
            }
        config =
            { base = data.base
            , prefixes = Dict.toList data.prefixes
            }

        ( inlinedSubjects, _ ) =
            List.foldl
                (\{ object } ( inlined, notInlined ) ->
                    if Maybe.isJust (Rdf.toBlankNode object) then
                        let
                            key : String
                            key =
                                Rdf.serialize object
                        in
                        if Set.member key notInlined then
                            ( inlined, notInlined )

                        else if Set.member key inlined then
                            ( Set.remove key inlined
                            , Set.insert key notInlined
                            )

                        else
                            ( Set.insert key inlined
                            , notInlined
                            )

                    else
                        ( inlined, notInlined )
                )
                ( Set.empty, Set.empty )
                data.triples
    in
    String.concat
        [ case data.base of
            Nothing ->
                ""

            Just base ->
                "@base <" ++ base ++ "> .\n"
        , if Dict.isEmpty data.prefixes then
            ""

          else
            String.concat
                [ data.prefixes
                    |> Dict.toList
                    |> List.sortBy Tuple.first
                    |> List.map (\( prefix, value ) -> "@prefix " ++ prefix ++ ": <" ++ value ++ "> .")
                    |> String.join "\n"
                , "\n\n"
                ]
        , data.bySubjectByPredicate
            |> Dict.filter (\key _ -> not (Set.member key inlinedSubjects))
            |> Dict.values
            |> List.filterMap
                (\predicateObjectsDict ->
                    let
                        predicateObjectsList : List (List Triple)
                        predicateObjectsList =
                            Dict.values predicateObjectsDict
                    in
                    case Maybe.andThen List.head (List.head predicateObjectsList) of
                        Nothing ->
                            Nothing

                        Just { subject } ->
                            Just
                                (serializeWith config subject
                                    ++ "\n"
                                    ++ serializePredicateObjects config
                                        data.bySubjectByPredicate
                                        inlinedSubjects
                                        predicateObjectsList
                                    ++ " ."
                                )
                )
            |> String.join "\n\n"
        ]


serializePredicateObjects :
    Prologue
    -> Dict String (Dict String (List Triple))
    -> Set String
    -> List (List Triple)
    -> String
serializePredicateObjects config bySubjectByPredicate inlinedSubjects predicateObjects =
    predicateObjects
        |> List.sortWith
            (\left right ->
                Maybe.withDefault EQ
                    (Maybe.map2 predicateToOrder
                        (left
                            |> List.head
                            |> Maybe.map .predicate
                        )
                        (right
                            |> List.head
                            |> Maybe.map .predicate
                        )
                    )
            )
        |> List.map
            (List.unique
                >> List.map
                    (serializePredicateObjectWith config
                        bySubjectByPredicate
                        inlinedSubjects
                    )
                >> String.join " ;\n"
            )
        |> String.join " ;\n"
        |> indent


predicateToOrder : Iri -> Iri -> Order
predicateToOrder left right =
    case iriToKind left of
        RdfType ->
            LT

        RdfSyntax ->
            case iriToKind right of
                RdfType ->
                    GT

                RdfSyntax ->
                    compare (Rdf.toUrl left) (Rdf.toUrl right)

                _ ->
                    LT

        RdfSchema ->
            case iriToKind right of
                RdfType ->
                    GT

                RdfSyntax ->
                    GT

                RdfSchema ->
                    compare (Rdf.toUrl left) (Rdf.toUrl right)

                _ ->
                    LT

        OtherScope ->
            case iriToKind right of
                RdfType ->
                    GT

                RdfSyntax ->
                    GT

                RdfSchema ->
                    GT

                OtherScope ->
                    compare (Rdf.toUrl left) (Rdf.toUrl right)


type Kind
    = RdfType
    | RdfSyntax
    | RdfSchema
    | OtherScope


iriToKind : Iri -> Kind
iriToKind iri =
    let
        url : String
        url =
            Rdf.toUrl iri
    in
    if url == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" then
        RdfType

    else if String.startsWith "http://www.w3.org/1999/02/22-rdf-syntax-ns#" url then
        RdfSyntax

    else if String.startsWith "http://www.w3.org/2000/01/rdf-schema#" url then
        RdfSchema

    else
        OtherScope


indent : String -> String
indent text =
    text
        |> String.lines
        |> List.map (\line -> "  " ++ line)
        |> String.join "\n"


serializePredicateObjectWith :
    Prologue
    -> Dict String (Dict String (List Triple))
    -> Set String
    -> Triple
    -> String
serializePredicateObjectWith config bySubjectByPredicate inlinedSubjects { predicate, object } =
    let
        keyObject : String
        keyObject =
            Rdf.serialize object
    in
    [ serializeWith config predicate
    , if Set.member keyObject inlinedSubjects then
        case Dict.get keyObject bySubjectByPredicate of
            Nothing ->
                serializeWith config object

            Just predicateObjects ->
                "[\n"
                    ++ indent
                        (serializePredicateObjects config
                            bySubjectByPredicate
                            inlinedSubjects
                            (Dict.values predicateObjects)
                            ++ " ]"
                        )

      else
        serializeWith config object
    ]
        |> String.join " "
