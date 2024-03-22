module RDF.Graph exposing
    ( Graph(..), GraphData
    , union
    , isEmpty
    , emptyGraph, singleton
    , decoder, encode, parse, serialize
    , fromNTriples
    , Seed, initialSeed
    , insert, insertAt, generateBlankNode
    )

{-|

@docs Graph, GraphData
@docs union

@docs isEmpty


## Create

@docs emptyGraph, singleton
@docs decoder, encode, parse, serialize
@docs fromNTriples


## Update

@docs Seed, initialSeed
@docs insert, insertAt, generateBlankNode

-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import RDF
    exposing
        ( BlankNode
        , BlankNodeOrIri
        , BlankNodeOrIriOrAnyLiteral
        , Error
        , Iri
        , IsBlankNodeOrIri
        , IsIri
        , NTriple
        , Node(..)
        , NodeInternal(..)
        , encodeNTriple
        , forgetCompatible
        , nTripleDecoder
        , parseNTriples
        , serializeNTriple
        , serializeNode
        , serializeNodeHelp
        , unwrap
        )
import RDF.PropertyPath exposing (PropertyPath(..))
import Random
import Tuple.Extra as Tuple
import UUID


{-| FIXME internals exposed for benchmarks
-}
type Graph
    = Graph GraphData


{-| TODO Add documentation
-}
type alias GraphData =
    { nTriples : List NTriple
    , subjects : List BlankNodeOrIri
    , objects : List BlankNodeOrIriOrAnyLiteral
    , bySubjectByPredicate : Dict String (Dict String (List NTriple))
    , byPredicateBySubject : Dict String (Dict String (List NTriple))
    }


{-| FIXME Input graphs are expected to be disjoint on blank nodes. Otherwise, behavior is undefined.
-}
union : Graph -> Graph -> Graph
union (Graph g) (Graph h) =
    let
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
            | nTriples = g.nTriples ++ h.nTriples
            , subjects = g.subjects ++ h.subjects
            , objects = g.objects ++ h.objects
            , bySubjectByPredicate = merge g.bySubjectByPredicate h.bySubjectByPredicate
            , byPredicateBySubject = merge g.byPredicateBySubject h.byPredicateBySubject
        }


{-| TODO Add documentation
-}
type Seed
    = Seed UUID.Seeds


{-| TODO Add documentation
-}
emptyGraph : Graph
emptyGraph =
    Graph
        { nTriples = []
        , subjects = []
        , objects = []
        , bySubjectByPredicate = Dict.empty
        , byPredicateBySubject = Dict.empty
        }


{-| TODO Add documentation
-}
singleton : IsBlankNodeOrIri compatible1 -> IsIri compatible2 -> Node compatible3 -> Graph
singleton subject predicate object =
    fromNTriples
        [ { subject = forgetCompatible subject
          , predicate = forgetCompatible predicate
          , object = forgetCompatible object
          }
        ]


{-| TODO Add documentation
-}
isEmpty : Graph -> Bool
isEmpty (Graph data) =
    List.isEmpty data.nTriples


{-| TODO Add documentation
-}
initialSeed : Seed
initialSeed =
    Seed
        { seed1 = Random.initialSeed 1
        , seed2 = Random.initialSeed 2
        , seed3 = Random.initialSeed 3
        , seed4 = Random.initialSeed 4
        }


{-| TODO Add documentation
-}
insert : IsBlankNodeOrIri compatible1 -> IsIri compatible2 -> Node compatible3 -> Graph -> Graph
insert subject predicate object (Graph graph) =
    let
        keySubject =
            serializeNode subject

        keyPredicate =
            serializeNode predicate

        triple =
            { subject = forgetCompatible subject
            , predicate = forgetCompatible predicate
            , object = forgetCompatible object
            }
    in
    Graph
        { graph
            | nTriples = triple :: graph.nTriples
            , subjects = forgetCompatible subject :: graph.subjects
            , objects = forgetCompatible object :: graph.objects
            , bySubjectByPredicate =
                Dict.update keySubject
                    (Maybe.map
                        (Dict.update keyPredicate
                            (Maybe.map (\triples -> triple :: triples)
                                >> Maybe.withDefault [ triple ]
                                >> Just
                            )
                        )
                        >> Maybe.withDefault (Dict.singleton keyPredicate [ triple ])
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


{-| TODO Add documentation
-}
insertAt : IsBlankNodeOrIri compatible1 -> PropertyPath -> Node compatible2 -> Graph -> Seed -> ( Graph, Seed )
insertAt subject path object graph seed =
    case path of
        PredicatePath predicate ->
            ( insert subject predicate object graph, seed )

        SequencePath (PredicatePath predicate) propertyPaths ->
            let
                insertAtNext idFocusNodeNext graphNext seedNext =
                    case propertyPaths of
                        [] ->
                            ( graphNext, seedNext )

                        [ first ] ->
                            insertAt idFocusNodeNext first object graphNext seedNext

                        first :: rest ->
                            insertAt idFocusNodeNext (SequencePath first rest) object graphNext seedNext
            in
            case getBlankNodeOrIriObject subject predicate graph of
                Nothing ->
                    let
                        ( idFocusNodeNext, seedUpdated ) =
                            generateBlankNode seed
                    in
                    ( insert subject predicate idFocusNodeNext graph, seedUpdated )
                        |> Tuple.apply (insertAtNext (forgetCompatible idFocusNodeNext))

                Just idFocusNodeNext ->
                    insertAtNext (forgetCompatible idFocusNodeNext) graph seed

        _ ->
            ( graph, seed )


getBlankNodeOrIriObject : IsBlankNodeOrIri compatible1 -> Iri -> Graph -> Maybe BlankNodeOrIriOrAnyLiteral
getBlankNodeOrIriObject subject predicate (Graph graph) =
    case List.unique (List.map Node (followPropertyPath graph predicate (unwrap subject))) of
        [ object ] ->
            Just object

        _ ->
            Nothing


followPropertyPath : GraphData -> Iri -> NodeInternal -> List NodeInternal
followPropertyPath data predicate subject =
    data.bySubjectByPredicate
        |> Dict.get (serializeNodeHelp subject)
        |> Maybe.withDefault Dict.empty
        |> Dict.get (serializeNode predicate)
        |> Maybe.withDefault []
        |> List.map (.object >> unwrap)


{-| TODO Add documentation
-}
generateBlankNode : Seed -> ( BlankNode, Seed )
generateBlankNode (Seed seed) =
    let
        ( uuid, seedUpdated ) =
            UUID.step seed
    in
    ( Node (BlankNode (UUID.toString uuid))
    , Seed seedUpdated
    )


{-| TODO Add documentation
-}
serialize : Graph -> String
serialize (Graph data) =
    data.bySubjectByPredicate
        |> Dict.values
        |> List.concatMap Dict.values
        |> List.concat
        |> List.map serializeNTriple
        |> String.join "\n"


{-| TODO Add documentation
-}
decoder : Decoder Graph
decoder =
    Decode.list nTripleDecoder
        |> Decode.map fromNTriples


{-| TODO Add documentation
-}
encode : Graph -> Value
encode (Graph data) =
    data.bySubjectByPredicate
        |> Dict.values
        |> List.concatMap Dict.values
        |> List.concat
        |> Encode.list encodeNTriple


{-| Get all N-Triples from a n-triple text file.
-}
parse : String -> Result (List Error) Graph
parse raw =
    raw
        |> parseNTriples
        |> Result.map fromNTriples


{-| TODO Add documentation
-}
fromNTriples : List NTriple -> Graph
fromNTriples nTriples =
    Graph
        { nTriples = nTriples
        , subjects = List.map .subject nTriples
        , objects = List.map .object nTriples
        , bySubjectByPredicate =
            nTriples
                |> List.map annotateWithIriSubject
                |> groupByTupleFirst
                |> List.map
                    (\( ( iriSubject, nTripleFirst ), rest ) ->
                        ( iriSubject
                        , (nTripleFirst :: List.map Tuple.second rest)
                            |> List.map annotateWithIriPredicate
                            |> groupByTupleFirst
                            |> dictFromGroups
                        )
                    )
                |> Dict.fromList
        , byPredicateBySubject =
            nTriples
                |> List.map annotateWithIriPredicate
                |> groupByTupleFirst
                |> List.map
                    (\( ( iriPredicate, nTripleFirst ), rest ) ->
                        ( iriPredicate
                        , (nTripleFirst :: List.map Tuple.second rest)
                            |> List.map annotateWithIriSubject
                            |> groupByTupleFirst
                            |> dictFromGroups
                        )
                    )
                |> Dict.fromList
        }


annotateWithIriSubject : NTriple -> ( String, NTriple )
annotateWithIriSubject nTriple =
    ( serializeNode nTriple.subject, nTriple )


annotateWithIriPredicate : NTriple -> ( String, NTriple )
annotateWithIriPredicate nTriple =
    ( serializeNode nTriple.predicate, nTriple )


groupByTupleFirst : List ( comparable, a ) -> List ( ( comparable, a ), List ( comparable, a ) )
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
