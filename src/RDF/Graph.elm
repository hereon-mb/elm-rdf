module RDF.Graph exposing
    ( Graph(..), GraphData
    , union
    , isEmpty
    , emptyGraph, singleton
    , decoder, encode
    , parse, Error(..)
    , serialize
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
@docs decoder, encode
@docs parse, Error
@docs serialize
@docs fromNTriples


## Update

@docs Seed, initialSeed
@docs insert, insertAt, generateBlankNode

-}

import Dict exposing (Dict)
import Internal.Turtle as Turtle
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import Parser
import RDF
    exposing
        ( BlankNode
        , BlankNodeOrIri
        , BlankNodeOrIriOrAnyLiteral
        , Iri
        , IsBlankNodeOrIri
        , IsIri
        , NTriple
        , Node(..)
        , NodeInternal(..)
        , encodeNTriple
        , forgetCompatible
        , nTripleDecoder
        , serializeNTriple
        , serializeNode
        , serializeNodeHelp
        , unwrap
        )
import RDF.Namespaces exposing (rdf, xsd)
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
        merge : Dict String (Dict String (List NTriple)) -> Dict String (Dict String (List NTriple)) -> Dict String (Dict String (List NTriple))
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
        keySubject : String
        keySubject =
            serializeNode subject

        keyPredicate : String
        keyPredicate =
            serializeNode predicate

        triple : NTriple
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
                insertAtNext : IsBlankNodeOrIri compatible1 -> Graph -> Seed -> ( Graph, Seed )
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


{-| Get all RDF Triples from a turtle text file.
-}
parse : String -> Result Error Graph
parse raw =
    raw
        |> Turtle.parse
        |> Result.mapError ErrorParser
        |> Result.andThen collectNTriples
        |> Result.map fromNTriples


{-| TODO
-}
type Error
    = ErrorParser (List Parser.DeadEnd)
    | MissingSubject
    | MissingPredicate
    | MissingBase
    | CouldNotResolvePrefixedName String String


type alias State =
    { base : Maybe String
    , prefixes : Dict String String
    , nTriples : List NTriple
    , subjects : List BlankNodeOrIri
    , predicates : List Iri
    , seed : Seed
    , blankNodes : Dict String BlankNode
    }


stateInitial : State
stateInitial =
    { base = Nothing
    , prefixes = Dict.empty
    , nTriples = []
    , subjects = []
    , predicates = []
    , seed = initialSeed
    , blankNodes = Dict.empty
    }


collectNTriples : List Turtle.Statement -> Result Error (List NTriple)
collectNTriples statements =
    statements
        |> List.foldl (\statement -> Result.andThen (collectNTriplesStep statement)) (Ok stateInitial)
        |> Result.map .nTriples


collectNTriplesStep : Turtle.Statement -> State -> Result Error State
collectNTriplesStep statement state =
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
                    mintBlankNode state.seed
            in
            (predicateObjectListSubject ++ predicateObjectList)
                |> List.foldl (\predicateObject -> Result.andThen (collectPredicateObjectList predicateObject))
                    (Ok
                        { state
                            | subjects = RDF.toBlankNodeOrIri node :: state.subjects
                            , seed = seed
                        }
                    )
                |> Result.map dropSubject


collectTriplesSubject : Turtle.Subject -> List Turtle.PredicateObjectList -> State -> Result Error State
collectTriplesSubject subject predicateObjectList state =
    case subject of
        Turtle.SubjectIri iri ->
            case resolveIri state iri of
                Err error ->
                    Err error

                Ok url ->
                    predicateObjectList
                        |> List.foldl (\predicateObject -> Result.andThen (collectPredicateObjectList predicateObject))
                            (Ok { state | subjects = RDF.toBlankNodeOrIri (RDF.iri url) :: state.subjects })
                        |> Result.map dropSubject

        Turtle.SubjectBlankNode (Turtle.BlankNodeLabel label) ->
            case Dict.get label state.blankNodes of
                Nothing ->
                    let
                        ( node, seed ) =
                            mintBlankNode state.seed
                    in
                    predicateObjectList
                        |> List.foldl (\predicateObject -> Result.andThen (collectPredicateObjectList predicateObject))
                            (Ok
                                { state
                                    | subjects = RDF.toBlankNodeOrIri node :: state.subjects
                                    , seed = seed
                                    , blankNodes = Dict.insert label node state.blankNodes
                                }
                            )
                        |> Result.map dropSubject

                Just node ->
                    predicateObjectList
                        |> List.foldl (\predicateObject -> Result.andThen (collectPredicateObjectList predicateObject))
                            (Ok { state | subjects = RDF.toBlankNodeOrIri node :: state.subjects })
                        |> Result.map dropSubject

        Turtle.SubjectBlankNode Turtle.Anon ->
            let
                ( node, seed ) =
                    mintBlankNode state.seed
            in
            predicateObjectList
                |> List.foldl (\predicateObject -> Result.andThen (collectPredicateObjectList predicateObject))
                    (Ok
                        { state
                            | subjects = RDF.toBlankNodeOrIri node :: state.subjects
                            , seed = seed
                        }
                    )
                |> Result.map dropSubject

        Turtle.SubjectCollection objects ->
            if List.isEmpty objects then
                predicateObjectList
                    |> List.foldl (\predicateObject -> Result.andThen (collectPredicateObjectList predicateObject))
                        (Ok { state | subjects = RDF.toBlankNodeOrIri (rdf "nil") :: state.subjects })
                    |> Result.map dropSubject

            else
                let
                    ( node, seed ) =
                        mintBlankNode state.seed
                in
                predicateObjectList
                    |> List.foldl (\predicateObject -> Result.andThen (collectPredicateObjectList predicateObject))
                        (Ok
                            { state
                                | subjects = RDF.toBlankNodeOrIri node :: state.subjects
                                , seed = seed
                            }
                        )
                    |> Result.andThen (addCollection node objects)
                    |> Result.map dropSubject


mintBlankNode : Seed -> ( BlankNode, Seed )
mintBlankNode (Seed seed) =
    UUID.step seed
        |> Tuple.mapFirst (UUID.toString >> RDF.blankNode)
        |> Tuple.mapSecond Seed


collectPredicateObjectList : Turtle.PredicateObjectList -> State -> Result Error State
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
                    (Ok { state | predicates = RDF.iri url :: state.predicates })
                |> Result.map dropPredicate


collectObject : Turtle.Object -> State -> Result Error State
collectObject object state =
    case object of
        Turtle.ObjectIri iri ->
            resolveIri state iri
                |> Result.andThen (\url -> addTriple (RDF.toBlankNodeOrIriOrAnyLiteral (RDF.iri url)) state)

        Turtle.ObjectBlankNode (Turtle.BlankNodeLabel label) ->
            case Dict.get label state.blankNodes of
                Nothing ->
                    let
                        ( node, seed ) =
                            mintBlankNode state.seed
                    in
                    addTriple (RDF.toBlankNodeOrIriOrAnyLiteral node)
                        { state
                            | seed = seed
                            , blankNodes = Dict.insert label node state.blankNodes
                        }

                Just node ->
                    addTriple (RDF.toBlankNodeOrIriOrAnyLiteral node) state

        Turtle.ObjectBlankNode Turtle.Anon ->
            let
                ( node, seed ) =
                    mintBlankNode state.seed
            in
            addTriple (RDF.toBlankNodeOrIriOrAnyLiteral node) { state | seed = seed }

        Turtle.ObjectCollection objects ->
            if List.isEmpty objects then
                addTriple (RDF.toBlankNodeOrIriOrAnyLiteral (rdf "nil")) state

            else
                let
                    ( nodeFirst, seedFirst ) =
                        mintBlankNode state.seed
                in
                { state | seed = seedFirst }
                    |> addTriple (RDF.toBlankNodeOrIriOrAnyLiteral nodeFirst)
                    |> Result.andThen (addCollection nodeFirst objects)

        Turtle.ObjectBlankNodePropertyList predicateObjects ->
            let
                ( node, seed ) =
                    mintBlankNode state.seed
            in
            predicateObjects
                |> List.foldl (Result.andThen << collectPredicateObjectList)
                    (Ok
                        { state
                            | subjects = RDF.toBlankNodeOrIri node :: state.subjects
                            , seed = seed
                        }
                    )
                |> Result.map dropSubject
                |> Result.andThen (addTriple (RDF.toBlankNodeOrIriOrAnyLiteral node))

        Turtle.ObjectLiteral (Turtle.LiteralString value) ->
            addTriple (RDF.toBlankNodeOrIriOrAnyLiteral (RDF.string value)) state

        Turtle.ObjectLiteral (Turtle.LiteralLangString value lang) ->
            addTriple (RDF.toBlankNodeOrIriOrAnyLiteral (RDF.langString lang value)) state

        Turtle.ObjectLiteral (Turtle.LiteralTyped value datatype) ->
            resolveIri state datatype
                |> Result.andThen (\url -> addTriple (RDF.toBlankNodeOrIriOrAnyLiteral (RDF.literal (RDF.iri url) value)) state)

        Turtle.ObjectLiteral (Turtle.LiteralInteger value) ->
            addTriple (RDF.toBlankNodeOrIriOrAnyLiteral (RDF.int value)) state

        Turtle.ObjectLiteral (Turtle.LiteralDecimal value) ->
            addTriple (RDF.toBlankNodeOrIriOrAnyLiteral (RDF.literal (xsd "decimal") value)) state

        Turtle.ObjectLiteral (Turtle.LiteralDouble value) ->
            addTriple (RDF.toBlankNodeOrIriOrAnyLiteral (RDF.literal (xsd "double") (String.fromFloat value))) state

        Turtle.ObjectLiteral Turtle.LiteralTrue ->
            addTriple (RDF.toBlankNodeOrIriOrAnyLiteral (RDF.bool True)) state

        Turtle.ObjectLiteral Turtle.LiteralFalse ->
            addTriple (RDF.toBlankNodeOrIriOrAnyLiteral (RDF.bool False)) state


addTriple : BlankNodeOrIriOrAnyLiteral -> State -> Result Error State
addTriple object state =
    Result.map3 NTriple
        (Result.fromMaybe MissingSubject (List.head state.subjects))
        (Result.fromMaybe MissingPredicate (List.head state.predicates))
        (Ok object)
        |> Result.map (\nTriple -> { state | nTriples = nTriple :: state.nTriples })


addCollection : BlankNode -> List Turtle.Object -> State -> Result Error State
addCollection nodeFirst objects state =
    case List.reverse objects of
        [] ->
            addTriple (RDF.toBlankNodeOrIriOrAnyLiteral (rdf "nil")) state

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
                                            mintBlankNode stateNext.seed
                                    in
                                    { stateNext
                                        | subjects = RDF.toBlankNodeOrIri nodePrevious :: stateNext.subjects
                                        , predicates = rdf "first" :: stateNext.predicates
                                        , seed = seedNext
                                    }
                                        |> collectObject obj
                                        |> Result.map dropPredicate
                                        |> Result.map
                                            (\stateNextNext ->
                                                { stateNextNext | predicates = rdf "rest" :: stateNextNext.predicates }
                                            )
                                        |> Result.andThen (addTriple (RDF.toBlankNodeOrIriOrAnyLiteral nodeNext))
                                        |> Result.map dropPredicate
                                        |> Result.map dropSubject
                                        |> Result.map (Tuple.pair nodeNext)
                                )
                    )
                    (Ok ( nodeFirst, state ))
                |> Result.andThen
                    (\( nodePrevious, stateNext ) ->
                        { stateNext
                            | subjects = RDF.toBlankNodeOrIri nodePrevious :: stateNext.subjects
                            , predicates = rdf "first" :: stateNext.predicates
                        }
                            |> collectObject last
                            |> Result.map dropPredicate
                            |> Result.map
                                (\stateNextNext ->
                                    { stateNextNext | predicates = rdf "rest" :: stateNextNext.predicates }
                                )
                            |> Result.andThen (addTriple (RDF.toBlankNodeOrIriOrAnyLiteral (rdf "nil")))
                            |> Result.map dropPredicate
                            |> Result.map dropSubject
                    )


resolveIri : State -> Turtle.Iri -> Result Error String
resolveIri state iri =
    case iri of
        Turtle.IriRef url ->
            -- FIXME Check correctly if the url is relative
            if String.startsWith "http" url then
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
