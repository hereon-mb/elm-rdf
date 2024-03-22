module RDF exposing
    ( Iri, BlankNode, Literal, LiteralData
    , BlankNodeOrIri, BlankNodeOrIriOrAnyLiteral, AnyLiteral
    , IsBlankNodeOrIri, IsIri
    , NTriple
    , Node(..), Yes, No, NodeInternal(..)
    , forgetCompatible
    , blankNode, iriAbsolute, literalWithDatatype, literal, bool, string, int
    , dash, dcterms, owl, qudt, rdf, rdfs, sh, xsd, a, prov
    , toIri, toUrl, toBlankNodeOrIri, toString, toLangString, toInt, toFloat, toDecimal, toDate, toDateTime
    , serialize, serializeNode
    , Graph(..), GraphData
    , union
    , emptyGraph, singleton, fromNTriples
    , decoder, encode
    , parse, Error
    , Seed, initialSeed
    , insert, insertAt, generateBlankNode
    , isEmpty
    , Query, emptyQuery
    , withSubject, withPredicate, withObject
    , PropertyPath(..), withPropertyPath, serializePropertyPath
    , exists
    , getSubjects, getIriSubjects
    , getSubject, getBlankNodeOrIriSubject, getIriSubject
    , getObjects, getBlankNodeOrIriObjects, getIriObjects, getInts, getFloats, getBools, getDates, getDateTimes, getListObjects, getListIriObjects, getListBlankNodeOrIriObjects
    , getObject, getBlankNodeOrIriObject, getIriObject, getAnyLiteralObject, getInt, getBool, getString, getDate, getDateTime, getPropertyPathObject
    , toValue
    , rdfsLabelFor, rdfsCommentFor
    , StringOrLangString, localize, nonLocalized, getStringOrLangString, stringOrLangStringFrom, stringOrLangStringFromList, mergeStringOrLangStrings
    )

{-|


# Node

@docs Iri, BlankNode, Literal, LiteralData
@docs BlankNodeOrIri, BlankNodeOrIriOrAnyLiteral, AnyLiteral
@docs IsBlankNodeOrIri, IsIri
@docs NTriple
@docs Node, Yes, No, NodeInternal
@docs forgetCompatible


## Create

@docs blankNode, iriAbsolute, literalWithDatatype, literal, bool, string, int


### Namespaces

@docs dash, dcterms, owl, qudt, rdf, rdfs, sh, xsd, a, prov


## Transform

@docs toIri, toUrl, toBlankNodeOrIri, toString, toLangString, toInt, toFloat, toDecimal, toDate, toDateTime


## Serialize

@docs serialize, serializeNode


# Graph

@docs Graph, GraphData
@docs union


## Create

@docs emptyGraph, singleton, fromNTriples
@docs decoder, encode
@docs parse, Error


## Update

@docs Seed, initialSeed
@docs insert, insertAt, generateBlankNode


## Retrieve

@docs isEmpty


## Filter

@docs Query, emptyQuery
@docs withSubject, withPredicate, withObject
@docs PropertyPath, withPropertyPath, serializePropertyPath


## Query

@docs exists
@docs getSubjects, getIriSubjects
@docs getSubject, getBlankNodeOrIriSubject, getIriSubject
@docs getObjects, getBlankNodeOrIriObjects, getIriObjects, getInts, getFloats, getBools, getDates, getDateTimes, getListObjects, getListIriObjects, getListBlankNodeOrIriObjects
@docs getObject, getBlankNodeOrIriObject, getIriObject, getAnyLiteralObject, getInt, getBool, getString, getDate, getDateTime, getPropertyPathObject
@docs toValue

@docs rdfsLabelFor, rdfsCommentFor
@docs StringOrLangString, localize, nonLocalized, getStringOrLangString, stringOrLangStringFrom, stringOrLangStringFromList, mergeStringOrLangStrings

-}

import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import Maybe.Extra as Maybe
import Parser exposing ((|.), (|=), Parser)
import Random
import Set
import Time exposing (Posix)
import Tuple.Extra as Tuple
import UUID


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


unwrap : Node compatible -> NodeInternal
unwrap (Node node) =
    node


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
type Query
    = Query QueryData


type alias QueryData =
    { subject : Maybe BlankNodeOrIri
    , propertyPath : Maybe PropertyPath
    , object : Maybe BlankNodeOrIriOrAnyLiteral
    }


{-| TODO Add documentation
-}
emptyQuery : Query
emptyQuery =
    Query
        { subject = Nothing
        , propertyPath = Nothing
        , object = Nothing
        }


{-| TODO Add documentation
-}
type PropertyPath
    = PredicatePath Iri
    | SequencePath PropertyPath (List PropertyPath)
    | AlternativePath PropertyPath (List PropertyPath)
    | InversePath PropertyPath
    | ZeroOrMorePath PropertyPath
    | OneOrMorePath PropertyPath
    | ZeroOrOnePath PropertyPath


{-| TODO Add documentation
-}
serializePropertyPath : PropertyPath -> String
serializePropertyPath propertyPath =
    case propertyPath of
        PredicatePath iri ->
            serializeNode iri

        SequencePath first rest ->
            (serializePropertyPath first :: List.map serializePropertyPath rest)
                |> String.join "/"

        AlternativePath first rest ->
            (serializePropertyPath first :: List.map serializePropertyPath rest)
                |> String.join "|"

        InversePath nested ->
            "^" ++ serializePropertyPath nested

        ZeroOrMorePath nested ->
            serializePropertyPath nested ++ "*"

        OneOrMorePath nested ->
            serializePropertyPath nested ++ "+"

        ZeroOrOnePath nested ->
            serializePropertyPath nested ++ "?"


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
singleton : IsBlankNodeOrIri compatible1 -> IsIri compatible2 -> Node compatible3 -> Graph
singleton subject predicate object =
    fromNTriples
        [ NTriple
            (forgetCompatible subject)
            (forgetCompatible predicate)
            (forgetCompatible object)
        ]


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
            case
                emptyQuery
                    |> withSubject subject
                    |> withPredicate predicate
                    |> getBlankNodeOrIriObject graph
            of
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



-- QUERY


{-| TODO Add documentation
-}
withSubject : IsBlankNodeOrIri compatible -> Query -> Query
withSubject node (Query query) =
    Query { query | subject = Just (forgetCompatible node) }


{-| TODO Add documentation
-}
withPredicate : IsIri compatible -> Query -> Query
withPredicate node (Query query) =
    Query { query | propertyPath = Just (PredicatePath (forgetCompatible node)) }


{-| TODO Add documentation
-}
withPropertyPath : PropertyPath -> Query -> Query
withPropertyPath propertyPath (Query data) =
    Query { data | propertyPath = Just propertyPath }


{-| TODO Add documentation
-}
withObject : Node compatible -> Query -> Query
withObject node (Query query) =
    Query { query | object = Just (forgetCompatible node) }


{-| Resturns if any matching triple exists.
-}
exists : Graph -> Query -> Bool
exists (Graph graph) (Query query) =
    let
        subjectMatches triple =
            case query.subject of
                Nothing ->
                    True

                Just subject ->
                    triple.subject == subject

        predicateMatches triple =
            case query.propertyPath of
                Just (PredicatePath predicate) ->
                    triple.predicate == predicate

                Just _ ->
                    False

                Nothing ->
                    True

        objectMatches triple =
            case query.object of
                Nothing ->
                    True

                Just object ->
                    triple.object == object
    in
    graph.nTriples
        |> List.filter objectMatches
        |> List.filter predicateMatches
        |> List.filter subjectMatches
        |> List.isEmpty
        |> not


{-| Return the subjects of all remaining triples.
-}
getSubjects : Graph -> Query -> List BlankNodeOrIri
getSubjects (Graph graph) (Query query) =
    List.unique
        (let
            filterByObject result =
                case query.object of
                    Nothing ->
                        result

                    Just object ->
                        List.filter (\nTriple -> nTriple.object == object) result
         in
         if query.subject == Nothing then
            case query.propertyPath of
                Just (PredicatePath iriPredicate) ->
                    graph.byPredicateBySubject
                        |> Dict.get (serializeNode iriPredicate)
                        |> Maybe.withDefault Dict.empty
                        |> Dict.values
                        |> List.concat
                        |> filterByObject
                        |> List.map .subject

                _ ->
                    graph.nTriples
                        |> filterByObject
                        |> List.map .subject

         else
            []
        )


{-| TODO Add documentation
-}
getIriSubjects : Graph -> Query -> List Iri
getIriSubjects graph query =
    query
        |> getSubjects graph
        |> List.filterMap toIri


{-| If there is only one triple left, return it's subject. Otherwise return
`Nothing`.
-}
getSubject : Graph -> Query -> Maybe BlankNodeOrIri
getSubject graph query =
    case query |> getSubjects graph of
        [ subject ] ->
            Just subject

        _ ->
            Nothing


{-| TODO Add documentation
-}
getBlankNodeOrIriSubject : Graph -> Query -> Maybe BlankNodeOrIri
getBlankNodeOrIriSubject graph query =
    let
        toBlankNodeOrIriSafe (Node node) =
            case node of
                BlankNode _ ->
                    Just (Node node)

                Iri _ ->
                    Just (Node node)

                Literal _ ->
                    Nothing
    in
    query
        |> getSubject graph
        |> Maybe.andThen toBlankNodeOrIriSafe


{-| TODO Add documentation
-}
getIriSubject : Graph -> Query -> Maybe Iri
getIriSubject graph query =
    query
        |> getSubject graph
        |> Maybe.andThen toIri


{-| Return the objects of all remaining triples.
-}
getObjects : Graph -> Query -> List BlankNodeOrIriOrAnyLiteral
getObjects (Graph graph) (Query query) =
    List.unique
        (if query.object == Nothing then
            case query.subject of
                Nothing ->
                    case query.propertyPath of
                        Just propertyPath ->
                            List.concatMap (unwrap >> followPropertyPath graph propertyPath >> List.map Node) graph.subjects

                        Nothing ->
                            graph.objects

                Just subject ->
                    case query.propertyPath of
                        Just propertyPath ->
                            List.map Node (followPropertyPath graph propertyPath (unwrap subject))

                        Nothing ->
                            graph.bySubjectByPredicate
                                |> Dict.get (serializeNode subject)
                                |> Maybe.withDefault Dict.empty
                                |> Dict.values
                                |> List.concat
                                |> List.map .object

         else
            []
        )


followPropertyPath : GraphData -> PropertyPath -> NodeInternal -> List NodeInternal
followPropertyPath data propertyPath subject =
    case propertyPath of
        PredicatePath iri ->
            data.bySubjectByPredicate
                |> Dict.get (serializeNodeHelp subject)
                |> Maybe.withDefault Dict.empty
                |> Dict.get (serializeNode iri)
                |> Maybe.withDefault []
                |> List.map (.object >> unwrap)

        SequencePath first rest ->
            List.foldl
                (\next -> List.concatMap (followPropertyPath data next))
                (followPropertyPath data first subject)
                rest

        _ ->
            []


{-| TODO Add documentation
-}
getBlankNodeOrIriObjects : Graph -> Query -> List BlankNodeOrIri
getBlankNodeOrIriObjects graph query =
    let
        onlyBlankNodeOrIri (Node node) =
            case node of
                BlankNode _ ->
                    Just (Node node)

                Iri _ ->
                    Just (Node node)

                Literal _ ->
                    Nothing
    in
    query
        |> getObjects graph
        |> List.filterMap onlyBlankNodeOrIri


{-| TODO Add documentation
-}
getIriObjects : Graph -> Query -> List Iri
getIriObjects graph query =
    query
        |> getObjects graph
        |> List.filterMap toIri


{-| If there is only one triple left, return it's object. Otherwise return
`Nothing`.
-}
getObject : Graph -> Query -> Maybe BlankNodeOrIriOrAnyLiteral
getObject graph query =
    case query |> getObjects graph of
        [ object ] ->
            Just object

        _ ->
            Nothing


{-| TODO Add documentation
-}
getBlankNodeOrIriObject : Graph -> Query -> Maybe BlankNodeOrIri
getBlankNodeOrIriObject graph query =
    let
        toBlankNodeOrIriSafe (Node node) =
            case node of
                BlankNode _ ->
                    Just (Node node)

                Iri _ ->
                    Just (Node node)

                Literal _ ->
                    Nothing
    in
    query
        |> getObject graph
        |> Maybe.andThen toBlankNodeOrIriSafe


{-| TODO Add documentation
-}
getIriObject : Graph -> Query -> Maybe Iri
getIriObject graph query =
    query
        |> getObject graph
        |> Maybe.andThen toIri


{-| TODO Add documentation
-}
getAnyLiteralObject : Graph -> Query -> Maybe AnyLiteral
getAnyLiteralObject graph query =
    query
        |> getObject graph
        |> Maybe.andThen toAnyLiteral


{-| TODO Add documentation
-}
getPropertyPathObject : Graph -> Query -> Maybe PropertyPath
getPropertyPathObject graph query =
    query
        |> getBlankNodeOrIriObject graph
        |> Maybe.andThen (objectToPropertyPath graph)


objectToPropertyPath : Graph -> BlankNodeOrIri -> Maybe PropertyPath
objectToPropertyPath graph object =
    [ objectToPredicatePath object
    , objectToSequencePath graph object
    , objectToAlternativePath graph object
    , objectToInversePath graph object
    , objectToZeroOrMorePath graph object
    , objectToOneOrMorePath graph object
    , objectToZeroOrOnePath graph object
    ]
        |> Maybe.orList


objectToPredicatePath : BlankNodeOrIri -> Maybe PropertyPath
objectToPredicatePath object =
    toIri object
        |> Maybe.map PredicatePath


objectToSequencePath : Graph -> BlankNodeOrIri -> Maybe PropertyPath
objectToSequencePath graph object =
    objectToListBlankNodeOrIri graph object
        |> Maybe.andThen (objectListToSequencePath graph)


objectListToSequencePath : Graph -> List BlankNodeOrIri -> Maybe PropertyPath
objectListToSequencePath graph objects =
    case objects of
        [] ->
            Nothing

        first :: rest ->
            Maybe.map2 SequencePath
                (objectToPropertyPath graph first)
                (rest
                    |> List.map (objectToPropertyPath graph)
                    |> Maybe.combine
                )


objectToAlternativePath : Graph -> BlankNodeOrIri -> Maybe PropertyPath
objectToAlternativePath graph object =
    emptyQuery
        |> withSubject object
        |> withPredicate (sh "alternativePath")
        |> getBlankNodeOrIriObject graph
        |> Maybe.andThen (objectToListBlankNodeOrIri graph)
        |> Maybe.andThen (objectListToAlternativePath graph)


objectListToAlternativePath : Graph -> List BlankNodeOrIri -> Maybe PropertyPath
objectListToAlternativePath triples list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            Maybe.map2 AlternativePath
                (objectToPropertyPath triples first)
                (rest
                    |> List.map (objectToPropertyPath triples)
                    |> Maybe.combine
                )


objectToInversePath : Graph -> BlankNodeOrIri -> Maybe PropertyPath
objectToInversePath graph object =
    emptyQuery
        |> withSubject object
        |> withPredicate (sh "inversePath")
        |> getBlankNodeOrIriObject graph
        |> Maybe.andThen (objectToPropertyPath graph)
        |> Maybe.map InversePath


objectToZeroOrMorePath : Graph -> BlankNodeOrIri -> Maybe PropertyPath
objectToZeroOrMorePath graph object =
    emptyQuery
        |> withSubject object
        |> withPredicate (sh "zeroOrMorePath")
        |> getBlankNodeOrIriObject graph
        |> Maybe.andThen (objectToPropertyPath graph)
        |> Maybe.map ZeroOrMorePath


objectToOneOrMorePath : Graph -> BlankNodeOrIri -> Maybe PropertyPath
objectToOneOrMorePath graph object =
    emptyQuery
        |> withSubject object
        |> withPredicate (sh "oneOrMorePath")
        |> getBlankNodeOrIriObject graph
        |> Maybe.andThen (objectToPropertyPath graph)
        |> Maybe.map OneOrMorePath


objectToZeroOrOnePath : Graph -> BlankNodeOrIri -> Maybe PropertyPath
objectToZeroOrOnePath graph object =
    emptyQuery
        |> withSubject object
        |> withPredicate (sh "zeroOrOnePath")
        |> getBlankNodeOrIriObject graph
        |> Maybe.andThen (objectToPropertyPath graph)
        |> Maybe.map ZeroOrOnePath


{-| TODO Add documentation
-}
getBools : Graph -> Query -> List Bool
getBools graph query =
    query
        |> getObjects graph
        |> List.filterMap toBool


{-| TODO Add documentation
-}
getBool : Graph -> Query -> Maybe Bool
getBool graph query =
    query
        |> getObject graph
        |> Maybe.andThen toBool


{-| TODO Add documentation
-}
getString : Graph -> Query -> Maybe String
getString graph query =
    query
        |> getObject graph
        |> Maybe.andThen toString


{-| TODO Add documentation
-}
getDates : Graph -> Query -> List Posix
getDates graph query =
    query
        |> getObjects graph
        |> List.filterMap toDate


{-| TODO Add documentation
-}
getDate : Graph -> Query -> Maybe Posix
getDate graph query =
    query
        |> getObject graph
        |> Maybe.andThen toDate


{-| TODO Add documentation
-}
getDateTimes : Graph -> Query -> List Posix
getDateTimes graph query =
    query
        |> getObjects graph
        |> List.filterMap toDateTime


{-| TODO Add documentation
-}
getDateTime : Graph -> Query -> Maybe Posix
getDateTime graph query =
    query
        |> getObject graph
        |> Maybe.andThen toDateTime


{-| TODO Add documentation
-}
getInts : Graph -> Query -> List Int
getInts graph query =
    let
        onlyInt (Node node) =
            case node of
                BlankNode _ ->
                    Nothing

                Iri _ ->
                    Nothing

                Literal data ->
                    if data.datatype == xsd "integer" then
                        String.toInt data.value

                    else if data.datatype == xsd "int" then
                        String.toInt data.value

                    else
                        Nothing
    in
    query
        |> getObjects graph
        |> List.filterMap onlyInt


{-| TODO Add documentation
-}
getInt : Graph -> Query -> Maybe Int
getInt graph query =
    List.head (getInts graph query)


{-| TODO Add documentation
-}
getFloats : Graph -> Query -> List Float
getFloats graph query =
    let
        onlyFloat (Node node) =
            case node of
                BlankNode _ ->
                    Nothing

                Iri _ ->
                    Nothing

                Literal data ->
                    if
                        (data.datatype == xsd "float")
                            || (data.datatype == xsd "double")
                            || (data.datatype == xsd "decimal")
                    then
                        String.toFloat data.value

                    else
                        Nothing
    in
    query
        |> getObjects graph
        |> List.filterMap onlyFloat


{-| TODO Add documentation
-}
getListObjects : Graph -> Query -> List (List BlankNodeOrIriOrAnyLiteral)
getListObjects graph query =
    query
        |> getBlankNodeOrIriObjects graph
        |> List.filterMap (objectToList graph)


{-| TODO Add documentation
-}
objectToList : Graph -> BlankNodeOrIri -> Maybe (List BlankNodeOrIriOrAnyLiteral)
objectToList graph object =
    if toIri object == Just (rdf "nil") then
        Just []

    else
        Maybe.map2
            (\first rest ->
                first :: rest
            )
            (emptyQuery
                |> withSubject object
                |> withPredicate (rdf "first")
                |> getObject graph
            )
            (emptyQuery
                |> withSubject object
                |> withPredicate (rdf "rest")
                |> getBlankNodeOrIriObject graph
                |> Maybe.andThen (objectToList graph)
            )


{-| TODO Add documentation
-}
getListIriObjects : Graph -> Query -> List (List Iri)
getListIriObjects graph query =
    query
        |> getBlankNodeOrIriObjects graph
        |> List.filterMap (objectToListIri graph)


{-| TODO Add documentation
-}
getListBlankNodeOrIriObjects : Graph -> Query -> List (List BlankNodeOrIri)
getListBlankNodeOrIriObjects graph query =
    query
        |> getBlankNodeOrIriObjects graph
        |> List.filterMap (objectToListBlankNodeOrIri graph)


{-| TODO Add documentation
-}
objectToListIri : Graph -> BlankNodeOrIri -> Maybe (List Iri)
objectToListIri graph object =
    if toIri object == Just (rdf "nil") then
        Just []

    else
        Maybe.map2
            (\first rest ->
                first :: rest
            )
            (emptyQuery
                |> withSubject object
                |> withPredicate (rdf "first")
                |> getIriObject graph
            )
            (emptyQuery
                |> withSubject object
                |> withPredicate (rdf "rest")
                |> getBlankNodeOrIriObject graph
                |> Maybe.andThen (objectToListIri graph)
            )


{-| TODO Add documentation
-}
objectToListBlankNodeOrIri : Graph -> BlankNodeOrIri -> Maybe (List BlankNodeOrIri)
objectToListBlankNodeOrIri graph object =
    if toIri object == Just (rdf "nil") then
        Just []

    else
        Maybe.map2
            (\first rest ->
                first :: rest
            )
            (emptyQuery
                |> withSubject object
                |> withPredicate (rdf "first")
                |> getBlankNodeOrIriObject graph
            )
            (emptyQuery
                |> withSubject object
                |> withPredicate (rdf "rest")
                |> getBlankNodeOrIriObject graph
                |> Maybe.andThen (objectToListBlankNodeOrIri graph)
            )


{-| TODO Add documentation
-}
rdfsLabelFor : Graph -> IsBlankNodeOrIri compatible -> Maybe StringOrLangString
rdfsLabelFor graph blankNodeOrIri =
    emptyQuery
        |> withSubject blankNodeOrIri
        |> withPredicate (rdfs "label")
        |> getStringOrLangString graph


{-| TODO Add documentation
-}
rdfsCommentFor : Graph -> IsBlankNodeOrIri compatible -> Maybe StringOrLangString
rdfsCommentFor graph blankNodeOrIri =
    emptyQuery
        |> withSubject blankNodeOrIri
        |> withPredicate (rdfs "comment")
        |> getStringOrLangString graph


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
getStringOrLangString : Graph -> Query -> Maybe StringOrLangString
getStringOrLangString graph query =
    let
        getStringOrLangStringHelp objects =
            let
                maybeString =
                    objects
                        |> List.filterMap toString
                        |> List.head

                langStrings =
                    objects
                        |> List.filterMap toLangString
                        |> Dict.fromList
            in
            if maybeString == Nothing && Dict.isEmpty langStrings then
                Nothing

            else
                { string = maybeString
                , langStrings = langStrings
                }
                    |> StringOrLangString
                    |> Just
    in
    query
        |> getObjects graph
        |> getStringOrLangStringHelp


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
decoder : Decoder Graph
decoder =
    Decode.list nTripleDecoder
        |> Decode.map fromNTriples


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
encode : Graph -> Value
encode (Graph data) =
    data.bySubjectByPredicate
        |> Dict.values
        |> List.concatMap Dict.values
        |> List.concat
        |> Encode.list encodeNTriple


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
parse : String -> Result (List Error) Graph
parse raw =
    raw
        |> parseNTriples
        |> Result.map fromNTriples


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
