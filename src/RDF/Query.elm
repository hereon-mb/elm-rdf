module RDF.Query exposing
    ( Query, emptyQuery
    , withSubject, withPredicate, withObject
    , withPropertyPath
    , exists
    , getSubjects, getIriSubjects
    , getSubject, getBlankNodeOrIriSubject, getIriSubject
    , getObjects, getBlankNodeOrIriObjects, getIriObjects, getInts, getFloats, getBools, getDates, getDateTimes, getListObjects, getListIriObjects, getListBlankNodeOrIriObjects
    , getObject, getBlankNodeOrIriObject, getIriObject, getAnyLiteralObject, getInt, getBool, getString, getDate, getDateTime, getPropertyPathObject, getStringOrLangString
    , rdfsLabelFor, rdfsCommentFor
    )

{-| TODO Add documentation

@docs Query, emptyQuery
@docs withSubject, withPredicate, withObject
@docs withPropertyPath

@docs exists
@docs getSubjects, getIriSubjects
@docs getSubject, getBlankNodeOrIriSubject, getIriSubject
@docs getObjects, getBlankNodeOrIriObjects, getIriObjects, getInts, getFloats, getBools, getDates, getDateTimes, getListObjects, getListIriObjects, getListBlankNodeOrIriObjects
@docs getObject, getBlankNodeOrIriObject, getIriObject, getAnyLiteralObject, getInt, getBool, getString, getDate, getDateTime, getPropertyPathObject, getStringOrLangString

@docs rdfsLabelFor, rdfsCommentFor

-}

import Dict exposing (Dict)
import List.Extra as List
import Maybe.Extra as Maybe
import RDF
    exposing
        ( AnyLiteral
        , BlankNodeOrIri
        , BlankNodeOrIriOrAnyLiteral
        , Iri
        , IsBlankNodeOrIri
        , IsIri
        , Node(..)
        , NodeInternal(..)
        , StringOrLangString(..)
        , forgetCompatible
        , serializeNode
        , serializeNodeHelp
        , toAnyLiteral
        , toBool
        , toDate
        , toDateTime
        , toIri
        , toLangString
        , toString
        , unwrap
        )
import RDF.Graph exposing (Graph(..), GraphData)
import RDF.Namespaces exposing (rdf, rdfs, sh, xsd)
import RDF.PropertyPath exposing (PropertyPath(..))
import Time exposing (Posix)


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
        subjectMatches : RDF.NTriple -> Bool
        subjectMatches triple =
            case query.subject of
                Nothing ->
                    True

                Just subject ->
                    triple.subject == subject

        predicateMatches : RDF.NTriple -> Bool
        predicateMatches triple =
            case query.propertyPath of
                Just (PredicatePath predicate) ->
                    triple.predicate == predicate

                Just _ ->
                    False

                Nothing ->
                    True

        objectMatches : RDF.NTriple -> Bool
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
            filterByObject : List RDF.NTriple -> List RDF.NTriple
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
        toBlankNodeOrIriSafe : Node compatible -> Maybe BlankNodeOrIri
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
        onlyBlankNodeOrIri : Node compatbiel -> Maybe BlankNodeOrIri
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
        toBlankNodeOrIriSafe : Node compatible -> Maybe BlankNodeOrIri
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
        onlyInt : Node compatible -> Maybe Int
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
        onlyFloat : Node compatible -> Maybe Float
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
getStringOrLangString : Graph -> Query -> Maybe StringOrLangString
getStringOrLangString graph query =
    let
        getStringOrLangStringHelp : List (Node compatible) -> Maybe StringOrLangString
        getStringOrLangStringHelp objects =
            let
                maybeString : Maybe String
                maybeString =
                    objects
                        |> List.filterMap toString
                        |> List.head

                langStrings : Dict String String
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
