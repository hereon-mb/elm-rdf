module RDF.Decode exposing
    ( Decoder
    , decode
    , Error(..), NodeType(..), errorToString
    , blankNode
    , blankNodeOrIri
    , bool
    , date
    , dateTime
    , iri
    , list, nonEmpty
    , literal
    , property
    , propertyPath
    , string
    , succeed
    , fail
    , map
    , map2
    , andThen
    , oneOf
    , many
    , hardcoded
    , optional
    , required
    , lazy
    )

{-| The `DefaultValue` API is a parser combinator for functions `Node -> a` that supports querying.

The `RDF` functions, say, `toIri`, are functions `Node -> a`, but do not allow for combination or querying.

The `RDF.Query` functions are functions `Graph -> a`, that do support querying (duh!), but no also combination.

So there _is_ some value there, but I think inlining the module out-of-existence might not be too bad of code duplication, either.

@docs Decoder
@docs decode
@docs Error, NodeType, errorToString

@docs blankNode
@docs blankNodeOrIri
@docs bool
@docs date
@docs dateTime
@docs iri
@docs list, nonEmpty
@docs literal
@docs property
@docs propertyPath
@docs string

@docs succeed
@docs fail

@docs map
@docs map2
@docs andThen
@docs oneOf
@docs many


# Pipeline

@docs hardcoded
@docs optional
@docs required


# Recursion

@docs lazy

-}

import Basics.Extra exposing (flip)
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Maybe.Extra as Maybe
import RDF exposing (BlankNodeOrIri, BlankNodeOrIriOrAnyLiteral, Iri)
import RDF.Graph exposing (Graph)
import RDF.Namespaces as RDF
import RDF.PropertyPath as RDF exposing (PropertyPath)
import RDF.Query as RDF
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


type Decoder a
    = Decoder (Graph -> Result Error (List BlankNodeOrIriOrAnyLiteral) -> Result Error a)


decode :
    Decoder a
    -> Graph
    -> List BlankNodeOrIriOrAnyLiteral
    -> Result Error a
decode (Decoder f) graph =
    f graph << Ok


type Error
    = InvalidDate BlankNodeOrIriOrAnyLiteral
    | InvalidDateTime BlankNodeOrIriOrAnyLiteral
    | UnexpectedLiteralDatatype Iri Iri
    | UnexpectedNode NodeType RDF.BlankNodeOrIriOrAnyLiteral
    | UnexpectedBool String
    | UnknownProperty BlankNodeOrIri PropertyPath
    | UnexpectedEmptyList
    | CustomError String
    | Batch (List Error)
    | TooManyNodes


errorToString : Error -> String
errorToString _ =
    "Could not decode RDF: TODO"


type NodeType
    = BlankNode
    | IriNode
    | LiteralNode


blankNode : Decoder a -> Decoder a
blankNode (Decoder f) =
    Decoder
        (\graph ->
            Result.andThen
                (\nodes ->
                    case nodes of
                        [ node ] ->
                            case RDF.toBlankNode node of
                                Just nodeNext ->
                                    Ok [ RDF.forgetCompatible nodeNext ]
                                        |> f graph

                                Nothing ->
                                    Err (UnexpectedNode BlankNode node)

                        _ ->
                            Err TooManyNodes
                )
        )


subject : Decoder RDF.BlankNode
subject =
    Decoder
        (\_ ->
            Result.andThen
                (\nodes ->
                    case nodes of
                        [ node ] ->
                            Maybe.unwrap (Err (UnexpectedNode BlankNode node)) Ok <|
                                RDF.toBlankNode node

                        _ ->
                            Err TooManyNodes
                )
        )


blankNodeOrIri : Decoder BlankNodeOrIri
blankNodeOrIri =
    oneOf
        [ map RDF.forgetCompatible iri
        , map RDF.forgetCompatible (blankNode subject)
        ]


bool : Decoder Bool
bool =
    literal (RDF.xsd "boolean")
        |> andThen
            (\stringLiteral ->
                case stringLiteral of
                    "true" ->
                        succeed True

                    "false" ->
                        succeed False

                    _ ->
                        fail (UnexpectedBool stringLiteral)
            )


date : Decoder Time.Posix
date =
    Decoder
        (\_ ->
            Result.andThen
                (\nodes ->
                    case nodes of
                        [ node ] ->
                            RDF.toDate node
                                |> Result.fromMaybe (InvalidDate node)

                        _ ->
                            Err TooManyNodes
                )
        )


dateTime : Decoder Time.Posix
dateTime =
    Decoder
        (\_ ->
            Result.andThen
                (\nodes ->
                    case nodes of
                        [ node ] ->
                            RDF.toDateTime node
                                |> Result.fromMaybe (InvalidDateTime node)

                        _ ->
                            Err TooManyNodes
                )
        )


iri : Decoder Iri
iri =
    Decoder
        (\_ ->
            Result.andThen
                (\nodes ->
                    case nodes of
                        [ node ] ->
                            node
                                |> RDF.toIri
                                |> Result.fromMaybe (UnexpectedNode IriNode node)

                        _ ->
                            Err TooManyNodes
                )
        )


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
                            Err TooManyNodes
                )
                >> Result.andThen
                    (\node ->
                        case node of
                            RDF.Node (RDF.BlankNode _) ->
                                Ok (RDF.forgetCompatible node)

                            RDF.Node (RDF.Iri _) ->
                                Err (UnexpectedNode BlankNode node)

                            RDF.Node (RDF.Literal _) ->
                                Err (UnexpectedNode BlankNode node)
                    )
                >> Result.andThen
                    (\focusNode ->
                        focusNode
                            |> RDF.objectToList graph
                            |> Maybe.withDefault []
                            |> List.map
                                (\nodeChild ->
                                    Ok [ nodeChild ]
                                        |> f graph
                                )
                            |> Result.combine
                    )
        )


nonEmpty : Decoder a -> Decoder (NonEmpty a)
nonEmpty =
    list
        >> andThen
            (NonEmpty.fromList
                >> Maybe.unwrap (fail UnexpectedEmptyList) succeed
            )


string : Decoder String
string =
    literal (RDF.xsd "string")


literal : Iri -> Decoder String
literal datatype =
    Decoder
        (\_ ->
            Result.andThen
                (\nodes ->
                    case nodes of
                        [ node ] ->
                            case node of
                                RDF.Node (RDF.BlankNode _) ->
                                    Err (UnexpectedNode LiteralNode node)

                                RDF.Node (RDF.Iri _) ->
                                    Err (UnexpectedNode LiteralNode node)

                                RDF.Node (RDF.Literal literalData) ->
                                    if literalData.datatype /= datatype then
                                        Err (UnexpectedLiteralDatatype datatype literalData.datatype)

                                    else
                                        Ok literalData.value

                        _ ->
                            Err TooManyNodes
                )
        )


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
                                    RDF.Node (RDF.BlankNode _) ->
                                        Ok (RDF.forgetCompatible node)

                                    RDF.Node (RDF.Iri _) ->
                                        Ok (RDF.forgetCompatible node)

                                    RDF.Node (RDF.Literal _) ->
                                        Err (UnexpectedNode BlankNode node)
                            )
                            nodes
                        )
                )
                >> Result.andThen
                    (\focusNodes ->
                        let
                            nodeChilds : Result Error (List RDF.BlankNodeOrIriOrAnyLiteral)
                            nodeChilds =
                                Result.map List.concat <|
                                    Result.combine
                                        (List.map
                                            (\focusNode ->
                                                case
                                                    RDF.emptyQuery
                                                        |> RDF.withSubject focusNode
                                                        |> RDF.withPropertyPath path
                                                        |> RDF.getObjects graph
                                                of
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


propertyPath : Decoder PropertyPath
propertyPath =
    list iri
        |> andThen
            (\irisOrNull ->
                case irisOrNull of
                    [] ->
                        fail (CustomError "expected property path, but got []")

                    iriFirst :: irisOther ->
                        succeed
                            (RDF.SequencePath
                                (RDF.PredicatePath iriFirst)
                                (List.map RDF.PredicatePath irisOther)
                            )
            )


succeed : a -> Decoder a
succeed x =
    Decoder (\_ _ -> Ok x)


map : (a -> b) -> Decoder a -> Decoder b
map f (Decoder g) =
    Decoder (\graph -> Result.map f << g graph)


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


map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 f g h =
    apply (map f g) h


apply : Decoder (a -> b) -> Decoder a -> Decoder b
apply (Decoder f) (Decoder g) =
    Decoder (\graph x -> Result.map2 (<|) (f graph x) (g graph x))


fail : Error -> Decoder a
fail e =
    Decoder (\_ _ -> Err e)


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


required : Decoder a -> Decoder (a -> b) -> Decoder b
required =
    flip apply


optional : Decoder a -> Decoder (Maybe a -> b) -> Decoder b
optional =
    required << try


hardcoded : a -> Decoder (a -> b) -> Decoder b
hardcoded =
    required << succeed


try : Decoder a -> Decoder (Maybe a)
try (Decoder f) =
    Decoder
        (\graph node ->
            case f graph node of
                Ok x ->
                    Ok (Just x)

                Err _ ->
                    Ok Nothing
        )


lazy : (() -> Decoder a) -> Decoder a
lazy f =
    andThen f (succeed ())
