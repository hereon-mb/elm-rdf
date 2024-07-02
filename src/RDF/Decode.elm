module RDF.Decode exposing
    ( Decoder
    , decode
    , Error(..), NodeType(..), errorToString
    , bool
    , date
    , dateTime
    , iri
    , list
    , literal
    , property
    , string
    , succeed
    , fail
    , map
    , map2
    , andThen
    )

{-| The `DefaultValue` API is a parser combinator for functions `Node -> a` that supports querying.

The `RDF` functions, say, `toIri`, are functions `Node -> a`, but do not allow for combination or querying.

The `RDF.Query` functions are functions `Graph -> a`, that do support querying (duh!), but no also combination.

So there _is_ some value there, but I think inlining the module out-of-existence might not be too bad of code duplication, either.

@docs Decoder
@docs decode
@docs Error, NodeType, errorToString

@docs bool
@docs date
@docs dateTime
@docs iri
@docs list
@docs literal
@docs property
@docs string

@docs succeed
@docs fail

@docs map
@docs map2
@docs andThen

-}

import List.NonEmpty exposing (NonEmpty)
import RDF exposing (BlankNodeOrIri, BlankNodeOrIriOrAnyLiteral, Iri)
import RDF.Graph exposing (Graph)
import RDF.Namespaces exposing (xsd)
import RDF.PropertyPath exposing (PropertyPath)
import RDF.Query as RDF
import Result.Extra as Result
import Time


type Decoder a
    = Decoder (Graph -> Result Error BlankNodeOrIriOrAnyLiteral -> Result Error a)


decode :
    Decoder a
    -> Graph
    -> BlankNodeOrIriOrAnyLiteral
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
    | UnexpectedUnit (NonEmpty Iri) Iri


errorToString : Error -> String
errorToString _ =
    "Could not decode RDF: TODO"


type NodeType
    = BlankNode
    | IriNode
    | LiteralNode


bool : Decoder Bool
bool =
    literal (xsd "boolean")
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
                (\node ->
                    RDF.toDate node
                        |> Result.fromMaybe (InvalidDate node)
                )
        )


dateTime : Decoder Time.Posix
dateTime =
    Decoder
        (\_ ->
            Result.andThen
                (\node ->
                    RDF.toDateTime node
                        |> Result.fromMaybe (InvalidDateTime node)
                )
        )


iri : Decoder Iri
iri =
    Decoder
        (\_ ->
            Result.andThen
                (\node ->
                    node
                        |> RDF.toIri
                        |> Result.fromMaybe (UnexpectedNode IriNode node)
                )
        )


list : Decoder a -> Decoder (List a)
list (Decoder f) =
    Decoder
        (\graph ->
            Result.andThen
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
                                    Ok nodeChild
                                        |> f graph
                                )
                            |> Result.combine
                    )
        )


string : Decoder String
string =
    literal (xsd "string")


literal : Iri -> Decoder String
literal datatype =
    Decoder
        (\_ ->
            Result.andThen
                (\node ->
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
                )
        )


property : PropertyPath -> Decoder a -> Decoder a
property path (Decoder f) =
    Decoder
        (\graph ->
            Result.andThen
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
                        case
                            RDF.emptyQuery
                                |> RDF.withSubject focusNode
                                |> RDF.withPropertyPath path
                                |> RDF.getObject graph
                        of
                            Nothing ->
                                Err (UnknownProperty focusNode path)

                            Just nodeChild ->
                                Ok nodeChild
                                    |> f graph
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
