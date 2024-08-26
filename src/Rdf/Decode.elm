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
    , from
    , blankNode
    , predicate, property
    , list, nonEmpty
    , at
    , decode
    , Error(..), NodeType(..), errorToString
    , map
    , map2
    , combine
    , required, requiredAt
    , optional, optionalAt
    , hardcoded
    , custom
    , succeed, fail
    , andThen
    , oneOf
    , many
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

@docs from
@docs blankNode
@docs predicate, property
@docs list, nonEmpty
@docs at


# Running Decoders

@docs decode
@docs Error, NodeType, errorToString


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

@docs succeed, fail
@docs andThen
@docs oneOf
@docs many
@docs lazy

-}

import Basics.Extra exposing (flip)
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Maybe.Extra as Maybe
import Rdf exposing (BlankNodeOrIri, BlankNodeOrIriOrAnyLiteral, Iri, IsBlankNodeOrIri, IsIri)
import Rdf.Graph exposing (Graph)
import Rdf.Namespaces as Rdf
import Rdf.PropertyPath as Rdf exposing (PropertyPath)
import Rdf.Query as Rdf
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
                (Rdf.emptyQuery
                    |> Rdf.withSubject nodeFocus
                    |> Rdf.exists graph
                )
                    || (Rdf.emptyQuery
                            |> Rdf.withObject nodeFocus
                            |> Rdf.exists graph
                       )
            then
                f graph (Ok [ Rdf.asBlankNodeOrIriOrAnyLiteral nodeFocus ])

            else
                Err (NodeDoesNotExist (Rdf.asBlankNodeOrIri nodeFocus))
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
    | UnexpectedLiteralDatatype Iri Iri
    | UnexpectedNode NodeType Rdf.BlankNodeOrIriOrAnyLiteral
    | UnexpectedBool String
    | UnexpectedInt String
    | UnexpectedFloat String
    | UnknownProperty BlankNodeOrIri PropertyPath
    | UnexpectedEmptyList
    | CustomError String
    | Batch (List Error)
    | TooManyNodes (List Rdf.BlankNodeOrIriOrAnyLiteral)
    | MissingLangString Rdf.AnyLiteral
    | TooManyStrings (List String)
    | NodeDoesNotExist BlankNodeOrIri
    | NoFocusNode


{-| Turn a decoding error into a human friendly string. E.g.

    errorToString (UnexpectedBool "42")
    --> "Expected a boolean, but found 42."

-}
errorToString : Error -> String
errorToString error_ =
    case error_ of
        InvalidDate _ ->
            "Not a date"

        InvalidDateTime _ ->
            "Not a date time"

        UnexpectedLiteralDatatype datatypeExpected datatypeFound ->
            "Expected a literal of type " ++ Rdf.toUrl datatypeExpected ++ ", but found a literal of type " ++ Rdf.toUrl datatypeFound ++ "."

        UnexpectedNode BlankNode nodeFound ->
            "Expected a blank node, but found " ++ Rdf.serializeNode nodeFound ++ "."

        UnexpectedNode IriNode nodeFound ->
            "Expected an IRI, but found " ++ Rdf.serializeNode nodeFound ++ "."

        UnexpectedNode LiteralNode nodeFound ->
            "Expected a literal, but found " ++ Rdf.serializeNode nodeFound ++ "."

        UnexpectedBool found ->
            "Expected a boolean, but found " ++ found ++ "."

        UnexpectedInt found ->
            "Expected an integer, but found " ++ found ++ "."

        UnexpectedFloat found ->
            "Expected a float, but found " ++ found ++ "."

        UnknownProperty nodeFocus pathExpected ->
            "No such property " ++ Rdf.serializePropertyPath pathExpected ++ " found at " ++ Rdf.serializeNode nodeFocus ++ "."

        UnexpectedEmptyList ->
            "Expected a non-empty list, but found an empty list."

        CustomError s ->
            s

        Batch errors ->
            "Decoding failed in one of the following ways:"
                ++ (String.join "\n" << List.map (\item -> "  - " ++ item))
                    (List.map errorToString errors)

        TooManyNodes nodesFound ->
            "I expected a single node, but I found multiple nodes" ++ String.join ", " (List.map Rdf.serializeNode nodesFound) ++ "."

        MissingLangString nodeFound ->
            "I expected a lang string, but I found " ++ Rdf.serializeNode nodeFound ++ "."

        TooManyStrings stringsFound ->
            "I expected a single string, but I found multiple strings " ++ String.join ", " stringsFound ++ "."

        NodeDoesNotExist node ->
            "The node " ++ Rdf.serializeNode node ++ " does not exist."

        NoFocusNode ->
            "There is no focus node to start decoding from."


{-| -}
type NodeType
    = BlankNode
    | IriNode
    | LiteralNode


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
                                    Err (UnexpectedNode BlankNode node)

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
                                (error (UnexpectedNode BlankNode node))
                                succeed
                                (Rdf.toBlankNode node)
                        )
                )
            )
        ]


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
                                (error (UnexpectedNode BlankNode node))
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
                        error (UnexpectedBool stringLiteral)
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
                        error (UnexpectedInt intLiteral)

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
                        error (UnexpectedFloat floatLiteral)

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
                            node
                                |> Rdf.toIri
                                |> Result.fromMaybe (UnexpectedNode IriNode node)

                        _ ->
                            Err (TooManyNodes nodes)
                )
        )


{-| TODO
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
                            Rdf.Node (Rdf.BlankNode data) ->
                                Ok (Rdf.Node (Rdf.BlankNode data))

                            Rdf.Node (Rdf.Iri _) ->
                                Err (UnexpectedNode BlankNode node)

                            Rdf.Node (Rdf.Literal _) ->
                                Err (UnexpectedNode BlankNode node)
                    )
                >> Result.andThen
                    (\focusNode ->
                        focusNode
                            |> Rdf.objectToList graph
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
                Maybe.unwrap (error (MissingLangString (Rdf.Node (Rdf.Literal node))))
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


literalData : Iri -> Decoder Rdf.LiteralData
literalData datatype =
    Decoder
        (\_ ->
            Result.andThen
                (\nodes ->
                    case nodes of
                        [ node ] ->
                            case node of
                                Rdf.Node (Rdf.BlankNode _) ->
                                    Err (UnexpectedNode LiteralNode node)

                                Rdf.Node (Rdf.Iri _) ->
                                    Err (UnexpectedNode LiteralNode node)

                                Rdf.Node (Rdf.Literal literalData_) ->
                                    if literalData_.datatype /= datatype then
                                        Err (UnexpectedLiteralDatatype datatype literalData_.datatype)

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
                                Rdf.Node (Rdf.BlankNode _) ->
                                    Err (UnexpectedNode LiteralNode node)

                                Rdf.Node (Rdf.Iri _) ->
                                    Err (UnexpectedNode LiteralNode node)

                                Rdf.Node (Rdf.Literal literalData_) ->
                                    Ok (Rdf.Node (Rdf.Literal literalData_))

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
                                    Rdf.Node (Rdf.BlankNode data) ->
                                        Ok (Rdf.Node (Rdf.BlankNode data))

                                    Rdf.Node (Rdf.Iri data) ->
                                        Ok (Rdf.Node (Rdf.Iri data))

                                    Rdf.Node (Rdf.Literal _) ->
                                        Err (UnexpectedNode BlankNode node)
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
                                                case
                                                    Rdf.emptyQuery
                                                        |> Rdf.withSubject focusNode
                                                        |> Rdf.withPropertyPath path
                                                        |> Rdf.getObjects graph
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


{-| TODO
-}
predicate : IsIri compatible -> Decoder a -> Decoder a
predicate =
    property << Rdf.PredicatePath << Rdf.asIri


{-| A decoder which always succeeds with the given value.
-}
succeed : a -> Decoder a
succeed x =
    Decoder (\_ _ -> Ok x)


{-| A decoder which always fails with the given message.
-}
fail : String -> Decoder a
fail =
    error << CustomError


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
