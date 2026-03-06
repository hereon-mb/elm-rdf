module Rdf.Sparql exposing
    ( Construct, construct, from, constructToString
    , GroupGraphPattern, Triple, triple, triplesAlong
    , Expression, simple, isIri
    , empty, values, triples
    , optional, or, and
    , filter, bind
    , Results, resultsDecoder
    )

{-|

@docs Construct, construct, from, constructToString

@docs GroupGraphPattern, Triple, triple, triplesAlong
@docs Expression, simple, isIri

@docs empty, values, triples
@docs optional, or, and
@docs filter, bind

@docs Results, resultsDecoder

-}

import Dict exposing (Dict)
import Internal.Term
    exposing
        ( Term(..)
        , Variant(..)
        )
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Rdf
    exposing
        ( BlankNodeOrIriOrLiteral
        , BlankNodeOrIriOrLiteralOrVar
        , BlankNodeOrIriOrVar
        , Iri
        , IriOrLiteralOrVar
        , IsBlankNodeOrIriOrLiteral
        , IsBlankNodeOrIriOrLiteralOrVar
        , IsBlankNodeOrIriOrVar
        , IsIriOrLiteralOrVar
        , IsPath
        , IsVar
        , IsVarOrPath
        , Var
        , VarOrPath
        )
import Rdf.Namespaces exposing (xsd)


{-| TODO Add Documentation
-}
type alias Results =
    List (Dict String BlankNodeOrIriOrLiteral)


{-| TODO Add Documentation
-}
resultsDecoder : Decoder Results
resultsDecoder =
    Decode.list (Decode.dict nodeDecoder)


nodeDecoder : Decoder BlankNodeOrIriOrLiteral
nodeDecoder =
    Decode.oneOf
        [ literalDecoder
        , namedNodeDecoder
        , blankNodeDecoder
        ]


literalDecoder : Decoder BlankNodeOrIriOrLiteral
literalDecoder =
    let
        toNode : String -> Maybe String -> Maybe String -> BlankNodeOrIriOrLiteral
        toNode value maybeDatatype maybeLanguage =
            case maybeLanguage of
                Nothing ->
                    let
                        datatype : Iri
                        datatype =
                            maybeDatatype
                                |> Maybe.map Rdf.iri
                                |> Maybe.withDefault (xsd "string")
                    in
                    value
                        |> Rdf.literal datatype
                        |> Rdf.asBlankNodeOrIriOrLiteral

                Just language ->
                    value
                        |> Rdf.langString language
                        |> Rdf.asBlankNodeOrIriOrLiteral
    in
    Decode.oneOf
        [ Decode.field "termType" Decode.string
            |> Decode.andThen
                (\termType ->
                    if termType == "Literal" then
                        Decode.succeed toNode
                            |> Decode.required "value" Decode.string
                            |> Decode.requiredAt [ "datatype", "value" ] (Decode.map Just Decode.string)
                            |> Decode.optional "language" (Decode.map Just Decode.string) Nothing

                    else
                        Decode.fail "not a Literal"
                )
        , Decode.field "type" Decode.string
            |> Decode.andThen
                (\type_ ->
                    if type_ == "literal" || type_ == "typed-literal" then
                        Decode.succeed toNode
                            |> Decode.required "value" Decode.string
                            |> Decode.optional "datatype" (Decode.map Just Decode.string) Nothing
                            |> Decode.optional "xml:lang" (Decode.map Just Decode.string) Nothing

                    else
                        Decode.fail "not a Literal"
                )
        ]


namedNodeDecoder : Decoder BlankNodeOrIriOrLiteral
namedNodeDecoder =
    let
        toNode : String -> BlankNodeOrIriOrLiteral
        toNode value =
            value
                |> Rdf.iri
                |> Rdf.asBlankNodeOrIriOrLiteral
    in
    Decode.oneOf
        [ Decode.field "termType" Decode.string
            |> Decode.andThen
                (\termType ->
                    if termType == "NamedNode" then
                        Decode.succeed toNode
                            |> Decode.required "value" Decode.string

                    else
                        Decode.fail "not an IRI"
                )
        , Decode.field "type" Decode.string
            |> Decode.andThen
                (\type_ ->
                    if type_ == "uri" then
                        Decode.succeed toNode
                            |> Decode.required "value" Decode.string

                    else
                        Decode.fail "not an IRI"
                )
        ]


blankNodeDecoder : Decoder BlankNodeOrIriOrLiteral
blankNodeDecoder =
    let
        toNode : String -> Decoder BlankNodeOrIriOrLiteral
        toNode value =
            case String.split ":" value of
                [ _, blankNode ] ->
                    blankNode
                        |> Rdf.blankNode
                        |> Rdf.asBlankNodeOrIriOrLiteral
                        |> Decode.succeed

                [ blankNode ] ->
                    blankNode
                        |> Rdf.blankNode
                        |> Rdf.asBlankNodeOrIriOrLiteral
                        |> Decode.succeed

                _ ->
                    Decode.fail "not a Blank Node"
    in
    Decode.oneOf
        [ Decode.field "termType" Decode.string
            |> Decode.andThen
                (\type_ ->
                    if type_ == "BlankNode" then
                        Decode.field "value" Decode.string
                            |> Decode.andThen toNode

                    else
                        Decode.fail "not a Blank Node"
                )
        , Decode.field "type" Decode.string
            |> Decode.andThen
                (\type_ ->
                    if type_ == "bnode" then
                        Decode.field "value" Decode.string
                            |> Decode.andThen toNode

                    else
                        Decode.fail "not a Blank Node"
                )
        ]


{-| TODO Add Documentation
-}
construct : List Triple -> GroupGraphPattern -> Construct
construct template clause =
    Construct
        { template = template
        , froms = []
        , clause = clause
        }


{-| TODO Add Documentation
-}
from : Iri -> Construct -> Construct
from iri (Construct data) =
    Construct { data | froms = iri :: data.froms }


{-| TODO Add Documentation
-}
constructToString : Construct -> String
constructToString (Construct data) =
    String.join "\n"
        [ "CONSTRUCT {"
        , data.template
            |> List.map serializeTriple
            |> String.join "\n"
            |> indent
        , if List.isEmpty data.froms then
            "} WHERE {"

          else
            String.join "\n"
                [ "}"
                , data.froms
                    |> List.map serializeFrom
                    |> String.join "\n"
                , "WHERE {"
                ]
        , data.clause
            |> serializeGroupGraphPattern
            |> Maybe.map indent
            |> Maybe.withDefault ""
        , "}"
        ]


serializeFrom : Iri -> String
serializeFrom iri =
    "FROM " ++ Rdf.serialize iri


{-| TODO Add Documentation
-}
type Construct
    = Construct DataConstruct


type alias DataConstruct =
    { template : List Triple
    , froms : List Iri
    , clause : GroupGraphPattern
    }


{-| TODO Add Documentation
-}
type GroupGraphPattern
    = Empty
    | Values Var (List BlankNodeOrIriOrLiteral)
    | Triples (List Triple)
    | Optional GroupGraphPattern
    | Union (List GroupGraphPattern)
    | Intersection (List GroupGraphPattern)
    | Filter Expression
    | Bind Expression Var


{-| TODO Add Documentation
-}
type alias Triple =
    { subject : BlankNodeOrIriOrVar
    , predicate : VarOrPath
    , object : BlankNodeOrIriOrLiteralOrVar
    }


{-| TODO Add Documentation
-}
type Expression
    = Simple IriOrLiteralOrVar
    | IsIri Expression


{-| TODO Add Documentation
-}
empty : GroupGraphPattern
empty =
    Empty


{-| TODO Add Documentation
-}
values :
    IsVar compatible1
    -> List (IsBlankNodeOrIriOrLiteral compatible2)
    -> GroupGraphPattern
values v vs =
    Values (Rdf.asVar v) (List.map Rdf.asBlankNodeOrIriOrLiteral vs)


{-| TODO Add Documentation
-}
triples : List Triple -> GroupGraphPattern
triples =
    Triples


{-| TODO Add Documentation
-}
optional : GroupGraphPattern -> GroupGraphPattern
optional =
    Optional


{-| TODO Add Documentation
-}
or : GroupGraphPattern -> GroupGraphPattern -> GroupGraphPattern
or left right =
    case ( left, right ) of
        ( Empty, _ ) ->
            right

        ( _, Empty ) ->
            left

        ( Union nestedLeft, Union nestedRight ) ->
            Union (nestedLeft ++ nestedRight)

        ( Union nestedLeft, _ ) ->
            Union (nestedLeft ++ [ right ])

        ( _, Union nestedRight ) ->
            Union (left :: nestedRight)

        _ ->
            Union [ left, right ]


{-| TODO Add Documentation
-}
and : GroupGraphPattern -> GroupGraphPattern -> GroupGraphPattern
and left right =
    case ( left, right ) of
        ( Empty, _ ) ->
            right

        ( _, Empty ) ->
            left

        ( Intersection nestedLeft, Intersection nestedRight ) ->
            Intersection (nestedLeft ++ nestedRight)

        ( Intersection nestedLeft, _ ) ->
            Intersection (nestedLeft ++ [ right ])

        ( _, Intersection nestedRight ) ->
            Intersection (left :: nestedRight)

        _ ->
            Intersection [ left, right ]


{-| TODO Add Documentation
-}
filter : Expression -> GroupGraphPattern
filter =
    Filter


{-| TODO Add Documentation
-}
bind : Expression -> IsVar compatible -> GroupGraphPattern
bind expression var =
    Bind expression (Rdf.asVar var)


{-| TODO Add Documentation
-}
simple : IsIriOrLiteralOrVar compatible -> Expression
simple node =
    Simple (Rdf.asIriOrLiteralOrVar node)


{-| TODO Add Documentation
-}
isIri : Expression -> Expression
isIri =
    IsIri


{-| TODO Add Documentation
-}
triple :
    IsBlankNodeOrIriOrVar compatible1
    -> IsVarOrPath compatible2
    -> IsBlankNodeOrIriOrLiteralOrVar compatible3
    -> Triple
triple subject predicate object =
    { subject = Rdf.asBlankNodeOrIriOrVar subject
    , predicate = Rdf.asVarOrPath predicate
    , object = Rdf.asBlankNodeOrIriOrLiteralOrVar object
    }


{-| TODO Add Documentation
-}
triplesAlong :
    String
    -> IsBlankNodeOrIriOrVar compatible1
    -> IsPath compatible2
    -> IsBlankNodeOrIriOrLiteralOrVar compatible3
    -> List Triple
triplesAlong id subject path object =
    Tuple.second (triplesAlongHelp 0 id subject path object)


triplesAlongHelp :
    Int
    -> String
    -> IsBlankNodeOrIriOrVar compatible1
    -> IsPath compatible2
    -> IsBlankNodeOrIriOrLiteralOrVar compatible3
    -> ( Int, List Triple )
triplesAlongHelp seed id subject (Term variant) object =
    case variant of
        (Iri _) as iri ->
            ( seed
            , [ triple subject (Term iri) object ]
            )

        Sequence first rest ->
            case List.reverse rest of
                [] ->
                    triplesAlongHelp seed id subject (Term first) object

                last :: reversed ->
                    let
                        termFirst : Var
                        termFirst =
                            Rdf.varQ (id ++ "_" ++ String.fromInt seed)

                        ( seedFirst, tripleFirst ) =
                            triplesAlongHelp (seed + 1)
                                id
                                subject
                                (Term first)
                                (forgetCompatible termFirst)

                        ( termSecondLast, seedSecondLast, triplesSecondLast ) =
                            List.foldl
                                (\current ( termCurrent, seedCurrent, collected ) ->
                                    let
                                        termNext : Var
                                        termNext =
                                            Rdf.varQ (id ++ "_" ++ String.fromInt seedCurrent)

                                        ( seedNext, tripleNext ) =
                                            triplesAlongHelp (seedCurrent + 1)
                                                id
                                                termCurrent
                                                (Term current)
                                                (forgetCompatible termNext)
                                    in
                                    ( forgetCompatible termNext
                                    , seedNext
                                    , tripleNext :: collected
                                    )
                                )
                                ( forgetCompatible termFirst
                                , seedFirst
                                , [ tripleFirst ]
                                )
                                (List.reverse reversed)

                        ( seedLast, tripleLast ) =
                            triplesAlongHelp (seedSecondLast + 1)
                                id
                                termSecondLast
                                (Term last)
                                object
                    in
                    ( seedLast
                    , List.concat (List.reverse (tripleLast :: triplesSecondLast))
                    )

        Alternative _ _ ->
            -- FIXME
            ( seed, [] )

        Inverse ((Iri _) as iri) ->
            case Rdf.toVar object of
                Nothing ->
                    case Rdf.toBlankNodeOrIri object of
                        Nothing ->
                            -- FIXME Return error instead
                            ( seed, [] )

                        Just blankNodeOrIri ->
                            ( seed
                            , [ triple
                                    blankNodeOrIri
                                    (Term iri)
                                    (Rdf.asBlankNodeOrIriOrVar subject)
                              ]
                            )

                Just objectVar ->
                    ( seed
                    , [ triple objectVar
                            (Term iri)
                            (Rdf.asBlankNodeOrIriOrVar subject)
                      ]
                    )

        Inverse _ ->
            -- FIXME
            ( seed, [] )

        ZeroOrMore _ ->
            -- FIXME Return error instead
            ( seed, [] )

        OneOrMore _ ->
            -- FIXME Return error instead
            ( seed, [] )

        ZeroOrOne _ ->
            -- FIXME Return error instead
            ( seed, [] )

        _ ->
            -- This should never happen
            ( 0, [] )


forgetCompatible : Term compatible1 -> Term compatible2
forgetCompatible (Term variant) =
    Term variant


serializeGroupGraphPattern : GroupGraphPattern -> Maybe String
serializeGroupGraphPattern groupGraphPattern =
    case groupGraphPattern of
        Empty ->
            Nothing

        Values t vs ->
            Just
                (String.join "\n"
                    [ "VALUES " ++ Rdf.serialize t ++ " {"
                    , indent
                        (String.join "\n"
                            (List.map Rdf.serialize vs)
                        )
                    , "}"
                    ]
                )

        Triples ts ->
            if List.isEmpty ts then
                Nothing

            else
                Just (String.join "\n" (List.map serializeTriple ts))

        Optional groupGraphPatternNested ->
            groupGraphPatternNested
                |> serializeGroupGraphPattern
                |> Maybe.map
                    (\serialized ->
                        String.join "\n"
                            [ "OPTIONAL {"
                            , indent serialized
                            , "}"
                            ]
                    )

        Union groupGraphPatterns ->
            groupGraphPatterns
                |> List.filterMap serializeGroupGraphPattern
                |> List.map
                    (\serialized ->
                        String.join "\n"
                            [ "{"
                            , indent serialized
                            , "}"
                            ]
                    )
                |> String.join "\nUNION\n"
                |> Just

        Intersection groupGraphPatterns ->
            case List.filterMap serializeGroupGraphPattern groupGraphPatterns of
                [] ->
                    Nothing

                serialized ->
                    Just (String.join "\n" serialized)

        Filter expression ->
            Just
                (String.concat
                    [ "FILTER("
                    , serializeExpression expression
                    , ")"
                    ]
                )

        Bind expression var ->
            Just
                (String.concat
                    [ "BIND("
                    , serializeExpression expression
                    , " AS "
                    , Rdf.serialize var
                    , ")"
                    ]
                )


serializeTriple : Triple -> String
serializeTriple { subject, predicate, object } =
    String.join " "
        [ Rdf.serialize subject
        , Rdf.serialize predicate
        , Rdf.serialize object
        , "."
        ]


serializeExpression : Expression -> String
serializeExpression expression =
    case expression of
        Simple var ->
            Rdf.serialize var

        IsIri expressionNested ->
            "isIRI(" ++ serializeExpression expressionNested ++ ")"


indent : String -> String
indent =
    String.lines
        >> List.map (\line -> "  " ++ line)
        >> String.join "\n"
