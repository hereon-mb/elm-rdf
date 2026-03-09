module Rdf.Sparql exposing
    ( Query
    , toString, toStringWith
    , construct
    , from
    , GroupGraphPattern, where_
    , empty, values
    , triples
    , optional, or, and
    , filter, bind
    , Expression
    , simple, isIri, equals, concat
    , Triple, triple, triplesAlong
    , Results, resultsDecoder
    )

{-| This module contains functions and types to create [SPARQL 1.1
queries](https://www.w3.org/TR/sparql11-query/).

@docs Query
@docs toString, toStringWith


# `CONSTRUCT` queries

@docs construct


# `FROM` clauses

@docs from


# `WHERE` clauses

@docs GroupGraphPattern, where_
@docs empty, values
@docs triples
@docs optional, or, and
@docs filter, bind


## Expressions

@docs Expression
@docs simple, isIri, equals, concat


# Triple patterns/templates

@docs Triple, triple, triplesAlong


# Results

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
        , Prologue
        , Var
        , VarOrPath
        , xsd
        )


{-| The `Query` type represents a SPARQL query.
-}
type Query
    = Construct DataConstruct


type alias DataConstruct =
    { template : List Triple
    , froms : List Iri
    , pattern : Maybe GroupGraphPattern
    }


{-| Turn a `Query` into a SPARQL query.
-}
toString : Query -> String
toString =
    toStringWith prologueEmpty


prologueEmpty : Prologue
prologueEmpty =
    { base = Nothing
    , prefixes = []
    }


{-| Turn a `Query` into a SPARQL query including and using the `BASE` and
`PREFIX` declarations.
-}
toStringWith : Prologue -> Query -> String
toStringWith prologue (Construct data) =
    String.concat
        [ serializePrologue prologue
        , "CONSTRUCT {"
        , if List.isEmpty data.template then
            ""

          else
            "\n"
                ++ (data.template
                        |> List.map (serializeTriple prologue)
                        |> String.join "\n"
                        |> indent
                   )
                ++ "\n"
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
        , data.pattern
            |> Maybe.withDefault empty
            |> serializeGroupGraphPattern prologue
            |> Maybe.map indent
            |> Maybe.map (\clause -> "\n" ++ clause ++ "\n")
            |> Maybe.withDefault ""
        , "}"
        ]


serializePrologue : Prologue -> String
serializePrologue prologue =
    String.concat
        [ case prologue.base of
            Nothing ->
                ""

            Just base ->
                "BASE <" ++ base ++ ">\n"
        , if List.isEmpty prologue.prefixes then
            ""

          else
            String.join "\n"
                (List.map serializePrefix prologue.prefixes)
                ++ "\n"
        ]


serializePrefix : ( String, String ) -> String
serializePrefix ( name, value ) =
    "PREFIX " ++ name ++ ": <" ++ value ++ ">"


serializeFrom : Iri -> String
serializeFrom iri =
    "FROM " ++ Rdf.serialize iri



-- CONSTRUCT QUERIES


{-| Build a [`CONSTRUCT`](https://www.w3.org/TR/sparql11-query/#construct)
query.
-}
construct : List Triple -> Query
construct template =
    Construct
        { template = template
        , froms = []
        , pattern = Nothing
        }



-- FROM CLAUSES


{-| Restrict a query to a specific graph by adding
a [`FROM`](https://www.w3.org/TR/sparql11-query/#specifyingDataset).
-}
from : Iri -> Query -> Query
from iri (Construct data) =
    Construct { data | froms = iri :: data.froms }



-- WHERE CLAUSES


{-| The basic building block of a `WHERE` clause. Take a look at the functions
below to learn how to create and combine these.
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


{-| Set the `WHERE` clause of a `Query` to the given group graph pattern. If
you don't provide this, the empty group graph pattern will be used.
-}
where_ : GroupGraphPattern -> Query -> Query
where_ pattern query =
    case query of
        Construct data ->
            Construct { data | pattern = Just pattern }


{-| Create the empty group graph pattern `{ }`.

    construct []
        |> where_ empty
        |> toString
    --> "CONSTRUCT {} WHERE {}"

-}
empty : GroupGraphPattern
empty =
    Empty


{-| Provide inline data.

    import Rdf

    construct []
        |> where_
            (values (Rdf.varQ "person")
                [ Rdf.iri "http://example.org/alice" ]
            )
        |> toString
        |> String.lines
    --> [ "CONSTRUCT {} WHERE {"
    --> , "  VALUES ?person {"
    --> , "    <http://example.org/alice>"
    --> , "  }"
    --> , "}"
    --> ]

-}
values :
    IsVar compatible1
    -> List (IsBlankNodeOrIriOrLiteral compatible2)
    -> GroupGraphPattern
values v vs =
    Values (Rdf.asVar v) (List.map Rdf.asBlankNodeOrIriOrLiteral vs)


{-| Create a basic graph pattern which matches a set of triple patterns.

    import Rdf exposing (iri, varQ, a, rdfs)

    construct []
        |> where_
            (triples
                [ triple (varQ "person") a (iri "http://example.org#Person")
                , triple (varQ "person") (rdfs "label") (varQ "label")
                ]
            )
        |> toStringWith
            { base = Just "http://example.org#"
            , prefixes = [ ( "rdfs", "http://www.w3.org/2000/01/rdf-schema#" ) ]
            }
        |> String.lines
    --> [ "BASE <http://example.org#>"
    --> , "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>"
    --> , "CONSTRUCT {} WHERE {"
    --> , "  ?person a <Person> ."
    --> , "  ?person rdfs:label ?label ."
    --> , "}"
    --> ]

-}
triples : List Triple -> GroupGraphPattern
triples =
    Triples


{-| Make a graph pattern optional.

    import Rdf exposing (iri, varQ, a, rdfs)

    construct []
        |> where_
            (and
                (triples
                    [ triple (varQ "person") a (iri "http://example.org#Person") ])
                (optional
                    (triples
                        [ triple (varQ "person") (rdfs "label") (varQ "label") ])
                )
            )
        |> toStringWith
            { base = Just "http://example.org#"
            , prefixes = [ ( "rdfs", "http://www.w3.org/2000/01/rdf-schema#" ) ]
            }
        |> String.lines
    --> [ "BASE <http://example.org#>"
    --> , "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>"
    --> , "CONSTRUCT {} WHERE {"
    --> , "  ?person a <Person> ."
    --> , "  OPTIONAL {"
    --> , "    ?person rdfs:label ?label ."
    --> , "  }"
    --> , "}"
    --> ]

-}
optional : GroupGraphPattern -> GroupGraphPattern
optional =
    Optional


{-| Create a graph pattern which matches the first **or** the second pattern.

    import Rdf exposing (iri, varQ, a, rdfs)

    construct []
        |> where_
            (or
                (triples [ triple (varQ "person") a (iri "http://example.org#Author") ])
                (triples [ triple (varQ "person") a (iri "http://example.org#Reader") ])
            )
        |> toStringWith
            { base = Just "http://example.org#"
            , prefixes = [ ( "rdfs", "http://www.w3.org/2000/01/rdf-schema#" ) ]
            }
        |> String.lines
    --> [ "BASE <http://example.org#>"
    --> , "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>"
    --> , "CONSTRUCT {} WHERE {"
    --> , "  {"
    --> , "    ?person a <Author> ."
    --> , "  }"
    --> , "  UNION"
    --> , "  {"
    --> , "    ?person a <Reader> ."
    --> , "  }"
    --> , "}"
    --> ]

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


{-| Create a graph pattern which matches the first **and** the second pattern.

    import Rdf exposing (iri, varQ, a, rdfs)

    construct []
        |> where_
            (and
                (triples [ triple (varQ "person") a (iri "http://example.org#Person") ])
                (triples [ triple (varQ "person") a (iri "http://example.org#Maintainer") ])
            )
        |> toStringWith
            { base = Just "http://example.org#"
            , prefixes = [ ( "rdfs", "http://www.w3.org/2000/01/rdf-schema#" ) ]
            }
        |> String.lines
    --> [ "BASE <http://example.org#>"
    --> , "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>"
    --> , "CONSTRUCT {} WHERE {"
    --> , "  ?person a <Person> ."
    --> , "  ?person a <Maintainer> ."
    --> , "}"
    --> ]

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


{-| Add a filter expression.

    import Rdf exposing (iri, string, varQ, a, rdfs)

    construct []
        |> where_
            (and
                (triples
                    [ triple (varQ "person") a (iri "http://example.org#Person")
                    , triple (varQ "person") (rdfs "label") (varQ "label")
                    ]
                )
                (filter
                    (equals
                        (simple (varQ "person"))
                        (simple (string "Alice"))
                    )
                )
            )
        |> toStringWith
            { base = Just "http://example.org#"
            , prefixes = [ ( "rdfs", "http://www.w3.org/2000/01/rdf-schema#" ) ]
            }
        |> String.lines
    --> [ "BASE <http://example.org#>"
    --> , "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>"
    --> , "CONSTRUCT {} WHERE {"
    --> , "  ?person a <Person> ."
    --> , "  ?person rdfs:label ?label ."
    --> , "  FILTER(?person = \"Alice\")"
    --> , "}"
    --> ]

-}
filter : Expression -> GroupGraphPattern
filter =
    Filter


{-| Bind the result of an expression to a query variable.

    import Rdf exposing (iri, string, varQ, a, rdfs)

    construct []
        |> where_
            (and
                (triples
                    [ triple (varQ "person") a (iri "http://example.org#Person")
                    , triple (varQ "person") (iri "http://example.org#firstName") (varQ "firstName")
                    , triple (varQ "person") (iri "http://example.org#lastName") (varQ "lastName")
                    ]
                )
                (bind
                    (concat
                        [ simple (varQ "firstName")
                        , simple (string " ")
                        , simple (varQ "lastName")
                        ]
                    )
                    (varQ "name")
                )
            )
        |> toStringWith
            { base = Just "http://example.org#"
            , prefixes = [ ( "rdfs", "http://www.w3.org/2000/01/rdf-schema#" ) ]
            }
        |> String.lines
    --> [ "BASE <http://example.org#>"
    --> , "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>"
    --> , "CONSTRUCT {} WHERE {"
    --> , "  ?person a <Person> ."
    --> , "  ?person <firstName> ?firstName ."
    --> , "  ?person <lastName> ?lastName ."
    --> , "  BIND("
    --> , "    CONCAT("
    --> , "      ?firstName,"
    --> , "      \" \","
    --> , "      ?lastName"
    --> , "    )"
    --> , "    AS"
    --> , "    ?name"
    --> , "  )"
    --> , "}"
    --> ]

-}
bind : Expression -> IsVar compatible -> GroupGraphPattern
bind expression var =
    Bind expression (Rdf.asVar var)



-- EXPRESSIONS


{-| The basic building block of an expression. Take a look at the functions
below to learn how to create and combine these.
-}
type Expression
    = Simple IriOrLiteralOrVar
    | IsIri Expression
    | Equals Expression Expression
    | Concat (List Expression)


{-| Turn an RDF term into an `Expression`.
-}
simple : IsIriOrLiteralOrVar compatible -> Expression
simple node =
    Simple (Rdf.asIriOrLiteralOrVar node)


{-| Check if the `Expression` is an IRI. This translates to
[`isIRI`](https://www.w3.org/TR/sparql11-query/#func-isIRI).
-}
isIri : Expression -> Expression
isIri =
    IsIri


{-| Compare two expressions. This translates to `<first> = <second>`.
-}
equals : Expression -> Expression -> Expression
equals =
    Equals


{-| Concatenate a list of `Expressions` which evaluate to string literals. This
translates to [`CONCAT`](https://www.w3.org/TR/sparql11-query/#func-concat).
-}
concat : List Expression -> Expression
concat =
    Concat


serializeGroupGraphPattern : Prologue -> GroupGraphPattern -> Maybe String
serializeGroupGraphPattern prologue groupGraphPattern =
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
                Just
                    (String.join "\n"
                        (List.map (serializeTriple prologue) ts)
                    )

        Optional groupGraphPatternNested ->
            groupGraphPatternNested
                |> serializeGroupGraphPattern prologue
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
                |> List.filterMap (serializeGroupGraphPattern prologue)
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
            case
                List.filterMap
                    (serializeGroupGraphPattern prologue)
                    groupGraphPatterns
            of
                [] ->
                    Nothing

                serialized ->
                    Just (String.join "\n" serialized)

        Filter expression ->
            Just
                (String.concat
                    [ "FILTER("
                    , serializeExpression prologue expression
                    , ")"
                    ]
                )

        Bind expression var ->
            Just
                (String.join "\n"
                    [ "BIND("
                    , indent
                        (String.join "\n"
                            [ serializeExpression prologue expression
                            , "AS"
                            , Rdf.serialize var
                            ]
                        )
                    , ")"
                    ]
                )


serializeTriple : Prologue -> Triple -> String
serializeTriple prologue { subject, predicate, object } =
    String.join " "
        [ Rdf.serializeWith prologue subject
        , Rdf.serializeWith prologue predicate
        , Rdf.serializeWith prologue object
        , "."
        ]


serializeExpression : Prologue -> Expression -> String
serializeExpression prologue expression =
    case expression of
        Simple var ->
            Rdf.serializeWith prologue var

        IsIri expressionNested ->
            "isIRI("
                ++ serializeExpression prologue expressionNested
                ++ ")"

        Equals left right ->
            serializeExpression prologue left
                ++ " = "
                ++ serializeExpression prologue right

        Concat nested ->
            String.join "\n"
                [ "CONCAT("
                , indent
                    (String.join ",\n"
                        (List.map (serializeExpression prologue) nested)
                    )
                , ")"
                ]


indent : String -> String
indent =
    String.lines
        >> List.map (\line -> "  " ++ line)
        >> String.join "\n"



-- TRIPLES


{-| This type represents triple patterns (which appear in `WHERE` clauses) as
well as triple templates (which appear in `CONSTRUCT` clauses). Note that since
predicates in triple templates cannot contain property paths, triples
containing these will be skipped when converting `Query`'s into SPARQL queries.
-}
type alias Triple =
    { subject : BlankNodeOrIriOrVar
    , predicate : VarOrPath
    , object : BlankNodeOrIriOrLiteralOrVar
    }


{-| Create a triple pattern/template by providing a subject, verb, and object.
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


{-| Expand a triple pattern/template with a property path predicate into a list
of triple patterns with IRI or variable predicates. The first argument is the
prefix used for intermediate query variables.

    import Rdf exposing (iri, inverse, sequence, varQ, asPath, a, rdfs)

    construct []
        |> where_
            (triplesAlong "person"
                (varQ "child")
                (sequence
                    (inverse (iri "http://example.org#hasChild"))
                    [ inverse (iri "http://example.org#hasChild")
                    , asPath a
                    ]
                )
                (iri "http://example.org#Person")
                |> triples
            )
        |> toStringWith
            { base = Just "http://example.org#"
            , prefixes = [ ( "rdfs", "http://www.w3.org/2000/01/rdf-schema#" ) ]
            }
        |> String.lines
    --> [ "BASE <http://example.org#>"
    --> , "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>"
    --> , "CONSTRUCT {} WHERE {"
    --> , "  ?person_0 <hasChild> ?child ."
    --> , "  ?person_1 <hasChild> ?person_0 ."
    --> , "  ?person_1 a <Person> ."
    --> , "}"
    --> ]

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



-- RESULTS


{-| An instance of `Results` represents the results of a `SELECT` query.
-}
type alias Results =
    List (Dict String BlankNodeOrIriOrLiteral)


{-| A JSON decoder to decode the
[`"results"`](https://www.w3.org/TR/sparql12-results-json/#select-results)
section of a variable binding result.
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
