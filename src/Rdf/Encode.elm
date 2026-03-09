module Rdf.Encode exposing
    ( Encoder
    , encode
    , node, blankNode
    , fromNode, fromBlankNode
    , property
    , term
    , GraphEncoder, IsGraphEncoder
    , PropertyEncoder, IsPropertyEncoder
    , TermEncoder, IsTermEncoder
    , GraphOrTermEncoder, IsGraphOrTermEncoder
    , Compatible
    )

{-| Turn elm values into `Graph`'s. This is what
[Json.Encode](https://package.elm-lang.org/packages/elm/json/latest/Json-Encode)
is for JSON values. Since generating RDF graphs potentially requires minting
(local) names for blank nodes, you first have to create an
[`Encoder`](#Encoder) and then turn this into an `Graph` by feeding it into
[`encode`](#encode) with a `Seed`.


# Encoding

@docs Encoder
@docs encode


# Primitives

@docs node, blankNode
@docs fromNode, fromBlankNode
@docs property
@docs term


# Types

@docs GraphEncoder, IsGraphEncoder
@docs PropertyEncoder, IsPropertyEncoder
@docs TermEncoder, IsTermEncoder
@docs GraphOrTermEncoder, IsGraphOrTermEncoder


## Utility

@docs Compatible

-}

import Rdf
    exposing
        ( BlankNodeOrIri
        , Iri
        , IsBlankNodeOrIri
        , IsBlankNodeOrIriOrLiteral
        , IsIri
        , IsPath
        )
import Rdf.Graph as Rdf exposing (Graph, Seed)
import Rdf.Predicate as Predicate



-- ENCODING


{-| A value which knows how to encode an Elm value into an RDF graph.

The `compatible` type variable is used to ensure that certain instances of an
`Encoder` can only be used in the right context.

-}
type Encoder compatible
    = Encoder EncoderInternal


type EncoderInternal
    = GraphEncoder (Seed -> ( BlankNodeOrIri, ( Graph, Seed ) ))
    | PropertyEncoder (Seed -> BlankNodeOrIri -> ( Graph, Seed ))
    | TermEncoder (Seed -> BlankNodeOrIri -> Iri -> ( Graph, Seed ))


{-| Run an [`Encoder`](#Encoder). You have to provide a `Seed` which is used
for minting unique blank node identifiers. The function `encode` returns the
RDF graph as well as an updated `Seed` which you should use for further
encoding or parsing of RDF graphs to ensure you don't get blank node identifier
clashes.
-}
encode : Seed -> IsGraphEncoder graph -> ( Graph, Seed )
encode seed (Encoder encoder) =
    case encoder of
        GraphEncoder f ->
            Tuple.second (f seed)

        PropertyEncoder _ ->
            ( Rdf.emptyGraph, seed )

        TermEncoder _ ->
            ( Rdf.emptyGraph, seed )



-- PRIMITIVES


{-| Encode a node, for which we already have a name, alongside some properties
where the node is the subject.
-}
node : IsBlankNodeOrIri compatible -> List PropertyEncoder -> GraphEncoder
node subject propertyEs =
    Encoder (GraphEncoder (\seed -> nodeHelp propertyEs subject seed))


{-| Encode a new blank node alongside some properties where the node is the
subject. This will mint a new blank node identifier under the hood.
-}
blankNode : List PropertyEncoder -> GraphEncoder
blankNode propertyEs =
    Encoder
        (GraphEncoder
            (\seed ->
                let
                    ( subject, seedUpdated ) =
                        Tuple.mapFirst Rdf.asBlankNodeOrIri
                            (Rdf.generateBlankNode seed)
                in
                nodeHelp propertyEs subject seedUpdated
            )
        )


{-| This works like [`node`](#node) but returns a `PropertyEncoder`. You can
use this if you are deep in a tree of encoders and have to encode
a different/detached part of the graph.
-}
fromNode : IsBlankNodeOrIri compatible -> List PropertyEncoder -> PropertyEncoder
fromNode subject propertyEs =
    Encoder (GraphEncoder (\seed -> nodeHelp propertyEs subject seed))


{-| This works like [`blankNode`](#blankNode) but returns a `PropertyEncoder`.
You can use this if you are deep in a tree of encoders and have to encode
a different/detached part of the graph.
-}
fromBlankNode : List PropertyEncoder -> PropertyEncoder
fromBlankNode propertyEs =
    Encoder
        (GraphEncoder
            (\seed ->
                let
                    ( subject, seedUpdated ) =
                        Tuple.mapFirst Rdf.asBlankNodeOrIri
                            (Rdf.generateBlankNode seed)
                in
                nodeHelp propertyEs subject seedUpdated
            )
        )


nodeHelp :
    List PropertyEncoder
    -> IsBlankNodeOrIri compatible
    -> Seed
    -> ( BlankNodeOrIri, ( Graph, Seed ) )
nodeHelp propertyEs subject seed =
    ( Rdf.asBlankNodeOrIri subject
    , Tuple.mapFirst (List.foldl Rdf.union Rdf.emptyGraph) <|
        List.foldl
            (\(Encoder propertyE) ( graphs, seedNext ) ->
                case propertyE of
                    PropertyEncoder f ->
                        let
                            ( graph, seedNextNext ) =
                                f seedNext (Rdf.asBlankNodeOrIri subject)
                        in
                        ( graph :: graphs, seedNextNext )

                    GraphEncoder f ->
                        let
                            ( _, ( graph, seedNextNext ) ) =
                                f seedNext
                        in
                        ( graph :: graphs, seedNextNext )

                    TermEncoder _ ->
                        ( graphs, seedNext )
            )
            ( [], seed )
            propertyEs
    )


{-| Encode a property by providing an IRI of property path and an object. The
current focus node will be the subject.
-}
property : IsPath compatible -> IsGraphOrTermEncoder object -> PropertyEncoder
property propertyPath encoder =
    case Predicate.fromPropertyPath propertyPath of
        Just ( first, rest ) ->
            rest
                |> List.foldr
                    (\p ->
                        encoderFromPredicate p
                            >> List.singleton
                            >> blankNode
                            >> forgetCompatible
                    )
                    encoder
                |> encoderFromPredicate first

        Nothing ->
            Encoder
                (PropertyEncoder
                    (\seed _ ->
                        -- XXX `propertyPath` contains non-`Iri` components, we
                        -- just give up..
                        ( Rdf.emptyGraph, seed )
                    )
                )


encoderFromPredicate :
    Predicate.Predicate
    -> IsGraphOrTermEncoder object
    -> PropertyEncoder
encoderFromPredicate p encoder =
    case p of
        Predicate.Predicate predicate_ ->
            predicate predicate_ encoder

        Predicate.Inverse predicate_ ->
            inverse predicate_ encoder


predicate : IsIri compatible -> IsGraphOrTermEncoder object -> PropertyEncoder
predicate p (Encoder encoder) =
    let
        encoderNew : Seed -> IsBlankNodeOrIri compatible2 -> ( Graph, Seed )
        encoderNew seed subject =
            case encoder of
                GraphEncoder f ->
                    let
                        ( objectNew, ( graphObject, seedUpdated ) ) =
                            f seed
                    in
                    ( Rdf.insert
                        subject
                        (Rdf.asIri p)
                        (Rdf.asBlankNodeOrIriOrLiteral objectNew)
                        graphObject
                    , seedUpdated
                    )

                PropertyEncoder _ ->
                    ( Rdf.emptyGraph, seed )

                TermEncoder f ->
                    f seed (Rdf.asBlankNodeOrIri subject) (Rdf.asIri p)
    in
    Encoder (PropertyEncoder encoderNew)


inverse :
    IsIri compatible1
    -> IsGraphOrTermEncoder compatible2
    -> PropertyEncoder
inverse p (Encoder encoder) =
    let
        encoderNew :
            Seed
            -> IsBlankNodeOrIriOrLiteral compatible
            -> ( Graph, Seed )
        encoderNew seed subject =
            case encoder of
                GraphEncoder f ->
                    let
                        ( objectNew, ( graphObject, seedUpdated ) ) =
                            f seed
                    in
                    ( Rdf.insert
                        objectNew
                        (Rdf.asIri p)
                        (Rdf.asBlankNodeOrIriOrLiteral subject)
                        graphObject
                    , seedUpdated
                    )

                TermEncoder _ ->
                    -- Since RDF Literals can never be the subject of a Triple,
                    -- this case should not happen
                    ( Rdf.emptyGraph, seed )

                PropertyEncoder _ ->
                    ( Rdf.emptyGraph, seed )
    in
    Encoder (PropertyEncoder encoderNew)


{-| Encode a single RDF term.

    import Rdf exposing (iri)
    import Rdf.Namespaces exposing (a)
    import Rdf.Graph exposing (initialSeed, serializeTurtle)

    blankNode
        [ property a (term (iri "http://example.org#Person")) ]
        |> encode initialSeed
        |> Tuple.first
        |> serializeTurtle
        |> String.lines
    --> [ "_:2273e9c9-fa2d-4c28-ae87-947ad36cbecd"
    --> , "  a <http://example.org#Person> ."
    --> ]

-}
term : IsBlankNodeOrIriOrLiteral compatible -> TermEncoder
term t =
    Encoder
        (TermEncoder
            (\seed subject predicate_ ->
                ( Rdf.singleton subject predicate_ t, seed )
            )
        )



-- TYPES


{-| You can ignore this type, it is only used for implementing the different
`Encoder` variants.
-}
type Compatible
    = Compatible Never



-- GRAPH ENCODER


{-| An `Encoder` which can be fed into `encode` to produce an RDF graph.
-}
type alias GraphEncoder =
    Encoder
        { graph : Compatible
        , graphOrTerm : Compatible
        }


{-| This type will only be used as the argument of functions.
-}
type alias IsGraphEncoder compatible =
    Encoder
        { compatible
            | graph : Compatible
            , graphOrTerm : Compatible
        }



-- PROPERTY ENCODER


{-| An `Encoder` which encodes an RDF property.
-}
type alias PropertyEncoder =
    Encoder
        { property : Compatible
        }


{-| This type will only be used as the argument of functions.
-}
type alias IsPropertyEncoder compatible =
    Encoder { compatible | property : Compatible }



-- LITERAL ENCODER


{-| An `Encoder` which encodes an RDF term.
-}
type alias TermEncoder =
    Encoder
        { term : Compatible
        , graphOrTerm : Compatible
        }


{-| This type will only be used as the argument of functions.
-}
type alias IsTermEncoder compatible =
    Encoder
        { compatible
            | term : Compatible
            , graphOrTerm : Compatible
        }



-- GRAPH OR LITERAL ENCODER


{-| An `Encoder` which encodes an RDF graph or an RDf term.
-}
type alias IsGraphOrTermEncoder compatible =
    Encoder { compatible | graphOrTerm : Compatible }


{-| This type will only be used as the argument of functions.
-}
type alias GraphOrTermEncoder =
    Encoder
        { graphOrTerm : Compatible
        }


forgetCompatible : Encoder compatible1 -> Encoder compatible2
forgetCompatible (Encoder encoder) =
    Encoder encoder
