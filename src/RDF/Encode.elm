module RDF.Encode exposing
    ( Encoder
    , encode
    , IsGraphEncoder, IsPropertyEncoder, IsLiteralEncoder, IsGraphOrLiteralEncoder
    , GraphEncoder, PropertyEncoder, LiteralEncoder
    , Yes, No
    , Subject, Predicate, Object
    , blankNode
    , node
    , predicate
    , property
    , iri
    , literal
    , object
    )

{-| A domain-specific language for encoding `Graph`s.


# Introduction

The minimal motivation for this module is to encode the following example in an ergonomic DSL. Note that in this example, blank nodes are nested.

```turtle
[
   a polylab:CapillaryConstant ;
   om:hasValue [
     om:hasNumericalValue "3.14" ;
     om:hasUnit polylab:squareMilliMeterPerSecondSquare ;
   ]
]
```

This module proposes the following DSL to solve the problem.

    blankNode
        [ property a (polylab "CapillaryConstant"
        , property (om "hasValue")
            (blankNode
                [ property (om "hasNumericalValue") (RDF.string "3.14")
                , property (om "hasUnit") (polylab "squareMilliMeterPerSecndSquare")
                ]
            )
        ]

The following aspects of the DSL are note-worthy:

  - blank node generation happens transparently

  - blank nodes may appear as root nodes (subjects) as well as arguments to `property` (object nodes)

The following are deliberate limitations of the current implementation:

  - only one node can be encoded (easily solvable)

  - blank nodes cannot freely refer to each other (unclear if solvable)

Implementation-wise, the only challenge is the fact that blank nodes can be subjects as well as objects. We deploy the same technique as `RDF` does for typing `Node`s.


# Interface

@docs Encoder
@docs encode

@docs IsGraphEncoder, IsPropertyEncoder, IsLiteralEncoder, IsGraphOrLiteralEncoder
@docs GraphEncoder, PropertyEncoder, LiteralEncoder
@docs Yes, No

@docs Subject, Predicate, Object

@docs blankNode
@docs node
@docs predicate
@docs property

@docs iri
@docs literal
@docs object

-}

import RDF
import RDF.Graph as RDF exposing (Graph, Seed)
import RDF.PropertyPath as RDF exposing (PropertyPath)


{-| TODO
-}
type Encoder compatible
    = Encoder EncoderInternal


type EncoderInternal
    = GraphEncoder (Seed -> ( Subject, ( Graph, Seed ) ))
    | PropertyEncoder (Seed -> Subject -> ( Graph, Seed ))
    | LiteralEncoder (Seed -> Subject -> Predicate -> ( Graph, Seed ))


{-| TODO
-}
type alias Predicate =
    RDF.Iri


{-| TODO
-}
type alias Subject =
    RDF.BlankNodeOrIri


{-| TODO
-}
type alias Object =
    RDF.BlankNodeOrIriOrAnyLiteral


{-| TODO
-}
encode : Seed -> IsGraphEncoder graph -> ( Graph, Seed )
encode seed (Encoder encoder) =
    case encoder of
        GraphEncoder f ->
            Tuple.second (f seed)

        PropertyEncoder _ ->
            ( RDF.emptyGraph, seed )

        LiteralEncoder _ ->
            ( RDF.emptyGraph, seed )


{-| TODO
-}
blankNode : List PropertyEncoder -> GraphEncoder
blankNode propertyEs =
    Encoder
        (GraphEncoder
            (\seed ->
                let
                    ( subject, seedUpdated ) =
                        Tuple.mapFirst RDF.forgetCompatible (RDF.generateBlankNode seed)
                in
                nodeHelp propertyEs subject seedUpdated
            )
        )


{-| TODO
-}
node : Subject -> List PropertyEncoder -> GraphEncoder
node subject propertyEs =
    Encoder (GraphEncoder (\seed -> nodeHelp propertyEs subject seed))


nodeHelp : List PropertyEncoder -> Subject -> Seed -> ( Subject, ( Graph, Seed ) )
nodeHelp propertyEs subject seed =
    ( subject
    , Tuple.mapFirst (List.foldl RDF.union RDF.emptyGraph) <|
        List.foldl
            (\(Encoder propertyE) ( graphs, seedNext ) ->
                case propertyE of
                    PropertyEncoder f ->
                        let
                            ( graph, seedNextNext ) =
                                f seedNext (RDF.forgetCompatible subject)
                        in
                        ( graph :: graphs, seedNextNext )

                    GraphEncoder _ ->
                        ( graphs, seedNext )

                    LiteralEncoder _ ->
                        ( graphs, seedNext )
            )
            ( [], seed )
            propertyEs
    )


{-| TODO
-}
property :
    PropertyPath
    -> IsGraphOrLiteralEncoder object
    -> PropertyEncoder
property propertyPath objectE =
    case RDF.normalizePropertyPath propertyPath of
        Just ( predicate_, predicates ) ->
            predicate predicate_
                (List.foldr
                    (\predicateCurrent objectCurrentE ->
                        forgetCompatible
                            (blankNode
                                [ predicate predicateCurrent objectCurrentE
                                ]
                            )
                    )
                    objectE
                    predicates
                )

        Nothing ->
            Encoder
                (PropertyEncoder
                    (\seed _ ->
                        -- XXX `propertyPath` contains non-`Iri` components, we just give up..
                        ( RDF.emptyGraph, seed )
                    )
                )


predicate : Predicate -> IsGraphOrLiteralEncoder object -> PropertyEncoder
predicate predicate_ (Encoder objectE) =
    Encoder
        (PropertyEncoder
            (\seed subject ->
                case objectE of
                    GraphEncoder f ->
                        let
                            ( object_, ( graphObject, seedUpdated ) ) =
                                f seed

                            ( graphProperty, seedUpdatedUpdated ) =
                                case predicate predicate_ (forgetCompatible (iri (RDF.forgetCompatible object_))) of
                                    Encoder propertyE ->
                                        case propertyE of
                                            PropertyEncoder g ->
                                                g seedUpdated subject

                                            GraphEncoder _ ->
                                                ( RDF.emptyGraph, seedUpdated )

                                            LiteralEncoder _ ->
                                                ( RDF.emptyGraph, seedUpdated )
                        in
                        ( RDF.union graphObject graphProperty, seedUpdatedUpdated )

                    LiteralEncoder f ->
                        f seed subject predicate_

                    PropertyEncoder _ ->
                        ( RDF.emptyGraph, seed )
            )
        )


{-| TODO
-}
object : Object -> LiteralEncoder
object object_ =
    Encoder
        (LiteralEncoder
            (\seed subject predicate_ ->
                ( RDF.singleton subject predicate_ object_, seed )
            )
        )


{-| TODO
-}
literal : RDF.Literal a -> LiteralEncoder
literal =
    object << RDF.forgetCompatible


{-| TODO
-}
iri : RDF.Iri -> LiteralEncoder
iri =
    object << RDF.forgetCompatible


{-| TODO
-}
type alias IsGraphEncoder compatible =
    Encoder { compatible | isGraph : Yes }


{-| TODO
-}
type alias GraphEncoder =
    Encoder
        { isGraph : Yes
        , isProperty : No
        , isLiteral : No
        , isGraphOrLiteral : Yes
        }


{-| TODO
-}
type alias IsPropertyEncoder compatible =
    Encoder { compatible | isProperty : Yes }


{-| TODO
-}
type alias PropertyEncoder =
    Encoder
        { isGraph : No
        , isProperty : Yes
        , isLiteral : No
        , isGraphOrLiteral : No
        }


{-| TODO
-}
type alias IsLiteralEncoder compatible =
    Encoder { compatible | isLiteral : Yes }


{-| TODO
-}
type alias LiteralEncoder =
    Encoder
        { isGraph : No
        , isProperty : No
        , isLiteral : Yes
        , isGraphOrLiteral : Yes
        }


{-| TODO
-}
type alias IsGraphOrLiteralEncoder compatible =
    Encoder { compatible | isGraphOrLiteral : Yes }


type Yes
    = Yes Never


type No
    = No Never


forgetCompatible : Encoder compatible1 -> Encoder compatible2
forgetCompatible (Encoder encoder) =
    Encoder encoder
