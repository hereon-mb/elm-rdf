module Rdf.Encode exposing
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
    , bunch
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
                [ property (om "hasNumericalValue") (Rdf.string "3.14")
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
@docs bunch

@docs iri
@docs literal
@docs object

-}

import Basics.Extra exposing (flip)
import List.NonEmpty as NonEmpty
import Rdf
import Rdf.Encode.Bunch as Bunch
import Rdf.Graph as Rdf exposing (Graph, Seed)
import Rdf.PropertyPath as PropertyPath exposing (PropertyPath)


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
    Rdf.Iri


{-| TODO
-}
type alias Subject =
    Rdf.BlankNodeOrIri


{-| TODO
-}
type alias Object =
    Rdf.BlankNodeOrIriOrAnyLiteral


{-| TODO
-}
encode : Seed -> IsGraphEncoder graph -> ( Graph, Seed )
encode seed (Encoder encoder) =
    case encoder of
        GraphEncoder f ->
            Tuple.second (f seed)

        PropertyEncoder _ ->
            ( Rdf.emptyGraph, seed )

        LiteralEncoder _ ->
            ( Rdf.emptyGraph, seed )


{-| TODO
-}
blankNode : List PropertyEncoder -> GraphEncoder
blankNode propertyEs =
    Encoder
        (GraphEncoder
            (\seed ->
                let
                    ( subject, seedUpdated ) =
                        Tuple.mapFirst Rdf.asBlankNodeOrIri (Rdf.generateBlankNode seed)
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
bunch : List ( PropertyPath, IsGraphOrLiteralEncoder object ) -> List PropertyEncoder
bunch =
    List.map bunchHelp
        << Bunch.reduce
        << List.filterMap
            (\( propertyPath, object_ ) ->
                Maybe.map (flip Tuple.pair object_)
                    (PropertyPath.normalizePropertyPath propertyPath)
            )


bunchHelp :
    Bunch.Tree
        PropertyPath.ConstructablePath
        ( PropertyPath.ConstructablePath, IsGraphOrLiteralEncoder object )
    -> PropertyEncoder
bunchHelp tree =
    case tree of
        Bunch.Leaf ( constructablePath, object_ ) ->
            case constructablePath of
                PropertyPath.Predicate predicate_ ->
                    predicate predicate_ object_

                PropertyPath.Inverse predicate_ ->
                    inverse predicate_ object_

        Bunch.Node constructablePath trees ->
            case constructablePath of
                PropertyPath.Predicate predicate_ ->
                    predicate predicate_
                        (blankNode (List.map bunchHelp (NonEmpty.toList trees)))

                PropertyPath.Inverse predicate_ ->
                    inverse predicate_
                        (blankNode (List.map bunchHelp (NonEmpty.toList trees)))


{-| TODO
-}
property :
    PropertyPath
    -> IsGraphOrLiteralEncoder object
    -> PropertyEncoder
property propertyPath objectE =
    case PropertyPath.normalizePropertyPath propertyPath of
        Just ( PropertyPath.Predicate predicate_, predicates ) ->
            predicate predicate_
                (List.foldr
                    (\predicateCurrent objectCurrentE ->
                        case predicateCurrent of
                            PropertyPath.Predicate predicateCurrent_ ->
                                forgetCompatible
                                    (blankNode
                                        [ predicate predicateCurrent_ objectCurrentE
                                        ]
                                    )

                            PropertyPath.Inverse predicateCurrent_ ->
                                forgetCompatible
                                    (blankNode
                                        [ inverse predicateCurrent_ objectCurrentE
                                        ]
                                    )
                    )
                    objectE
                    predicates
                )

        Just ( PropertyPath.Inverse predicate_, predicates ) ->
            inverse predicate_
                (List.foldr
                    (\predicateCurrent objectCurrentE ->
                        case predicateCurrent of
                            PropertyPath.Predicate predicateCurrent_ ->
                                forgetCompatible
                                    (blankNode
                                        [ predicate predicateCurrent_ objectCurrentE
                                        ]
                                    )

                            PropertyPath.Inverse predicateCurrent_ ->
                                forgetCompatible
                                    (blankNode
                                        [ inverse predicateCurrent_ objectCurrentE
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
                        -- XXX `propertyPath` contains non-`Iri` components, we
                        -- just give up..
                        ( Rdf.emptyGraph, seed )
                    )
                )


{-| TODO
-}
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
                                case Rdf.toBlankNodeOrIri object_ of
                                    Nothing ->
                                        -- FIXME
                                        ( Rdf.emptyGraph, seedUpdated )

                                    Just blankNodeOrIri ->
                                        case predicate predicate_ (forgetCompatible (object (Rdf.asBlankNodeOrIriOrAnyLiteral blankNodeOrIri))) of
                                            Encoder propertyE ->
                                                case propertyE of
                                                    PropertyEncoder g ->
                                                        g seedUpdated subject

                                                    GraphEncoder _ ->
                                                        ( Rdf.emptyGraph, seedUpdated )

                                                    LiteralEncoder _ ->
                                                        ( Rdf.emptyGraph, seedUpdated )
                        in
                        ( Rdf.union graphObject graphProperty, seedUpdatedUpdated )

                    LiteralEncoder f ->
                        f seed subject predicate_

                    PropertyEncoder _ ->
                        ( Rdf.emptyGraph, seed )
            )
        )


{-| TODO
-}
inverse : Predicate -> IsGraphOrLiteralEncoder object -> PropertyEncoder
inverse predicate_ (Encoder objectE) =
    Encoder
        (PropertyEncoder
            (\seed subject ->
                case objectE of
                    GraphEncoder f ->
                        let
                            ( object_, ( graphObject, seedUpdated ) ) =
                                f seed

                            ( graphProperty, seedUpdatedUpdated ) =
                                case Rdf.toBlankNodeOrIri object_ of
                                    Nothing ->
                                        -- FIXME
                                        ( Rdf.emptyGraph, seedUpdated )

                                    Just blankNodeOrIri ->
                                        case
                                            predicate predicate_
                                                (forgetCompatible
                                                    (object
                                                        (Rdf.asBlankNodeOrIriOrAnyLiteral subject)
                                                    )
                                                )
                                        of
                                            Encoder propertyE ->
                                                case propertyE of
                                                    PropertyEncoder g ->
                                                        g seedUpdated blankNodeOrIri

                                                    GraphEncoder _ ->
                                                        ( Rdf.emptyGraph, seedUpdated )

                                                    LiteralEncoder _ ->
                                                        ( Rdf.emptyGraph, seedUpdated )
                        in
                        ( Rdf.union graphObject graphProperty, seedUpdatedUpdated )

                    LiteralEncoder f ->
                        -- f seed subject predicate_
                        ( Rdf.emptyGraph, seed )

                    PropertyEncoder _ ->
                        ( Rdf.emptyGraph, seed )
            )
        )


{-| TODO
-}
object : Object -> LiteralEncoder
object object_ =
    Encoder
        (LiteralEncoder
            (\seed subject predicate_ ->
                ( Rdf.singleton subject predicate_ object_, seed )
            )
        )


{-| TODO
-}
literal : Rdf.Literal a -> LiteralEncoder
literal =
    object << Rdf.asBlankNodeOrIriOrAnyLiteral


{-| TODO
-}
iri : Rdf.Iri -> LiteralEncoder
iri =
    object << Rdf.asBlankNodeOrIriOrAnyLiteral


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


{-| TODO
-}
type Yes
    = Yes Never


{-| TODO
-}
type No
    = No Never


forgetCompatible : Encoder compatible1 -> Encoder compatible2
forgetCompatible (Encoder encoder) =
    Encoder encoder
