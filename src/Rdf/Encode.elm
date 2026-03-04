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
    , from, fromBlankNode
    )

{-| A domain-specific language for encoding `Graph`s.


# Introduction

The minimal motivation for this module is to encode the following example in an
ergonomic DSL. Note that in this example, blank nodes are nested.

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

  - blank nodes may appear as root nodes (subjects) as well as arguments to
    `property` (object nodes)

The following are deliberate limitations of the current implementation:

  - only one node can be encoded (easily solvable)

  - blank nodes cannot freely refer to each other (unclear if solvable)

Implementation-wise, the only challenge is the fact that blank nodes can be
subjects as well as objects. We deploy the same technique as `RDF` does for
typing `Node`s.


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


## Experimental

@docs from, fromBlankNode

-}

import List.NonEmpty as NonEmpty
import Rdf exposing (IsBlankNodeOrIriOrLiteral, IsIriOrPath, Literal)
import Rdf.Encode.Bunch as Bunch
import Rdf.Graph as Rdf exposing (Graph, Seed)
import Rdf.Predicate as Predicate


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
    Rdf.BlankNodeOrIriOrLiteral


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
                        Tuple.mapFirst Rdf.asBlankNodeOrIri
                            (Rdf.generateBlankNode seed)
                in
                nodeHelp propertyEs subject seedUpdated
            )
        )


{-| TODO
-}
from :
    Rdf.IsBlankNodeOrIri compatible
    -> List PropertyEncoder
    -> PropertyEncoder
from subject propertyEs =
    Encoder
        (GraphEncoder
            (\seed ->
                nodeHelp propertyEs subject seed
            )
        )


{-| TODO
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


{-| TODO
-}
node : Rdf.IsBlankNodeOrIri compatible -> List PropertyEncoder -> GraphEncoder
node subject propertyEs =
    Encoder (GraphEncoder (\seed -> nodeHelp propertyEs subject seed))


nodeHelp :
    List PropertyEncoder
    -> Rdf.IsBlankNodeOrIri compatible
    -> Seed
    -> ( Subject, ( Graph, Seed ) )
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

                    LiteralEncoder _ ->
                        ( graphs, seedNext )
            )
            ( [], seed )
            propertyEs
    )


{-| TODO
-}
bunch :
    List ( IsIriOrPath compatible, IsGraphOrLiteralEncoder object )
    -> List PropertyEncoder
bunch =
    List.filterMap
        (\( propertyPath, encoder ) ->
            propertyPath
                |> Predicate.fromPropertyPath
                |> Maybe.map (\p -> ( p, encoder ))
        )
        >> Bunch.reduce
        >> List.map encoderFromTree


encoderFromTree :
    Bunch.Tree
        Predicate.Predicate
        ( Predicate.Predicate, IsGraphOrLiteralEncoder object )
    -> PropertyEncoder
encoderFromTree tree =
    case tree of
        Bunch.Leaf ( p, encoder ) ->
            encoderFromPredicate p encoder

        Bunch.Node p trees ->
            NonEmpty.toList trees
                |> List.map encoderFromTree
                |> blankNode
                |> encoderFromPredicate p


encoderFromPredicate :
    Predicate.Predicate
    -> IsGraphOrLiteralEncoder object
    -> PropertyEncoder
encoderFromPredicate p encoder =
    case p of
        Predicate.Predicate predicate_ ->
            predicate predicate_ encoder

        Predicate.Inverse predicate_ ->
            inverse predicate_ encoder


{-| TODO
-}
property :
    IsIriOrPath compatible
    -> IsGraphOrLiteralEncoder object
    -> PropertyEncoder
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


{-| TODO
-}
predicate :
    Rdf.IsIri compatible
    -> IsGraphOrLiteralEncoder object
    -> PropertyEncoder
predicate p (Encoder encoder) =
    let
        encoderNew : Seed -> Subject -> ( Graph, Seed )
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

                LiteralEncoder f ->
                    f seed subject (Rdf.asIri p)
    in
    Encoder (PropertyEncoder encoderNew)


{-| TODO
-}
inverse : Predicate -> IsGraphOrLiteralEncoder object -> PropertyEncoder
inverse p (Encoder encoder) =
    let
        encoderNew : Seed -> IsBlankNodeOrIriOrLiteral compatible -> ( Graph, Seed )
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

                LiteralEncoder _ ->
                    -- Since RDF Literals can never be the subject of a Triple,
                    -- this case should not happen
                    ( Rdf.emptyGraph, seed )

                PropertyEncoder _ ->
                    ( Rdf.emptyGraph, seed )
    in
    Encoder (PropertyEncoder encoderNew)


{-| TODO
-}
object : Rdf.IsBlankNodeOrIriOrLiteral compatible -> LiteralEncoder
object object_ =
    Encoder
        (LiteralEncoder
            (\seed subject predicate_ ->
                ( Rdf.singleton subject predicate_ object_, seed )
            )
        )


{-| TODO
-}
literal : Literal -> LiteralEncoder
literal =
    object << Rdf.asBlankNodeOrIriOrLiteral


{-| TODO
-}
iri : Rdf.Iri -> LiteralEncoder
iri =
    object << Rdf.asBlankNodeOrIriOrLiteral


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
