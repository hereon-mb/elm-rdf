module Rdf.Encode.Bunch exposing
    ( Tree(..), Forest
    , reduce
    )

{-|

@docs Tree, Forest
@docs reduce

-}

import Basics.Extra exposing (flip)
import List.Extra as List
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Result.Extra as Result


type Tree node leaf
    = Node node (NonEmpty (Tree node leaf))
    | Leaf leaf


type alias Forest node leaf =
    List (Tree node leaf)


reduce : List ( NonEmpty node, leaf ) -> Forest node ( node, leaf )
reduce xs_ =
    let
        xs : ( List ( ( node, NonEmpty node ), leaf ), List ( node, leaf ) )
        xs =
            partition xs_

        ( nodes, leafs ) =
            Tuple.mapFirst
                (List.unique << List.map (Tuple.first << Tuple.first))
                xs
    in
    List.map Leaf leafs
        ++ List.filterMap
            (\node ->
                Maybe.map (Node node)
                    (NonEmpty.fromList
                        (reduce (restrict node (Tuple.first xs)))
                    )
            )
            nodes


partition :
    List ( NonEmpty node, leaf )
    -> ( List ( ( node, NonEmpty node ), leaf ), List ( node, leaf ) )
partition =
    Result.partition
        << List.map
            (\( ( node, nodes ), value ) ->
                Maybe.withDefault (Err ( node, value ))
                    (Maybe.map
                        (Ok << flip Tuple.pair value << Tuple.pair node)
                        (NonEmpty.fromList nodes)
                    )
            )


restrict :
    node
    -> List ( ( node, NonEmpty node ), leaf )
    -> List ( NonEmpty node, leaf )
restrict node0 =
    List.filterMap
        (\( ( node, nodes ), leaf ) ->
            if node == node0 then
                Just ( nodes, leaf )

            else
                Nothing
        )
