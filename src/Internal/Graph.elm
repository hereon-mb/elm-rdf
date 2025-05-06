module Internal.Graph exposing
    ( Graph(..)
    , Data
    )

{-|

@docs Graph
@docs Data

-}

import Dict exposing (Dict)
import Rdf


type Graph
    = Graph Data


type alias Data =
    { base : Maybe String
    , prefixes : Dict String String
    , triples : List Rdf.Triple
    , subjects : List Rdf.BlankNodeOrIri
    , objects : List Rdf.BlankNodeOrIriOrAnyLiteral
    , bySubjectByPredicate : Dict String (Dict String (List Rdf.Triple))
    , byPredicateBySubject : Dict String (Dict String (List Rdf.Triple))
    }
