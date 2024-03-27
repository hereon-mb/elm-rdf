module RDF.Namespaces exposing
    ( rdf, rdfs, xsd
    , a
    , owl, sh, dash
    , dcterms, qudt, prov
    , schema
    )

{-|

@docs rdf, rdfs, xsd
@docs a
@docs owl, sh, dash
@docs dcterms, qudt, prov
@docs schema

-}

import RDF exposing (Iri, iriAbsolute)


{-| TODO Add documentation
-}
dash : String -> Iri
dash name =
    iriAbsolute ("http://datashapes.org/dash#" ++ name)


{-| TODO Add documentation
-}
dcterms : String -> Iri
dcterms name =
    iriAbsolute ("http://purl.org/dc/terms/" ++ name)


{-| TODO Add documentation
-}
owl : String -> Iri
owl name =
    iriAbsolute ("http://www.w3.org/2002/07/owl#" ++ name)


{-| TODO Add documentation
-}
qudt : String -> Iri
qudt name =
    iriAbsolute ("http://qudt.org/schema/qudt/" ++ name)


{-| TODO Add documentation
-}
rdf : String -> Iri
rdf name =
    iriAbsolute ("http://www.w3.org/1999/02/22-rdf-syntax-ns#" ++ name)


{-| TODO Add documentation
-}
rdfs : String -> Iri
rdfs name =
    iriAbsolute ("http://www.w3.org/2000/01/rdf-schema#" ++ name)


{-| TODO Add documentation
-}
sh : String -> Iri
sh name =
    iriAbsolute ("http://www.w3.org/ns/shacl#" ++ name)


{-| TODO Add documentation
-}
xsd : String -> Iri
xsd name =
    iriAbsolute ("http://www.w3.org/2001/XMLSchema#" ++ name)


{-| TODO Add documentation
-}
prov : String -> Iri
prov name =
    iriAbsolute ("http://www.w3.org/ns/prov#" ++ name)


{-| TODO Add documentation
-}
schema : String -> Iri
schema name =
    iriAbsolute ("http://schema.org/" ++ name)


{-| TODO Add documentation
-}
a : Iri
a =
    rdf "type"
