[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.18978208.svg)](https://doi.org/10.5281/zenodo.18978208) [![pipeline status](https://codebase.helmholtz.cloud/hereon-mb/elm-rdf/badges/main/pipeline.svg)](https://codebase.helmholtz.cloud/hereon-mb/elm-rdf/-/commits/main)  [![Latest Release](https://codebase.helmholtz.cloud/hereon-mb/elm-rdf/-/badges/release.svg)](https://codebase.helmholtz.cloud/hereon-mb/elm-rdf/-/releases)

# RDF in Elm

Work with [Resource Description
Framework](https://www.w3.org/TR/rdf11-primer/) (RDF) data in Elm. Use
this package to convert Elm values into RDF graphs or vice versa, and
build [SPARQL](https://www.w3.org/TR/sparql11-query/) queries.


## Example

Say we have some data about [Alice, Bob and the Mona
Lisa](https://www.w3.org/TR/rdf11-primer/#section-data-model). With this
package we can parse its Turtle serialization and decode everything into
Elm values.

```elm
module Data exposing (persons)

import Rdf exposing (Iri)
import Rdf.Decode as Decode exposing (Decoder)
import Rdf.Graph as Graph
import Time


-- Start with the RDF graph in Turtle

data : String
data =
    """
    @base <http://example.org/> .
    @prefix dbpedia: <http://dbpedia.org/resource/> .
    @prefix foaf: <http://xmlns.com/foaf/0.1/> .
    @prefix schema: <http://schema.org/> .
    @prefix terms: <http://purl.org/dc/terms/> .
    @prefix wikidata: <http://www.wikidata.org/entity/> .
    @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

    <bob#me> a foaf:Person ;
      foaf:knows <alice#me> ;
      schema:birthDate "1990-07-04"^^xsd:date ;
      foaf:topic_interest wikidata:Q12418 .

    wikidata:Q12418
      terms:title "Mona Lisa" ;
      terms:creator dbpedia:Leonardo_da_Vinci .

    <http://data.europeana.eu/item/04802/243FA8618938F4117025F17A8B813C5F9AA4D619> terms:subject wikidata:Q12418 .
  """


-- Define Elm types to model the data

type alias Person =
    { iri : Iri
    , knows : List Iri
    , birthDate : Time.Posix
    , topic_interest : Topic
    }


type alias Topic =
    { iri : Iri
    , title : String
    , creator : Iri
    , subjectOf : List Iri
    }


-- Write a decoder

decoder : Decoder (List Person)
decoder =
    Decode.from (foaf "Person")
        (Decode.property (Rdf.inverse Rdf.a)
            (Decode.many decoderPerson)
        )

decoderPerson : Decoder Person
decoderPerson =
    Decode.succeed Person
        |> Decode.custom Decode.iri
        |> Decode.required (foaf "knows") (Decode.many Decode.iri)
        |> Decode.required (schema "birthDate") Decode.date
        |> Decode.required (foaf "topic_interest") decoderTopic

decoderTopic : Decoder Topic
decoderTopic =
    Decode.succeed Topic
        |> Decode.custom Decode.iri
        |> Decode.required (terms "title") Decode.string
        |> Decode.required (terms "creator") Decode.iri
        |> Decode.required (Rdf.inverse (terms "subject"))
            (Decode.many Decode.iri)


-- Finally, parse the RDF graph and
-- run the decoder to extract the data

persons : Result Decode.Error (List Person)
persons =
    data
        |> Graph.parse
        |> Result.withDefault Graph.emptyGraph
        |> Decode.decode
            (Decode.from (foaf "Person")
                (Decode.property (Rdf.inverse Rdf.a)
                    (Decode.many decoder)
                )
            )
--> Ok
-->   [ { iri = example "bob"
-->     , knows = [ example "alice" ]
-->     , birthDate = Time.millisToPosix 647049600000
-->     , topic_interest =
-->         { iri = wikidata "Q12418"
-->         , title = "Mona Lisa"
-->         , creator = dbpedia "Leonardo_da_Vinci"
-->         , subjectOf = [ Rdf.iri "http://data.europeana.eu/item/04802/243FA8618938F4117025F17A8B813C5F9AA4D619" ]
-->         }
-->     }
-->   ]



dbpedia : String -> Iri
dbpedia name =
    Rdf.iri ("http://dbpedia.org/resource/" ++ name)


foaf : String -> Iri
foaf name =
    Rdf.iri ("http://xmlns.com/foaf/0.1/" ++ name)


schema : String -> Iri
schema name =
    Rdf.iri ("http://schema.org/" ++ name)


terms : String -> Iri
terms name =
    Rdf.iri ("http://purl.org/dc/terms/" ++ name)


wikidata : String -> Iri
wikidata name =
    Rdf.iri ("http://www.wikidata.org/entity/" ++ name)
```


## Features and future plans

We don't consider this package to be feature complete. It is being
actively developed as part of the semantic electronic lab notebook and
research database
[Herbie](http://codebase.helmholtz.cloud/hereon-mb/herbie). Currently,
the following things are supported:

- Parsing of [Turtle](https://www.w3.org/TR/turtle/) and
  [N-Triples](https://www.w3.org/TR/n-triples/) files, and serialization
  into these formats.
- Elm data model for RDF terms (`Iri`, `BlankNode`, `Literal`, ...) and
  graphs (`Graph`).
- Conversion between `Graph`'s and Elm values via
  [elm-json](https://package.elm-lang.org/packages/elm/json/latest/)-style
  decoders and encoders.
- Combinators for building
  [SPARQL](https://www.w3.org/TR/sparql11-query/) queries.

Here is an incomplete and unprioritized list of additional things which
could go in this package:

- Parsers and serializers for all [RDF serialization
  formats](https://www.w3.org/TR/rdf11-primer/#section-graph-syntax).
- JSON decoders and encoders for the [RDFJS](https://rdf.js.org/) data
  model.
- Support for [RDF
  Dataset](https://www.w3.org/TR/rdf11-concepts/#section-dataset)'s
- Extending the SPARQL DSL to support all SPARQL query forms.
- Performance improvements of existing parsers, decoders and encoders.
- HTTP helper functions for the [SPARQL
  Protocol](https://www.w3.org/TR/sparql11-protocol/) and the [SPARQL
  Graph Store HTTP
  Protocol](https://www.w3.org/TR/sparql11-http-rdf-update/)

And here is a list of other interesting topics concerning RDF and Elm,
which could go into separate packages:

- [SHACL](https://www.w3.org/TR/shacl/) validator
- SPARQL query engine


## Contributing

If you want to contribute, please reach out to us at
fabian.kirchner@hereon.de. You can open issues or merge requests on
https://codebase.helmholtz.cloud/hereon-mb/elm-rdf.


## License information

Copyright 2024-2026 Helmholtz-Zentrum hereon GmbH

Licensed under the Apache License, Version 2.0 (the "License"); you may
not use this package except in compliance with the License. You may
obtain a copy of the License at
http://www.apache.org/licenses/LICENSE-2.0. Unless required by
applicable law or agreed to in writing, software distributed under the
License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
CONDITIONS OF ANY KIND, either express or implied. See the License for
the specific language governing permissions and limitations under the
License.
