module Rdf exposing
    ( Triple
    , Term
    , Iri, IsIri, asIri
    , BlankNode, IsBlankNode, asBlankNode
    , Literal, IsLiteral, asLiteral
    , Var, IsVar, asVar
    , Path, IsPath, asPath
    , BlankNodeOrIri, IsBlankNodeOrIri, asBlankNodeOrIri
    , BlankNodeOrIriOrLiteral, IsBlankNodeOrIriOrLiteral, asBlankNodeOrIriOrLiteral
    , BlankNodeOrIriOrLiteralOrVar, IsBlankNodeOrIriOrLiteralOrVar, asBlankNodeOrIriOrLiteralOrVar
    , BlankNodeOrIriOrVar, IsBlankNodeOrIriOrVar, asBlankNodeOrIriOrVar
    , IriOrLiteralOrVar, IsIriOrLiteralOrVar, asIriOrLiteralOrVar
    , VarOrPath, IsVarOrPath, asVarOrPath
    , Compatible
    , iri
    , blankNode
    , literal
    , varQ, varD
    , sequence, alternative, inverse, zeroOrMore, oneOrMore, zeroOrOne
    , string, langString
    , boolean
    , decimal
    , integer, int
    , double, float
    , date, dateTime
    , toIri
    , toBlankNode
    , toLiteral
    , toVar
    , toBlankNodeOrIri, toBlankNodeOrIriOrLiteral
    , toUrl
    , lexicalForm
    , toString, toLangString
    , toInt, toFloat, toDecimal
    , toDate, toDateTime
    , toBool
    , appendPath
    , dropFragment, setFragment
    , setQueryParam
    , append
    , StringOrLangString
    , localize, nonLocalized
    , stringOrLangStringFrom, stringOrLangStringFromList
    , mergeStringOrLangStrings
    , stringOrLangStringInfo
    , startsWith
    , rightOf
    , lastPredicatePath
    , serialize, serializeWith, Prologue
    , serializeTriple
    , encodeTriple
    , tripleDecoder
    )

{-| This module defines the types and helper functions to work with the basic
buildings blocks of the RDF ecosystem. If you are new to RDF, you should take
a look at the [RDF 1.1 Primer](https://www.w3.org/TR/rdf11-primer/) for
a introduction on its data model.

@docs Triple


# RDF Terms


## Types

@docs Term


### Basic

@docs Iri, IsIri, asIri
@docs BlankNode, IsBlankNode, asBlankNode
@docs Literal, IsLiteral, asLiteral
@docs Var, IsVar, asVar
@docs Path, IsPath, asPath


### Combinations

@docs BlankNodeOrIri, IsBlankNodeOrIri, asBlankNodeOrIri
@docs BlankNodeOrIriOrLiteral, IsBlankNodeOrIriOrLiteral, asBlankNodeOrIriOrLiteral
@docs BlankNodeOrIriOrLiteralOrVar, IsBlankNodeOrIriOrLiteralOrVar, asBlankNodeOrIriOrLiteralOrVar
@docs BlankNodeOrIriOrVar, IsBlankNodeOrIriOrVar, asBlankNodeOrIriOrVar
@docs IriOrLiteralOrVar, IsIriOrLiteralOrVar, asIriOrLiteralOrVar
@docs VarOrPath, IsVarOrPath, asVarOrPath


### Utility

@docs Compatible


## Create

This section contains helper functions to create [`Term`](#Term)'s from Elm
values.

@docs iri
@docs blankNode
@docs literal
@docs varQ, varD
@docs sequence, alternative, inverse, zeroOrMore, oneOrMore, zeroOrOne


### Specific literals

@docs string, langString
@docs boolean


#### Numerical

@docs decimal
@docs integer, int
@docs double, float


#### Temporal

@docs date, dateTime


## Conversions


### Specialize

This section contains helper functions to convert any [`Term`](#Term)'s into
a more specific variant.

@docs toIri
@docs toBlankNode
@docs toLiteral
@docs toVar
@docs toBlankNodeOrIri, toBlankNodeOrIriOrLiteral


### Elm values

This section contains helper functions to convert specific [`Term`](#Term)'s
into Elm values.

@docs toUrl
@docs lexicalForm
@docs toString, toLangString
@docs toInt, toFloat, toDecimal
@docs toDate, toDateTime
@docs toBool


### Transform IRI's

A few helper functions to transform [`Iri`](#Iri)'s.

@docs appendPath
@docs dropFragment, setFragment
@docs setQueryParam


### Transform query variables

@docs append


### StringOrLangString

A few convenience functions for working with literals of type `xsd:string` and
`xsd:langString`.

@docs StringOrLangString
@docs localize, nonLocalized
@docs stringOrLangStringFrom, stringOrLangStringFromList
@docs mergeStringOrLangStrings
@docs stringOrLangStringInfo


### Property paths

@docs startsWith
@docs rightOf
@docs lastPredicatePath


## Serialization

@docs serialize, serializeWith, Prologue
@docs serializeTriple


## Json

@docs encodeTriple
@docs tripleDecoder

-}

import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Internal.Term as Internal
    exposing
        ( DataLiteral
        , Term(..)
        , Variant(..)
        , toVariant
        )
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import List.NonEmpty as NonEmpty
import Maybe.Extra as Maybe
import String.Extra as String
import Time exposing (Posix)


{-| This type represents an [RDF
triple](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-rdf-triple).
It consists of

  - a subject, which must be an
    [IRI](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-iri) or
    a [blank
    node](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-blank-node),
  - a predicate, which must be an
    [IRI](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-iri),
  - an object, which can be any RDF term, i.e. an
    [IRI](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-iri),
    a [blank
    node](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-blank-node),
    or
    a [literal](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-literal).

An RDF triple describes a directed labelled node within an RDF graph, pointing
from the subject to the object and the label being the predicate.

-}
type alias Triple =
    { subject : BlankNodeOrIri
    , predicate : Iri
    , object : BlankNodeOrIriOrLiteral
    }


{-| This type represents an [RDF
Term](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-rdf-triple)
(which can be an
[IRI](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-iri),
a [literal](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-literal),
or a [blank
node](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-blank-node)),
a [query variable](https://www.w3.org/TR/sparql11-query/#sparqlQueryVariables),
or a [property path](https://www.w3.org/TR/sparql11-query/#propertypaths).
They are used in different contexts, for example when encoding or decoding RDF
graphs, or when building different parts of a SPARQL query, like WHERE,
CONSTRUCT, or SELECT clauses, as well as expressions.

The `compatible` type variable is used to ensure that certain instances of
a `Term` can only be used in the right context. For example, subjects in an
RDF graph can only be IRI's or blank nodes, or, verbs in a WHERE clause can
only be IRI's, query variables or property paths.

For each term variant and all their combinations, we define two type aliases,
one with a type variable and one without, e.g. `IsIri compatible` and `Iri`.

  - The first one will only be used for **arguments** of functions,
  - the second only for **returned values**.

Doing this makes the following code work, as the `insert` function uses the
general `Is... compatible` types.

    import Rdf.Graph as Graph exposing (Graph)

    alice : Iri
    alice =
        iri "http://example.org/alice"

    knows : Iri
    knows =
        iri "http://example.org/#knows"

    someone : BlankNode
    someone =
        blankNode "someone"

    graph : Graph
    graph =
        Graph.insert alice knows someone Graph.empty

Here is an overview of all possible terms and in which context they are
allowed:

```markdown
| Terms         | Graph | Where | Construct | Select | Expression | Path |
|               | S P O | S V O | S V O     |        |            |      |
|---------------|-------|-------|-----------|--------|------------|------|
| BlankNode     | x   x | x   x | x   x     |        |            |      |
| Iri           | x x x | x o x | x x x     |        |     x      |  o   |
| Literal       |     x |     x |     x     |        |     x      |      |
| Var           |       | x x x | x x x     |   x    |     x      |      |
| Path          |       |   x   |           |        |            |  x   |
```

It follows that a term can be compatible or not within the following
combinations:

  - [`Iri`](#Iri)
  - [`Var`](#Var)
  - [`BlankNode`](#BlankNode) or [`Iri`](#Iri)
  - [`BlankNode`](#BlankNode) or [`Iri`](#Iri) or [`Literal`](#Literal)
  - [`BlankNode`](#BlankNode) or [`Iri`](#Iri) or [`Literal`](#Literal) or [`Var`](#Var)
  - [`BlankNode`](#BlankNode) or [`Iri`](#Iri) or [`Var`](#Var)
  - [`Iri`](#Iri) or [`Literal`](#Literal) or [`Var`](#Var)
  - [`Iri`](#Iri) or [`Var`](#Var)
  - [`Var`](#Var) or [`Path`](#Path) (which includes [`Iri`](#Iri))
  - [`Path`](#Path) (which includes [`Iri`](#Iri))

We consider an IRI to be the same as a predicate path and therefore define
`VarOrPath` instead of `IriOrVarOrPath`.

-}
type alias Term compatible =
    Internal.Term compatible


{-| You can ignore this type, it is only used for implementing the different
`Term` variants.
-}
type Compatible
    = Compatible Never



-- IRI


{-| An RDF term which can only be an
[IRI](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-iri). This
type will only be used as the return type of functions and if the values need
to be stored in the model. Take a look at
[Term](Rdf#Term) for a general explanation of
the idea behind this.
-}
type alias Iri =
    Term
        { iri : Compatible
        , blankNodeOrIri : Compatible
        , blankNodeOrIriOrLiteral : Compatible
        , blankNodeOrIriOrLiteralOrVar : Compatible
        , blankNodeOrIriOrVar : Compatible
        , iriOrLiteralOrVar : Compatible
        , iriOrVar : Compatible
        , varOrPath : Compatible
        , path : Compatible
        }


{-| This type will only be used as the argument of functions and ensures that
only [IRI](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-iri)'s
are passed. Take a look at [Term](Rdf#Term) for a general explanation of the
idea behind this.
-}
type alias IsIri compatible =
    Term
        { compatible
            | iri : Compatible
            , blankNodeOrIri : Compatible
            , blankNodeOrIriOrLiteral : Compatible
            , blankNodeOrIriOrLiteralOrVar : Compatible
            , blankNodeOrIriOrVar : Compatible
            , iriOrLiteralOrVar : Compatible
            , iriOrVar : Compatible
            , varOrPath : Compatible
            , path : Compatible
        }


{-| When writing your own helper functions, it might be necessary that you have
to convert the argument type into the return value type. Take a look at
[Term](Rdf#Term) for a general explanation of
the idea behind this.
-}
asIri : IsIri compatible -> Iri
asIri (Term variant) =
    Term variant



-- BLANK NODE


{-| An RDF term which can only be a
[blank
node](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-blank-node).
This type will only be used as the return type of functions and if the values
need to be stored in the model. Take a look at [Term](Rdf#Term) for a general
explanation of the idea behind this.
-}
type alias BlankNode =
    Term
        { blankNodeOrIri : Compatible
        , blankNodeOrIriOrLiteral : Compatible
        , blankNodeOrIriOrLiteralOrVar : Compatible
        , blankNodeOrIriOrVar : Compatible
        }


{-| This type will only be used as the argument of functions and ensures that
only [blank
node](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-blank-node)'s
are passed. Take a look at [Term](Rdf#Term) for a general explanation of the
idea behind this.
-}
type alias IsBlankNode compatible =
    Term
        { compatible
            | blankNodeOrIri : Compatible
            , blankNodeOrIriOrLiteral : Compatible
            , blankNodeOrIriOrLiteralOrVar : Compatible
            , blankNodeOrIriOrVar : Compatible
        }


{-| When writing your own helper functions, it might be necessary that you have
to convert the argument type into the return value type. Take a look at
[Term](Rdf#Term) for a general explanation of the idea behind this.
-}
asBlankNode : IsBlankNode compatible -> BlankNode
asBlankNode (Term variant) =
    Term variant



-- LITERAL


{-| An RDF term which can only be a
[literal](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-literal).
This type will only be used as the return type of functions and if the values
need to be stored in the model. Take a look at [Term](Rdf#Term) for a general
explanation of the idea behind this.
-}
type alias Literal =
    Term
        { blankNodeOrIriOrLiteral : Compatible
        , blankNodeOrIriOrLiteralOrVar : Compatible
        , iriOrLiteralOrVar : Compatible
        }


{-| This type will only be used as the argument of functions and ensures that
only
[literal](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-literal)'s
are passed. Take a look at [Term](Rdf#Term) for a general explanation of the
idea behind this.
-}
type alias IsLiteral compatible =
    Term
        { compatible
            | blankNodeOrIriOrLiteral : Compatible
            , blankNodeOrIriOrLiteralOrVar : Compatible
            , iriOrLiteralOrVar : Compatible
        }


{-| When writing your own helper functions, it might be necessary that you have
to convert the argument type into the return value type. Take a look at
[Term](Rdf#Term) for a general explanation of the idea behind this.
-}
asLiteral : IsLiteral compatible -> Literal
asLiteral (Term variant) =
    Term variant



-- VAR


{-| An RDF term which can only be a
[query variable](https://www.w3.org/TR/sparql11-query/#sparqlQueryVariables).
This type will only be used as the return type of functions and if the values
need to be stored in the model. Take a look at [Term](Rdf#Term) for a general
explanation of the idea behind this.
-}
type alias Var =
    Term
        { isVar : Compatible
        , blankNodeOrIriOrLiteralOrVar : Compatible
        , blankNodeOrIriOrVar : Compatible
        , iriOrLiteralOrVar : Compatible
        , iriOrVar : Compatible
        , varOrPath : Compatible
        }


{-| This type will only be used as the argument of functions and ensures that
only [query
variable](https://www.w3.org/TR/sparql11-query/#sparqlQueryVariables)'s are
passed. Take a look at [Term](Rdf#Term) for a general explanation of the idea
behind this.
-}
type alias IsVar compatible =
    Term
        { compatible
            | isVar : Compatible
            , blankNodeOrIriOrLiteralOrVar : Compatible
            , blankNodeOrIriOrVar : Compatible
            , iriOrLiteralOrVar : Compatible
            , iriOrVar : Compatible
            , varOrPath : Compatible
        }


{-| When writing your own helper functions, it might be necessary that you have
to convert the argument type into the return value type. Take a look at
[Term](Rdf#Term) for a general explanation of the idea behind this.
-}
asVar : IsVar compatible -> Var
asVar (Term variant) =
    Term variant



-- PATH


{-| An RDF term which can only be a
[property path](https://www.w3.org/TR/sparql11-query/#propertypaths). This type
will only be used as the return type of functions and if the values need to be
stored in the model. Take a look at [Term](Rdf#Term) for a general explanation
of the idea behind this.
-}
type alias Path =
    Term
        { varOrPath : Compatible
        , path : Compatible
        }


{-| This type will only be used as the argument of functions and ensures that
only [property path](https://www.w3.org/TR/sparql11-query/#propertypaths)'s are
passed. Take a look at [Term](Rdf#Term) for a general explanation of the idea
behind this.
-}
type alias IsPath compatible =
    Term
        { compatible
            | varOrPath : Compatible
            , path : Compatible
        }


{-| When writing your own helper functions, it might be necessary that you have
to convert the argument type into the return value type. Take a look at
[Term](Rdf#Term) for a general explanation of the idea behind this.
-}
asPath : IsPath compatible -> Path
asPath (Term variant) =
    Term variant



-- BLANK NODE OR IRI


{-| An RDF term which can only be a
[blank
node](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-blank-node)
or an [IRI](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-iri).
This type will only be used as the return type of functions and if the values
need to be stored in the model. Take a look at [Term](Rdf#Term) for a general
explanation of the idea behind this.
-}
type alias BlankNodeOrIri =
    Term
        { blankNodeOrIri : Compatible
        , blankNodeOrIriOrLiteral : Compatible
        , blankNodeOrIriOrLiteralOrVar : Compatible
        , blankNodeOrIriOrVar : Compatible
        }


{-| This type will only be used as the argument of functions and ensures that
only [blank
node](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-blank-node)'s
or [IRI](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-iri)'s are
passed. Take a look at [Term](Rdf#Term) for a general explanation of the idea
behind this.
-}
type alias IsBlankNodeOrIri compatible =
    Term
        { compatible
            | blankNodeOrIri : Compatible
            , blankNodeOrIriOrLiteral : Compatible
            , blankNodeOrIriOrLiteralOrVar : Compatible
            , blankNodeOrIriOrVar : Compatible
        }


{-| When writing your own helper functions, it might be necessary that you have
to convert the argument type into the return value type. Take a look at
[Term](Rdf#Term) for a general explanation of
the idea behind this.
-}
asBlankNodeOrIri : IsBlankNodeOrIri compatible -> BlankNodeOrIri
asBlankNodeOrIri (Term variant) =
    Term variant



-- BLANK NODE OR IRI OR LITERAL


{-| An RDF term which can only be a
[blank
node](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-blank-node),
an [IRI](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-iri), or
a [literal](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-literal).
This type will only be used as the return type of functions and if the values
need to be stored in the model. Take a look at [Term](Rdf#Term) for a general
explanation of the idea behind this.
-}
type alias BlankNodeOrIriOrLiteral =
    Term
        { blankNodeOrIriOrLiteral : Compatible
        , blankNodeOrIriOrLiteralOrVar : Compatible
        }


{-| This type will only be used as the argument of functions and ensures that
only [blank
node](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-blank-node)'s,
[IRI](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-iri)'s, or
[literal](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-literal)'s
are passed. Take a look at [Term](Rdf#Term) for a general explanation of the
idea behind this.
-}
type alias IsBlankNodeOrIriOrLiteral compatible =
    Term
        { compatible
            | blankNodeOrIriOrLiteral : Compatible
            , blankNodeOrIriOrLiteralOrVar : Compatible
        }


{-| When writing your own helper functions, it might be necessary that you have
to convert the argument type into the return value type. Take a look at
[Term](Rdf#Term) for a general explanation of
the idea behind this.
-}
asBlankNodeOrIriOrLiteral : IsBlankNodeOrIriOrLiteral compatible -> BlankNodeOrIriOrLiteral
asBlankNodeOrIriOrLiteral (Term variant) =
    Term variant



-- BLANK NODE OR IRI OR LITERAL OR VAR


{-| TODO Add documentation
-}
type alias BlankNodeOrIriOrLiteralOrVar =
    Term
        { blankNodeOrIriOrLiteralOrVar : Compatible
        }


{-| TODO Add documentation
-}
type alias IsBlankNodeOrIriOrLiteralOrVar compatible =
    Term { compatible | blankNodeOrIriOrLiteralOrVar : Compatible }


{-| TODO Add documentation
-}
asBlankNodeOrIriOrLiteralOrVar :
    IsBlankNodeOrIriOrLiteralOrVar compatible
    -> BlankNodeOrIriOrLiteralOrVar
asBlankNodeOrIriOrLiteralOrVar (Term variant) =
    Term variant



-- BLANK NODE OR IRI OR VAR


{-| TODO Add documentation
-}
type alias BlankNodeOrIriOrVar =
    Term
        { blankNodeOrIriOrLiteralOrVar : Compatible
        , blankNodeOrIriOrVar : Compatible
        }


{-| TODO Add documentation
-}
type alias IsBlankNodeOrIriOrVar compatible =
    Term
        { compatible
            | blankNodeOrIriOrLiteralOrVar : Compatible
            , blankNodeOrIriOrVar : Compatible
        }


{-| TODO Add documentation
-}
asBlankNodeOrIriOrVar : IsBlankNodeOrIriOrVar compatible -> BlankNodeOrIriOrVar
asBlankNodeOrIriOrVar (Term variant) =
    Term variant



-- IRI OR LITERAL OR VAR


{-| TODO Add documentation
-}
type alias IriOrLiteralOrVar =
    Term
        { iriOrLiteralOrVar : Compatible
        }


{-| TODO Add documentation
-}
type alias IsIriOrLiteralOrVar compatible =
    Term { compatible | iriOrLiteralOrVar : Compatible }


{-| TODO Add documentation
-}
asIriOrLiteralOrVar : IsIriOrLiteralOrVar compatible -> IriOrLiteralOrVar
asIriOrLiteralOrVar (Term variant) =
    Term variant



-- VAR OR PATH


{-| TODO Add documentation
-}
type alias VarOrPath =
    Term
        { varOrPath : Compatible
        }


{-| TODO Add documentation
-}
type alias IsVarOrPath compatible =
    Term { compatible | varOrPath : Compatible }


{-| TODO Add documentation
-}
asVarOrPath : IsVarOrPath compatible -> VarOrPath
asVarOrPath (Term variant) =
    Term variant



-- CREATE


{-| Create an
[IRI](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-iri)
(Internal Resource Identifier) by providing its string representation, e.g.

    alice : Iri
    alice =
        iri "http://example.org/alice"

-}
iri : String -> Iri
iri value =
    Term (Iri value)


{-| Create a [blank
node](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-blank-node)
by providing a blank node identifier, e.g.

    unnamed : BlankNode
    unnamed =
        blankNode "unnamed"

**Warning**: When you are using this function directly, you have to ensure
yourself, that you don't run into naming conflicts. TODO Add a public function
to `Rdf.Graph` for minting blank nodes.

-}
blankNode : String -> BlankNode
blankNode value =
    Term (BlankNode value)


{-| Create
a [literal](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-literal)
by providing its [datatype
IRI](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-datatype-iri)
and its [lexical
form](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-lexical-form),
e.g.

    pi : Literal a
    pi =
        literal xsdDecimal "3.14"

    xsdDecimal : Iri
    xsdDecimal =
        iri "http://www.w3.org/2001/XMLSchema#decimal"

There are a few helper functions for creating literals of the most common
datatypes which are part of [built-in
datatypes](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#xsd-datatypes).

-}
literal : Iri -> String -> Literal
literal datatype value =
    Term
        (Literal
            { value = value
            , datatype = toUrl datatype
            , languageTag = Nothing
            }
        )


{-| Create a [query
variable](https://www.w3.org/TR/sparql11-query/#sparqlQueryVariables) of the
form `?name`.
-}
varQ : String -> Var
varQ name =
    Term (VarQ name)


{-| Create a [query
variable](https://www.w3.org/TR/sparql11-query/#sparqlQueryVariables) of the
form `$name`.
-}
varD : String -> Var
varD name =
    Term (VarD name)


{-| Create a sequence [property
path](https://www.w3.org/TR/sparql11-query/#propertypaths).
-}
sequence : IsPath compatible1 -> List (IsPath compatible2) -> Path
sequence (Term first) rest =
    Term (Sequence first (List.map toVariant rest))


{-| Create an alternative [property
path](https://www.w3.org/TR/sparql11-query/#propertypaths).
-}
alternative : IsPath compatible1 -> List (IsPath compatible2) -> Path
alternative (Term first) rest =
    Term (Alternative first (List.map toVariant rest))


{-| Create an inverse [property
path](https://www.w3.org/TR/sparql11-query/#propertypaths).
-}
inverse : IsPath compatible1 -> Path
inverse (Term nested) =
    Term (Inverse nested)


{-| Create a zero or more [property
path](https://www.w3.org/TR/sparql11-query/#propertypaths).
-}
zeroOrMore : IsPath compatible1 -> Path
zeroOrMore (Term nested) =
    Term (ZeroOrMore nested)


{-| Create a one or more [property
path](https://www.w3.org/TR/sparql11-query/#propertypaths).
-}
oneOrMore : IsPath compatible1 -> Path
oneOrMore (Term nested) =
    Term (OneOrMore nested)


{-| Create a zero or one [property
path](https://www.w3.org/TR/sparql11-query/#propertypaths).
-}
zeroOrOne : IsPath compatible1 -> Path
zeroOrOne (Term nested) =
    Term (ZeroOrOne nested)



-- CREATE


{-| Create
a [literal](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-literal)
with datatype [xsd:string](https://www.w3.org/TR/xmlschema11-2/#string).

    string "Alice Wonderland"
    --> literal
    -->   (iri "http://www.w3.org/2001/XMLSchema#string")
    -->   "Alice Wonderland"

-}
string : String -> Literal
string value =
    Term
        (Literal
            { value = value
            , datatype = urlXsdString
            , languageTag = Nothing
            }
        )


{-| Create
a [literal](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-literal)
with datatype `http://www.w3.org/1999/02/22-rdf-syntax-ns#langString`. The
first argument must be language tag as defined by
[BCP47](https://www.rfc-editor.org/info/bcp47), the second argument is the
actual string.

    serialize (langString "en" "RDF is great!")
    --> "\"RDF is great!\"@en"

-}
langString : String -> String -> Literal
langString languageTag value =
    Term
        (Literal
            { value = value
            , datatype = urlRdfLangString
            , languageTag = Just languageTag
            }
        )


{-| Create
a [literal](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-literal)
with datatype [xsd:boolean](https://www.w3.org/TR/xmlschema11-2/#boolean).

    boolean True
    --> literal
    -->   (iri "http://www.w3.org/2001/XMLSchema#boolean")
    -->   "true"

-}
boolean : Bool -> Literal
boolean value =
    Term
        (Literal
            { value =
                if value then
                    "true"

                else
                    "false"
            , datatype = urlXsdBoolean
            , languageTag = Nothing
            }
        )



-- NUMERICAL


{-| Create
a [literal](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-literal)
with datatype [xsd:decimal](https://www.w3.org/TR/xmlschema11-2/#decimal). Here
we are using the
[Decimal](https://package.elm-lang.org/packages/torreyatcitty/the-best-decimal/latest/Decimal#Decimal)
type from the
[torreyatcitty/the-best-decimal](https://package.elm-lang.org/packages/torreyatcitty/the-best-decimal/latest/)
package.

    import Decimal exposing (Decimal)

    pi : Decimal
    pi =
        3.14
            |> Decimal.fromFloat
            |> Maybe.withDefault (Decimal.fromInt 0)

    decimal pi
    --> literal
    -->   (iri "http://www.w3.org/2001/XMLSchema#decimal")
    -->   "3.14"

-}
decimal : Decimal -> Literal
decimal value =
    Term
        (Literal
            { value = Decimal.toString value
            , datatype = urlXsdDecimal
            , languageTag = Nothing
            }
        )


{-| Create
a [literal](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-literal)
with datatype [xsd:integer](https://www.w3.org/TR/xmlschema11-2/#integer).

    integer 42
    --> literal
    -->   (iri "http://www.w3.org/2001/XMLSchema#integer")
    -->   "42"

-}
integer : Int -> Literal
integer value =
    Term
        (Literal
            { value = String.fromInt value
            , datatype = urlXsdInteger
            , languageTag = Nothing
            }
        )


{-| Create
a [literal](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-int)
with datatype [xsd:int](https://www.w3.org/TR/xmlschema11-2/#int).

    int 42
    --> literal
    -->   (iri "http://www.w3.org/2001/XMLSchema#int")
    -->   "42"

-}
int : Int -> Literal
int value =
    Term
        (Literal
            { value = String.fromInt value
            , datatype = urlXsdInt
            , languageTag = Nothing
            }
        )


{-| Create
a [literal](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-literal)
with datatype [xsd:double](https://www.w3.org/TR/xmlschema11-2/#double).

    double 3.14
    --> literal
    -->   (iri "http://www.w3.org/2001/XMLSchema#double")
    -->   "3.14"

-}
double : Float -> Literal
double value =
    Term
        (Literal
            { value = String.fromFloat value
            , datatype = urlXsdDouble
            , languageTag = Nothing
            }
        )


{-| Create
a [literal](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-literal)
with datatype [xsd:float](https://www.w3.org/TR/xmlschema11-2/#float).

    float 3.14
    --> literal
    -->   (iri "http://www.w3.org/2001/XMLSchema#float")
    -->   "3.14"

-}
float : Float -> Literal
float value =
    Term
        (Literal
            { value = String.fromFloat value
            , datatype = urlXsdFloat
            , languageTag = Nothing
            }
        )



-- TEMPORAL


{-| Create
a [literal](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-literal)
with datatype [xsd:date](https://www.w3.org/TR/xmlschema11-2/#date).

    import Time

    date (Time.millisToPosix 0)
    --> literal
    -->   (iri "http://www.w3.org/2001/XMLSchema#date")
    -->   "1970-01-01"

-}
date : Posix -> Literal
date value =
    Term
        (Literal
            { value = String.left (4 + 1 + 2 + 1 + 2) (Iso8601.fromTime value)
            , datatype = urlXsdDate
            , languageTag = Nothing
            }
        )


{-| Create
a [literal](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#dfn-literal)
with datatype [xsd:dateTime](https://www.w3.org/TR/xmlschema11-2/#dateTime).

    import Time

    dateTime (Time.millisToPosix 0)
    --> literal
    -->   (iri "http://www.w3.org/2001/XMLSchema#dateTime")
    -->   "1970-01-01T00:00:00.000Z"

-}
dateTime : Posix -> Literal
dateTime value =
    Term
        (Literal
            { value = Iso8601.fromTime value
            , datatype = urlXsdDateTime
            , languageTag = Nothing
            }
        )



-- CONVERSION: SPECIALIZE


{-| Turn any [`Term`](#Term) into an [`Iri`](#Iri) if possible.
-}
toIri : Term compatible -> Maybe Iri
toIri (Term node) =
    case node of
        Iri _ ->
            Just (Term node)

        _ ->
            Nothing


{-| Turn any [`Term`](#Term) into a [`BlankNode`](#BlankNode) if possible.
-}
toBlankNode : Term compatible -> Maybe BlankNode
toBlankNode (Term variant) =
    case variant of
        BlankNode _ ->
            Just (Term variant)

        _ ->
            Nothing


{-| Turn any [`Term`](#Term) into a [`Literal`](#Literal) if
possible.
-}
toLiteral : Term compatible -> Maybe Literal
toLiteral (Term variant) =
    case variant of
        Literal _ ->
            Just (Term variant)

        _ ->
            Nothing


{-| Turn any [`Term`](#Term) into a [`Var`](#Var) if
possible.
-}
toVar : Term compatible -> Maybe Var
toVar (Term variant) =
    case variant of
        VarQ _ ->
            Just (Term variant)

        VarD _ ->
            Just (Term variant)

        _ ->
            Nothing


{-| Turn any [`Term`](#Term) into a [`BlankNodeOrIri`](#BlankNodeOrIri) if
possible.
-}
toBlankNodeOrIri : Term compatible -> Maybe BlankNodeOrIri
toBlankNodeOrIri (Term variant) =
    case variant of
        BlankNode _ ->
            Just (Term variant)

        Iri _ ->
            Just (Term variant)

        _ ->
            Nothing


{-| Turn any [`Term`](#Term) into
a [`BlankNodeOrIriOrLiteral`](#BlankNodeOrIriOrLiteral) if possible.
-}
toBlankNodeOrIriOrLiteral : Term compatible -> Maybe BlankNodeOrIriOrLiteral
toBlankNodeOrIriOrLiteral (Term variant) =
    case variant of
        BlankNode _ ->
            Just (Term variant)

        Iri _ ->
            Just (Term variant)

        _ ->
            Nothing



-- CONVERSION: ELM VALUES


{-| Extract the URL from an [`Iri`](#Iri).

    toUrl (iri "http://example.org")
    --> "http://example.org"

-}
toUrl : Iri -> String
toUrl (Term variant) =
    case variant of
        Iri url ->
            url

        _ ->
            ""


{-| Turn [`Literal`](#Literal) into it's [lexical
form](https://www.w3.org/TR/rdf11-concepts/#section-Graph-Literal).

    lexicalForm (double 3.14)
    --> "3.14"

    lexicalForm (string "RDF is great!")
    --> "RDF is great!"

    lexicalForm (boolean True)
    --> "true"

-}
lexicalForm : Literal -> String
lexicalForm (Term variant) =
    case variant of
        Literal { value } ->
            value

        _ ->
            ""


{-| Take any [`Term`](#Term) and extract its String value if it is a literal
with datatype `xsd:string`.
-}
toString : Term compatible -> Maybe String
toString (Term variant) =
    case variant of
        Literal data ->
            if data.datatype == urlXsdString then
                Just data.value

            else
                Nothing

        _ ->
            Nothing


{-| Take any [`Term`](#Term) and extract its language tag and String value if
it is a literal with datatype `xsd:langString`.
-}
toLangString : Term compatible -> Maybe ( String, String )
toLangString (Term variant) =
    case variant of
        Literal data ->
            if data.datatype == urlRdfLangString then
                Maybe.map2 Tuple.pair data.languageTag (Just data.value)

            else
                Nothing

        _ ->
            Nothing


{-| Take any [`Term`](#Term) and extract its Int value if it is a literal
with datatype `xsd:integer` or `xsd:int`.
-}
toInt : Term compatible -> Maybe Int
toInt (Term variant) =
    case variant of
        Literal data ->
            if data.datatype == urlXsdInt then
                String.toInt data.value

            else if data.datatype == urlXsdInteger then
                String.toInt data.value

            else
                Nothing

        _ ->
            Nothing


{-| Take any [`Term`](#Term) and extract its Float value if it is a literal
with datatype `xsd:double` or `xsd:float`.
-}
toFloat : Term compatible -> Maybe Float
toFloat (Term variant) =
    case variant of
        Literal data ->
            if data.datatype == urlXsdDouble then
                String.toFloat data.value

            else if data.datatype == urlXsdFloat then
                String.toFloat data.value

            else
                Nothing

        _ ->
            Nothing


{-| Take any [`Term`](#Term) and extract its Decimal value if it is a literal
with datatype `xsd:decimal`.
-}
toDecimal : Term compatible -> Maybe Decimal
toDecimal (Term variant) =
    case variant of
        Literal data ->
            if data.datatype == urlXsdDecimal then
                Decimal.fromString data.value

            else
                Nothing

        _ ->
            Nothing


{-| Take any [`Term`](#Term) and extract its Posix value if it is a literal
with datatype `xsd:date`.
-}
toDate : Term compatible -> Maybe Posix
toDate (Term variant) =
    case variant of
        Literal data ->
            if data.datatype == urlXsdDate then
                (data.value ++ "T00:00:00.000Z")
                    |> Iso8601.toTime
                    |> Result.toMaybe

            else
                Nothing

        _ ->
            Nothing


{-| Take any [`Term`](#Term) and extract its Posix value if it is a literal
with datatype `xsd:dateTime`.
-}
toDateTime : Term compatible -> Maybe Posix
toDateTime (Term variant) =
    case variant of
        Literal data ->
            if data.datatype == urlXsdDateTime then
                data.value
                    |> Iso8601.toTime
                    |> Result.toMaybe

            else
                Nothing

        _ ->
            Nothing


{-| Take any [`Term`](#Term) and extract its Bool value if it is a literal
with datatype `xsd:boolean`.
-}
toBool : Term compatible -> Maybe Bool
toBool (Term variant) =
    case variant of
        Literal data ->
            if data.datatype == urlXsdBoolean then
                case data.value of
                    "true" ->
                        Just True

                    "false" ->
                        Just False

                    _ ->
                        Nothing

            else
                Nothing

        _ ->
            Nothing



-- TRANSFORM


{-| Append a String to the path component of an [`Iri`](#Iri).

    appendPath "/alice" (iri "http://example.org")
    --> iri "http://example.org/alice"

    appendPath "/datasets" (iri "http://example.org?sort=id")
    --> iri "http://example.org/datasets?sort=id"

    appendPath "/people" (iri "http://example.org#alice")
    --> iri "http://example.org/people#alice"

-}
appendPath : String -> IsIri compatible -> Iri
appendPath segment (Term variant) =
    case variant of
        Iri url ->
            case String.split "?" url of
                [ _ ] ->
                    case String.split "#" url of
                        [ _ ] ->
                            Term (Iri (url ++ segment))

                        [ beforeFragment, fragment ] ->
                            Term (Iri (beforeFragment ++ segment ++ "#" ++ fragment))

                        _ ->
                            Term (Iri (url ++ segment))

                [ beforeQuery, rest ] ->
                    case String.split "#" rest of
                        [ _ ] ->
                            Term (Iri (beforeQuery ++ segment ++ "?" ++ rest))

                        [ query, fragment ] ->
                            Term (Iri (beforeQuery ++ segment ++ "?" ++ query ++ "#" ++ fragment))

                        _ ->
                            Term (Iri (url ++ segment))

                _ ->
                    Term (Iri (url ++ segment))

        _ ->
            Term variant


{-| Drop the fragment from an [`Iri`](#Iri).

    dropFragment (iri "http://example.org#alice")
    --> iri "http://example.org"

-}
dropFragment : IsIri compatible -> Iri
dropFragment (Term variant) =
    case variant of
        Iri url ->
            case String.split "#" url of
                [ _ ] ->
                    Term variant

                [ beforeFragment, _ ] ->
                    Term (Iri beforeFragment)

                _ ->
                    Term variant

        _ ->
            Term variant


{-| Set the fragment of an [`Iri`](#Iri)

    setFragment "alice" (iri "http://example.org")
    --> iri "http://example.org#alice"

    setFragment "bob" (iri "http://example.org#alice")
    --> iri "http://example.org#bob"

-}
setFragment : String -> IsIri compatible -> Iri
setFragment fragment (Term variant) =
    case variant of
        Iri url ->
            case String.split "#" url of
                [ _ ] ->
                    Term (Iri (url ++ "#" ++ fragment))

                [ beforeFragment, _ ] ->
                    Term (Iri (beforeFragment ++ "#" ++ fragment))

                _ ->
                    Term variant

        _ ->
            Term variant


{-| Set a query parameter in an [`Iri`](#Iri)

    setQueryParam "sort" "id" (iri "http://example.org")
    --> iri "http://example.org?sort=id"

    setQueryParam "sort" "name" (iri "http://example.org?sort=id")
    --> iri "http://example.org?sort=name"

-}
setQueryParam : String -> String -> IsIri compatible -> Iri
setQueryParam name value (Term variant) =
    case variant of
        Iri url ->
            case String.split "?" url of
                [ beforeQuery ] ->
                    let
                        queryString : String
                        queryString =
                            Dict.singleton name value
                                |> serializeQuery
                    in
                    Term
                        (Iri
                            (beforeQuery
                                ++ "?"
                                ++ queryString
                            )
                        )

                [ beforeQuery, rest ] ->
                    case String.split "#" rest of
                        [ queryString ] ->
                            -- FIXME
                            let
                                queryStringUpdated : String
                                queryStringUpdated =
                                    queryString
                                        |> parseQuery
                                        |> Dict.insert name value
                                        |> serializeQuery
                            in
                            Term
                                (Iri
                                    (beforeQuery
                                        ++ "?"
                                        ++ queryStringUpdated
                                    )
                                )

                        [ _, fragment ] ->
                            let
                                queryStringUpdated : String
                                queryStringUpdated =
                                    rest
                                        |> parseQuery
                                        |> Dict.insert name value
                                        |> serializeQuery
                            in
                            Term
                                (Iri
                                    (beforeQuery
                                        ++ "?"
                                        ++ queryStringUpdated
                                        ++ "#"
                                        ++ fragment
                                    )
                                )

                        _ ->
                            Term variant

                _ ->
                    Term variant

        _ ->
            Term variant


parseQuery : String -> Dict String String
parseQuery queryString =
    queryString
        |> String.split "&"
        |> List.map (String.split "=")
        |> List.filterMap
            (\param ->
                case param of
                    [ name ] ->
                        Just ( name, name )

                    [ name, value ] ->
                        Just ( name, value )

                    _ ->
                        Nothing
            )
        |> Dict.fromList


serializeQuery : Dict String String -> String
serializeQuery query =
    query
        |> Dict.toList
        |> List.map (\( name, value ) -> name ++ "=" ++ value)
        |> String.join "&"



-- TRANSFORM QUERY VARIABLES


{-| Append a `String` to a query variable.

    append "Label" (varQ "instance")
    --> varQ "instanceLabel"


    append "Label" (varD "instance")
    --> varD "instanceLabel"

-}
append : String -> IsVar compatible -> IsVar compatible
append suffix ((Term variant) as term) =
    case variant of
        VarQ var ->
            Term (VarQ (var ++ suffix))

        VarD var ->
            Term (VarD (var ++ suffix))

        _ ->
            term



-- STRING OR LANG STRING


{-| TODO Add documentation
-}
type StringOrLangString
    = StringOrLangString
        { string : Maybe String
        , langStrings : Dict String String
        }


{-| TODO Add documention
-}
stringOrLangStringInfo :
    StringOrLangString
    ->
        { string : Maybe String
        , langStrings : Dict String String
        }
stringOrLangStringInfo (StringOrLangString stringOrLangString) =
    stringOrLangString


{-| TODO Add documentation
-}
localize : String -> StringOrLangString -> Maybe String
localize locale (StringOrLangString stringOrLangString) =
    [ Dict.get locale stringOrLangString.langStrings
    , Dict.get "en" stringOrLangString.langStrings
    , stringOrLangString.string
    ]
        |> Maybe.orList


{-| TODO Add documentation
-}
nonLocalized : StringOrLangString -> Maybe String
nonLocalized (StringOrLangString stringOrLangString) =
    stringOrLangString.string


{-| TODO Add documentation
-}
stringOrLangStringFrom : Maybe String -> List ( String, String ) -> StringOrLangString
stringOrLangStringFrom maybeString langStrings =
    StringOrLangString
        { string = maybeString
        , langStrings = Dict.fromList langStrings
        }


{-| TODO Add documentation
-}
stringOrLangStringFromList : List ( String, String ) -> StringOrLangString
stringOrLangStringFromList langStrings =
    StringOrLangString
        { string = Nothing
        , langStrings = Dict.fromList langStrings
        }


{-| TODO Add documentation
-}
mergeStringOrLangStrings : List StringOrLangString -> Maybe StringOrLangString
mergeStringOrLangStrings stringOrLangStrings =
    if List.isEmpty stringOrLangStrings then
        Nothing

    else
        { string =
            stringOrLangStrings
                |> List.filterMap
                    (\(StringOrLangString stringOrLangString) -> stringOrLangString.string)
                |> List.head
        , langStrings =
            stringOrLangStrings
                |> List.map
                    (\(StringOrLangString stringOrLangString) -> stringOrLangString.langStrings)
                |> List.foldr Dict.union Dict.empty
        }
            |> StringOrLangString
            |> Just



-- PROPERTY PATHS


{-| TODO Explain properties.
-}
normalize : IsPath compatible -> Path
normalize ((Term variant) as path) =
    case variant of
        Iri _ ->
            asPath path

        Sequence first rest ->
            case rest of
                [] ->
                    normalize (Term first)

                _ ->
                    (first :: rest)
                        |> flatten
                        |> NonEmpty.fromList
                        |> Maybe.map
                            (\( firstFlat, restFlat ) ->
                                Term
                                    (Sequence
                                        (toVariant (normalize (Term firstFlat)))
                                        (List.map (Term >> normalize >> toVariant) restFlat)
                                    )
                            )
                        |> Maybe.withDefault (asPath path)

        Alternative _ _ ->
            asPath path

        Inverse _ ->
            asPath path

        ZeroOrMore _ ->
            asPath path

        OneOrMore _ ->
            asPath path

        ZeroOrOne _ ->
            asPath path

        _ ->
            asPath path


flatten : List Variant -> List Variant
flatten paths =
    List.concatMap
        (\path ->
            case path of
                Sequence first rest ->
                    first :: rest

                _ ->
                    [ path ]
        )
        paths


{-| TODO Add documentation
-}
startsWith : IsPath compatible1 -> IsPath compatible2 -> Bool
startsWith a b =
    startsWithHelp (normalize a) (normalize b)


startsWithHelp : IsPath compatible1 -> IsPath compatible2 -> Bool
startsWithHelp (Term a) (Term b) =
    case a of
        Iri iriA ->
            case b of
                Iri iriB ->
                    iriA == iriB

                Sequence (Iri iriB) _ ->
                    iriA == iriB

                _ ->
                    False

        Sequence (Iri iriA) restA ->
            case b of
                Iri _ ->
                    False

                Sequence (Iri iriB) restB ->
                    (iriA == iriB)
                        && (List.zip restA restB
                                |> List.map
                                    (\( nextA, nextB ) ->
                                        startsWithHelp (Term nextA) (Term nextB)
                                    )
                                |> List.all identity
                           )

                _ ->
                    -- FIXME Implement
                    False

        _ ->
            -- FIXME Implement
            False


{-| TODO Add documentation
-}
rightOf : IsPath compatible1 -> IsPath compatible2 -> Maybe Path
rightOf a b =
    rightOfHelp (normalize a) (normalize b)


{-| TODO Add documentation
-}
lastPredicatePath : IsPath compatible -> Maybe Iri
lastPredicatePath (Term variant) =
    case variant of
        Iri _ ->
            Just (Term variant)

        Sequence first rest ->
            case rest of
                [] ->
                    lastPredicatePath (Term first)

                _ ->
                    rest
                        |> List.reverse
                        |> List.head
                        |> Maybe.andThen (Term >> lastPredicatePath)

        _ ->
            Nothing


rightOfHelp : IsPath compatible1 -> IsPath compatible2 -> Maybe Path
rightOfHelp (Term a) (Term b) =
    case a of
        Iri iriA ->
            case b of
                Iri _ ->
                    Nothing

                Sequence (Iri iriB) (firstB :: restB) ->
                    if iriA == iriB then
                        Just (Term (Sequence firstB restB))

                    else
                        Nothing

                _ ->
                    Nothing

        Sequence (Iri iriA) restA ->
            case b of
                Iri _ ->
                    Nothing

                Sequence (Iri iriB) restB ->
                    if
                        (iriA == iriB)
                            && (List.zip restA restB
                                    |> List.map
                                        (\( nextA, nextB ) ->
                                            startsWithHelp (Term nextA) (Term nextB)
                                        )
                                    |> List.all identity
                               )
                    then
                        case List.drop (List.length restA) restB of
                            firstB :: restRestB ->
                                Just (Term (Sequence firstB restRestB))

                            _ ->
                                Nothing

                    else
                        Nothing

                _ ->
                    -- FIXME Implement
                    Nothing

        _ ->
            -- FIXME Implement
            Nothing



-- SERIALIZE


{-| Serialize a [`Term`](#Term) into how it would be represented in the
[Turtle](https://www.w3.org/TR/turtle/) format or in
[SPARQL](https://www.w3.org/TR/sparql11-query/).
-}
serialize : Term compatible -> String
serialize (Term variant) =
    Internal.serializeVariant variant


{-| Serialize a [`Term`](#Term) within the context of
a [`Prologue`](#Prologue).
-}
serializeWith : Prologue -> Term compatible -> String
serializeWith config (Term variant) =
    serializeWithHelp config variant


{-| A `Prologue` captures the `base` and `prefixes` which are used to
shorten the serialization of [`Term`](#Term)'s.
-}
type alias Prologue =
    { base : Maybe String
    , prefixes : List ( String, String )
    }


serializeWithHelp : Prologue -> Variant -> String
serializeWithHelp config variant =
    case variant of
        Iri url ->
            if url == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" then
                "a"

            else
                case config.base of
                    Nothing ->
                        case
                            List.find
                                (\( _, value ) -> String.startsWith value url)
                                config.prefixes
                        of
                            Nothing ->
                                "<" ++ url ++ ">"

                            Just ( prefix, value ) ->
                                prefix ++ ":" ++ String.rightOf value url

                    Just base ->
                        if String.startsWith base url then
                            "<" ++ String.rightOf base url ++ ">"

                        else
                            case
                                List.find
                                    (\( _, value ) -> String.startsWith value url)
                                    config.prefixes
                            of
                                Nothing ->
                                    "<" ++ url ++ ">"

                                Just ( prefix, value ) ->
                                    prefix ++ ":" ++ String.rightOf value url

        Literal data ->
            let
                replaceLineBreaks : String -> String
                replaceLineBreaks =
                    String.replace "\n" "\\n"

                replaceQuotes : String -> String
                replaceQuotes =
                    String.replace "\"" "\\\""
            in
            if data.datatype == urlXsdString then
                [ "\""
                , data.value
                    |> replaceLineBreaks
                    |> replaceQuotes
                , "\""
                , case data.languageTag of
                    Nothing ->
                        ""

                    Just languageTag ->
                        "@" ++ languageTag
                ]
                    |> String.concat

            else if
                (data.datatype == urlXsdInteger)
                    || (data.datatype == urlXsdInt)
            then
                data.value

            else
                [ "\""
                , data.value
                    |> replaceLineBreaks
                    |> replaceQuotes
                , "\""
                , case data.languageTag of
                    Nothing ->
                        "^^" ++ serializeWith config (iri data.datatype)

                    Just languageTag ->
                        "@" ++ languageTag
                ]
                    |> String.concat

        _ ->
            Internal.serializeVariant variant


{-| Serialize a [`Triple`](#Triple) into how it would be represented in the
[Turtle](https://www.w3.org/TR/turtle/) format or in
[SPARQL](https://www.w3.org/TR/sparql11-query/).
-}
serializeTriple : Triple -> String
serializeTriple { subject, predicate, object } =
    [ serialize subject
    , serialize predicate
    , serialize object
    , "."
    ]
        |> String.join " "



-- JSON


{-| TODO Add documentation
-}
tripleDecoder : Decoder Triple
tripleDecoder =
    Decode.succeed Triple
        |> Decode.required "subject" subjectDecoder
        |> Decode.required "predicate" predicateDecoder
        |> Decode.required "object" objectDecoder


subjectDecoder : Decoder BlankNodeOrIri
subjectDecoder =
    [ blankNodeDecoder
    , iriDecoder
    ]
        |> Decode.oneOf
        |> Decode.map Term


predicateDecoder : Decoder Iri
predicateDecoder =
    iriDecoder
        |> Decode.map Term


objectDecoder : Decoder BlankNodeOrIriOrLiteral
objectDecoder =
    [ blankNodeDecoder
    , literalDecoder
    , iriDecoder
    ]
        |> Decode.oneOf
        |> Decode.map Term


blankNodeDecoder : Decoder Variant
blankNodeDecoder =
    Decode.string
        |> Decode.field "termType"
        |> Decode.andThen
            (\termType ->
                if termType == "BlankNode" then
                    Decode.string
                        |> Decode.field "value"
                        |> Decode.map BlankNode

                else
                    Decode.fail "not a blank node"
            )


iriDecoder : Decoder Variant
iriDecoder =
    Decode.map Iri urlDecoder


urlDecoder : Decoder String
urlDecoder =
    Decode.string
        |> Decode.field "termType"
        |> Decode.andThen
            (\termType ->
                if termType == "NamedNode" then
                    Decode.string
                        |> Decode.field "value"

                else
                    Decode.fail "not a named node"
            )


literalDecoder : Decoder Variant
literalDecoder =
    Decode.string
        |> Decode.field "termType"
        |> Decode.andThen
            (\termType ->
                if termType == "Literal" then
                    Decode.succeed DataLiteral
                        |> Decode.required "value" Decode.string
                        |> Decode.required "datatype" urlDecoder
                        |> Decode.required "language"
                            (Decode.oneOf
                                [ Decode.null Nothing
                                , Decode.string
                                    |> Decode.map
                                        (\languageTag ->
                                            if languageTag == "" then
                                                Nothing

                                            else
                                                Just languageTag
                                        )
                                ]
                            )
                        |> Decode.map Literal

                else
                    Decode.fail "not a literal node"
            )


{-| TODO Add documentation
-}
encodeTriple : Triple -> Value
encodeTriple triple =
    [ ( "subject", encodeSubject triple.subject )
    , ( "predicate", encodePredicate triple.predicate )
    , ( "object", encodeObject triple.object )
    ]
        |> Encode.object


encodeSubject : BlankNodeOrIri -> Value
encodeSubject (Term variant) =
    case variant of
        BlankNode name ->
            encodeBlankNode name

        Iri url ->
            encodeIri url

        Literal _ ->
            Encode.null

        _ ->
            Encode.null


encodePredicate : Iri -> Value
encodePredicate (Term variant) =
    case variant of
        BlankNode _ ->
            Encode.null

        Iri url ->
            encodeIri url

        Literal _ ->
            Encode.null

        _ ->
            Encode.null


encodeObject : BlankNodeOrIriOrLiteral -> Value
encodeObject (Term variant) =
    case variant of
        BlankNode name ->
            encodeBlankNode name

        Iri url ->
            encodeIri url

        Literal data ->
            encodeLiteral data

        _ ->
            Encode.null


encodeBlankNode : String -> Value
encodeBlankNode name =
    [ ( "termType", Encode.string "BlankNode" )
    , ( "value", Encode.string name )
    ]
        |> Encode.object


encodeIri : String -> Value
encodeIri url =
    [ ( "termType", Encode.string "NamedNode" )
    , ( "value", Encode.string url )
    ]
        |> Encode.object


encodeLiteral : DataLiteral -> Value
encodeLiteral data =
    [ ( "termType", Encode.string "Literal" )
    , ( "value", Encode.string data.value )
    , ( "datatype", encodeIri data.datatype )
    , ( "language"
      , case data.languageTag of
            Nothing ->
                Encode.null

            Just languageTag ->
                Encode.string languageTag
      )
    ]
        |> Encode.object



-- CONSTANTS


urlXsdString : String
urlXsdString =
    "http://www.w3.org/2001/XMLSchema#string"


urlXsdInt : String
urlXsdInt =
    "http://www.w3.org/2001/XMLSchema#int"


urlXsdInteger : String
urlXsdInteger =
    "http://www.w3.org/2001/XMLSchema#integer"


urlXsdDouble : String
urlXsdDouble =
    "http://www.w3.org/2001/XMLSchema#double"


urlXsdFloat : String
urlXsdFloat =
    "http://www.w3.org/2001/XMLSchema#float"


urlXsdDecimal : String
urlXsdDecimal =
    "http://www.w3.org/2001/XMLSchema#decimal"


urlXsdDate : String
urlXsdDate =
    "http://www.w3.org/2001/XMLSchema#date"


urlXsdDateTime : String
urlXsdDateTime =
    "http://www.w3.org/2001/XMLSchema#dateTime"


urlXsdBoolean : String
urlXsdBoolean =
    "http://www.w3.org/2001/XMLSchema#boolean"


urlRdfLangString : String
urlRdfLangString =
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString"
