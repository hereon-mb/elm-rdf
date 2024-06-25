module Internal.Turtle exposing
    ( parse
    , TurtleDoc
    , Statement(..)
    , Directive(..)
    , Triples(..)
    , Verb(..)
    , Subject(..)
    , Object(..)
    , Literal(..)
    , Iri(..)
    , BlankNode(..)
    )

{-|

@docs parse

@docs TurtleDoc
@docs Statement
@docs Directive
@docs Triples
@docs Verb
@docs Subject
@docs Object
@docs Literal
@docs Iri
@docs BlankNode

-}

import Parser exposing ((|.), (|=), Parser, oneOf)
import Set


type alias TurtleDoc =
    List Statement


type Statement
    = Directive Directive
    | Triples Triples


type Directive
    = PrefixId String String
    | Base String
    | SparqlPrefix String String
    | SparqlBase String


type Triples
    = TriplesSubject Subject PredicateObjectList
    | TriplesBlankNodePropertyList PredicateObjectList (Maybe PredicateObjectList)


type alias PredicateObjectList =
    List PredicateObjectListData


type alias PredicateObjectListData =
    { verb : Verb
    , objectList : List Object
    }


type Verb
    = Predicate Iri
    | A


type Subject
    = SubjectIri Iri
    | SubjectBlankNode BlankNode
    | SubjectCollection (List Object)


type Object
    = ObjectIri Iri
    | ObjectBlankNode BlankNode
    | ObjectCollection (List Object)
    | ObjectBlankNodePropertyList PredicateObjectList
    | ObjectLiteral Literal


type Literal
    = RdfLiteralString String
    | RdfLiteralLangString String String
    | RdfLiteralTyped String Iri
    | NumericLiteral NumericLiteral
    | BooleanLiteral BooleanLiteral


type NumericLiteral
    = Integer String
    | Decimal String
    | Double String


type alias LangTag =
    { tag : String
    , iri : Iri
    }


type BooleanLiteral
    = BooleanLiteralTrue
    | BooleanLiteralFalse


type Iri
    = IriRef String
    | PrefixedName String


type BlankNode
    = BlankNodeLabel String
    | Anon


parse : String -> Result (List Parser.DeadEnd) TurtleDoc
parse =
    Parser.run turtleDoc


turtleDoc : Parser TurtleDoc
turtleDoc =
    Parser.loop [] turtleDocHelp


turtleDocHelp : List Statement -> Parser (Parser.Step (List Statement) (List Statement))
turtleDocHelp statementsReversed =
    Parser.oneOf
        [ Parser.succeed (\statementNext -> Parser.Loop (statementNext :: statementsReversed))
            |= statement
            |. whitespace
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse statementsReversed))
        ]


statement : Parser Statement
statement =
    Parser.oneOf
        [ Parser.map Directive directive
        , Parser.map Triples triples
        ]


directive : Parser Directive
directive =
    Parser.oneOf
        [ prefixId
        , base
        , sparqlPrefix
        , sparqlBase
        ]


prefixId : Parser Directive
prefixId =
    Parser.succeed PrefixId
        |. Parser.keyword "@prefix"
        |. whitespace
        |= Parser.variable
            { start = \char -> char /= ':' && isPnCharsBase char
            , inner = \char -> char /= ':' && isPnChars char
            , reserved = Set.empty
            }
        |. Parser.symbol ":"
        |. whitespace
        |= iriRef
        |. whitespace
        |. Parser.symbol "."


base : Parser Directive
base =
    Parser.succeed Base
        |. Parser.keyword "@base"
        |. whitespace
        |= iriRef
        |. whitespace
        |. Parser.symbol "."


sparqlPrefix : Parser Directive
sparqlPrefix =
    Parser.succeed SparqlPrefix
        |. Parser.keyword "PREFIX"
        |. whitespace
        |= Parser.variable
            { start = \char -> char /= ':' && isPnCharsBase char
            , inner = \char -> char /= ':' && isPnChars char
            , reserved = Set.empty
            }
        |. Parser.symbol ":"
        |. whitespace
        |= iriRef


sparqlBase : Parser Directive
sparqlBase =
    Parser.succeed SparqlBase
        |. Parser.keyword "BASE"
        |. whitespace
        |= iriRef


triples : Parser Triples
triples =
    Parser.oneOf
        [ triplesSubject
        ]


triplesSubject : Parser Triples
triplesSubject =
    Parser.succeed TriplesSubject
        |= subject
        |. whitespace
        |= predicateObjectList


subject : Parser Subject
subject =
    Parser.oneOf
        [ subjectIri
        , subjectBlankNode
        , subjectCollection
        ]


subjectIri : Parser Subject
subjectIri =
    Parser.succeed SubjectIri
        |= iri


subjectBlankNode : Parser Subject
subjectBlankNode =
    Parser.succeed SubjectBlankNode
        |= blankNode


subjectCollection : Parser Subject
subjectCollection =
    Parser.succeed SubjectCollection
        |= Parser.sequence
            { start = "("
            , separator = ""
            , end = ")"
            , spaces = whitespace
            , item = object
            , trailing = Parser.Forbidden
            }


predicateObjectList : Parser PredicateObjectList
predicateObjectList =
    predicateObjectListData
        |> Parser.andThen (\itemFirst -> Parser.loop [ itemFirst ] predicateObjectListHelp)


predicateObjectListHelp : List PredicateObjectListData -> Parser (Parser.Step (List PredicateObjectListData) (List PredicateObjectListData))
predicateObjectListHelp itemsReversed =
    let
        end =
            Parser.succeed ()
                |> Parser.map (\_ -> Parser.Done (List.reverse itemsReversed))
    in
    Parser.succeed identity
        |. whitespace
        |= Parser.oneOf
            [ Parser.succeed identity
                |. Parser.symbol ";"
                |. whitespace
                |= Parser.oneOf
                    [ Parser.succeed (\itemNext -> Parser.Loop (itemNext :: itemsReversed))
                        |= predicateObjectListData
                    , end
                    ]
            , end
            ]


predicateObjectListData : Parser PredicateObjectListData
predicateObjectListData =
    Parser.succeed PredicateObjectListData
        |= verb
        |. whitespace
        |= objectList


objectList : Parser (List Object)
objectList =
    object
        |> Parser.andThen (\objectFirst -> Parser.loop [ objectFirst ] objectListHelp)


objectListHelp : List Object -> Parser (Parser.Step (List Object) (List Object))
objectListHelp objectsReversed =
    let
        end =
            Parser.succeed ()
                |> Parser.map (\_ -> Parser.Done (List.reverse objectsReversed))
    in
    Parser.succeed identity
        |. whitespace
        |= Parser.oneOf
            [ Parser.succeed identity
                |. Parser.symbol ","
                |. whitespace
                |= Parser.oneOf
                    [ Parser.succeed (\objectNext -> Parser.Loop (objectNext :: objectsReversed))
                        |= object
                    , end
                    ]
            , end
            ]


verb : Parser Verb
verb =
    Parser.oneOf
        [ a
        , predicate
        ]


a : Parser Verb
a =
    Parser.succeed A
        |. Parser.keyword "a"


predicate : Parser Verb
predicate =
    Parser.succeed Predicate
        |= iri


object : Parser Object
object =
    Parser.oneOf
        [ objectIri
        , objectBlankNode
        , objectCollection
        , objectLiteral
        ]


objectIri : Parser Object
objectIri =
    Parser.map ObjectIri iri


objectBlankNode : Parser Object
objectBlankNode =
    Parser.succeed ObjectBlankNode
        |= blankNode


objectCollection : Parser Object
objectCollection =
    Parser.succeed ObjectCollection
        |= Parser.sequence
            { start = "("
            , separator = ""
            , end = ")"
            , spaces = whitespace
            , item = Parser.lazy (\_ -> object)
            , trailing = Parser.Forbidden
            }


objectLiteral : Parser Object
objectLiteral =
    Parser.succeed ObjectLiteral
        |= literal


iri : Parser Iri
iri =
    Parser.oneOf
        [ Parser.map IriRef iriRef
        ]


iriRef : Parser String
iriRef =
    Parser.succeed identity
        |. Parser.symbol "<"
        |= url
        |. Parser.symbol ">"


url : Parser String
url =
    Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompWhile allowedUrlChar


blankNode : Parser BlankNode
blankNode =
    Parser.oneOf
        [ Parser.map BlankNodeLabel blankNodeLabel
        ]


blankNodeLabel : Parser String
blankNodeLabel =
    Parser.andThen
        (\label ->
            if String.endsWith "." label then
                Parser.problem "blank node must not end with '.'"

            else
                Parser.succeed label
        )
        (Parser.succeed identity
            |. Parser.symbol "_:"
            |= Parser.variable
                { start = \char -> Char.isDigit char || isPnCharsU char
                , inner = \char -> char == '.' || isPnChars char
                , reserved = Set.empty
                }
        )


allowedUrlChar : Char -> Bool
allowedUrlChar char =
    (char /= '<')
        && (char /= '>')
        && (char /= '{')
        && (char /= '}')
        && (char /= '|')
        && (char /= '^')
        && (char /= '`')
        && (char /= '\\')
        && (Char.toCode char > 0x20)


literal : Parser Literal
literal =
    Parser.oneOf
        [ rdfLiteral
        ]


rdfLiteral : Parser Literal
rdfLiteral =
    string
        |> Parser.andThen
            (\str ->
                Parser.oneOf
                    [ Parser.succeed (RdfLiteralLangString str)
                        |. Parser.symbol "@"
                        |= langTag
                    , Parser.succeed (RdfLiteralTyped str)
                        |. Parser.symbol "^^"
                        |= iri
                    , Parser.succeed (RdfLiteralString str)
                    ]
            )


string : Parser String
string =
    Parser.succeed identity
        |. Parser.symbol "\""
        |= Parser.loop [] stringHelp


stringHelp : List String -> Parser (Parser.Step (List String) String)
stringHelp revChunks =
    Parser.oneOf
        [ Parser.succeed (\chunk -> Parser.Loop (chunk :: revChunks))
            |. Parser.token "\\"
            |= Parser.oneOf
                [ Parser.map (\_ -> "\n") (Parser.token "n")
                , Parser.map (\_ -> "\t") (Parser.token "t")
                , Parser.map (\_ -> "\"") (Parser.token "\"")
                , Parser.map (\_ -> "\\") (Parser.token "\\")
                , Parser.map (\_ -> "\u{000D}") (Parser.token "r")
                , Parser.succeed String.fromChar
                    |. Parser.token "u{"
                    |= unicode
                    |. Parser.token "}"
                ]
        , Parser.token "\""
            |> Parser.map (\_ -> Parser.Done (String.join "" (List.reverse revChunks)))
        , Parser.chompWhile isUninteresting
            |> Parser.getChompedString
            |> Parser.map (\chunk -> Parser.Loop (chunk :: revChunks))
        ]


unicode : Parser Char
unicode =
    Parser.getChompedString (Parser.chompWhile Char.isHexDigit)
        |> Parser.andThen codeToChar


codeToChar : String -> Parser Char
codeToChar str =
    let
        length =
            String.length str

        code =
            String.foldl addHex 0 str
    in
    if 4 <= length && length <= 6 then
        Parser.problem "code point must have between 4 and 6 digits"

    else if 0 <= code && code <= 0x0010FFFF then
        Parser.succeed (Char.fromCode code)

    else
        Parser.problem "code point must be between 0 and 0x10FFFF"


addHex : Char -> Int -> Int
addHex char total =
    let
        code =
            Char.toCode char
    in
    if 0x30 <= code && code <= 0x39 then
        16 * total + (code - 0x30)

    else if 0x41 <= code && code <= 0x46 then
        16 * total + (10 + code - 0x41)

    else
        16 * total + (10 + code - 0x61)


isUninteresting : Char -> Bool
isUninteresting char =
    char /= '\\' && char /= '"'


langTag : Parser String
langTag =
    Parser.succeed
        (\lang maybeRegion ->
            case maybeRegion of
                Nothing ->
                    lang

                Just region ->
                    lang ++ "-" ++ region
        )
        |= Parser.variable
            { start = Char.isAlpha
            , inner = Char.isAlpha
            , reserved = Set.empty
            }
        |= Parser.oneOf
            [ Parser.succeed Just
                |. Parser.symbol "-"
                |= Parser.variable
                    { start = Char.isAlphaNum
                    , inner = Char.isAlphaNum
                    , reserved = Set.empty
                    }
            , Parser.succeed Nothing
            ]


whitespace : Parser ()
whitespace =
    Parser.chompWhile
        (\char -> char == ' ' || char == '\t')


isPnCharsBase : Char -> Bool
isPnCharsBase char =
    let
        code =
            Char.toCode char
    in
    Char.isAlpha char
        || (code >= 0xC0 && code <= 0xD6)
        || (code >= 0xD8 && code <= 0xF6)
        || (code >= 0xF8 && code <= 0x02FF)
        || (code >= 0x0370 && code <= 0x037D)
        || (code >= 0x037F && code <= 0x1FFF)
        || (code >= 0x200C && code <= 0x200D)
        || (code >= 0x2070 && code <= 0x218F)
        || (code >= 0x2C00 && code <= 0x2FEF)
        || (code >= 0x3001 && code <= 0xD7FF)
        || (code >= 0xF900 && code <= 0xFDCF)
        || (code >= 0xFDF0 && code <= 0xFFFD)
        || (code >= 0x00010000 && code <= 0x000EFFFF)


isPnCharsU : Char -> Bool
isPnCharsU char =
    isPnCharsBase char || char == '_' || char == ':'


isPnChars : Char -> Bool
isPnChars char =
    let
        code =
            Char.toCode char
    in
    isPnCharsU char
        || (char == '-')
        || Char.isDigit char
        || (code == 0xB7)
        || (code >= 0x0300 && code <= 0x036F)
        || (code >= 0x203F && code <= 0x2040)
