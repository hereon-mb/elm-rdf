module Internal.Turtle exposing
    ( parse
    , TurtleDoc
    , Statement(..)
    , Directive(..)
    , Triples(..)
    , PredicateObjectList
    , PredicateObjectListData
    , Verb(..)
    , Subject(..)
    , Object(..)
    , Literal(..)
    , NumericLiteral(..)
    , BooleanLiteral(..)
    , Iri(..)
    , BlankNode(..)
    )

{-|

@docs parse

@docs TurtleDoc
@docs Statement
@docs Directive
@docs Triples
@docs PredicateObjectList
@docs PredicateObjectListData
@docs Verb
@docs Subject
@docs Object
@docs Literal
@docs NumericLiteral
@docs BooleanLiteral
@docs Iri
@docs BlankNode

-}

import Parser exposing ((|.), (|=), Parser)
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
    | TriplesBlankNodePropertyList PredicateObjectList PredicateObjectList


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
    = Integer Int
    | Decimal String
    | Double Float


type BooleanLiteral
    = BooleanLiteralTrue
    | BooleanLiteralFalse


type Iri
    = IriRef String
    | PrefixedName String String


type BlankNode
    = BlankNodeLabel String
    | Anon


parse : String -> Result (List Parser.DeadEnd) TurtleDoc
parse =
    Parser.run turtleDoc


turtleDoc : Parser TurtleDoc
turtleDoc =
    Parser.succeed identity
        |= Parser.loop [] turtleDocHelp
        |. whitespace
        |. Parser.end


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
        |= pnameNs
        |. whitespace
        |= iriRef
        |. whitespace
        |. Parser.symbol "."


pnameNs : Parser String
pnameNs =
    Parser.oneOf
        [ Parser.succeed ""
            |. Parser.symbol ":"
        , Parser.succeed identity
            |= Parser.variable
                { start = \char -> char /= ':' && isPnCharsBase char
                , inner = \char -> char /= ':' && isPnChars char
                , reserved = Set.empty
                }
            |. Parser.symbol ":"
        ]


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
    Parser.succeed identity
        |= Parser.oneOf
            [ triplesSubject
            , triplesBlankNodePropertyList
            ]
        |. whitespace
        |. Parser.symbol "."


triplesSubject : Parser Triples
triplesSubject =
    Parser.succeed TriplesSubject
        |= subject
        |. whitespace
        |= predicateObjectList


subject : Parser Subject
subject =
    Parser.oneOf
        [ subjectBlankNode
        , subjectCollection
        , subjectIri
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


triplesBlankNodePropertyList : Parser Triples
triplesBlankNodePropertyList =
    Parser.succeed TriplesBlankNodePropertyList
        |. Parser.symbol "["
        |. whitespace
        |= predicateObjectList
        |. whitespace
        |. Parser.symbol "]"
        |. whitespace
        |= Parser.oneOf
            [ predicateObjectList
            , Parser.succeed []
            ]


predicateObjectList : Parser PredicateObjectList
predicateObjectList =
    predicateObjectListData
        |> Parser.andThen (\itemFirst -> Parser.loop [ itemFirst ] predicateObjectListHelp)


predicateObjectListHelp : List PredicateObjectListData -> Parser (Parser.Step (List PredicateObjectListData) (List PredicateObjectListData))
predicateObjectListHelp itemsReversed =
    let
        end : Parser (Parser.Step (List PredicateObjectListData) (List PredicateObjectListData))
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
        end : Parser (Parser.Step (List Object) (List Object))
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
        [ objectBlankNode
        , objectCollection
        , objectBlankNodePropertyList
        , objectLiteral
        , objectIri
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


objectBlankNodePropertyList : Parser Object
objectBlankNodePropertyList =
    Parser.succeed ObjectBlankNodePropertyList
        |. Parser.symbol "["
        |. whitespace
        |= Parser.lazy (\_ -> predicateObjectList)
        |. whitespace
        |. Parser.symbol "]"


objectLiteral : Parser Object
objectLiteral =
    Parser.succeed ObjectLiteral
        |= literal


iri : Parser Iri
iri =
    Parser.oneOf
        [ Parser.map IriRef iriRef
        , Parser.succeed PrefixedName
            |= pnameNs
            |= pnLocal
        ]


pnLocal : Parser String
pnLocal =
    Parser.oneOf
        [ Parser.succeed ""
            |. Parser.chompIf isWhitespace
        , Parser.succeed ()
            |. Parser.chompIf
                (\char ->
                    isPnCharsU char
                        || (char == ':')
                        || Char.isDigit char
                )
            |. Parser.chompWhile
                (\char ->
                    isPnChars char
                        || (char == '.')
                        || (char == ':')
                )
            |> Parser.getChompedString
            |> Parser.andThen
                (\chomped ->
                    if String.endsWith "." chomped then
                        Parser.problem "prefixed IRI must not end with '.'"

                    else
                        Parser.succeed chomped
                )
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
        , Parser.succeed Anon
            |. Parser.symbol "[]"
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
        , numericLiteral
        , booleanLiteral
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


numericLiteral : Parser Literal
numericLiteral =
    Parser.map NumericLiteral <|
        Parser.oneOf
            [ Parser.succeed negateNumericLiteral
                |. Parser.symbol "-"
                |= numeric
            , Parser.succeed identity
                |. Parser.symbol "+"
                |= numeric
            , numeric
            ]


numeric : Parser NumericLiteral
numeric =
    (Parser.succeed ()
        |. Parser.chompIf
            (\char ->
                Char.isDigit char
                    || (char == '.')
            )
        |. Parser.chompWhile
            (\char ->
                Char.isDigit char
                    || (char == '.')
                    || (char == 'e')
                    || (char == 'E')
                    || (char == '-')
                    || (char == '+')
            )
    )
        |> Parser.getChompedString
        |> Parser.andThen
            (\raw ->
                case String.toInt raw of
                    Nothing ->
                        if String.contains "e" raw || String.contains "E" raw then
                            String.toFloat raw
                                |> Maybe.map (Double >> Parser.succeed)
                                |> Maybe.withDefault (Parser.problem ("'" ++ raw ++ "' is not a valid double literal"))

                        else
                            raw
                                |> Decimal
                                |> Parser.succeed

                    Just int ->
                        int
                            |> Integer
                            |> Parser.succeed
            )


negateNumericLiteral : NumericLiteral -> NumericLiteral
negateNumericLiteral num =
    case num of
        Integer value ->
            Integer (-1 * value)

        Decimal value ->
            Decimal ("-" ++ value)

        Double value ->
            Double (-1 * value)


booleanLiteral : Parser Literal
booleanLiteral =
    Parser.oneOf
        [ Parser.succeed BooleanLiteralTrue
            |. Parser.keyword "true"
            |> Parser.map BooleanLiteral
        , Parser.succeed BooleanLiteralFalse
            |. Parser.keyword "false"
            |> Parser.map BooleanLiteral
        ]


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
            |> Parser.map (\_ -> Parser.Done (String.concat (List.reverse revChunks)))
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
        length : Int
        length =
            String.length str
    in
    if 4 <= length && length <= 6 then
        Parser.problem "code point must have between 4 and 6 digits"

    else
        let
            code : Int
            code =
                String.foldl addHex 0 str
        in
        if 0 <= code && code <= 0x0010FFFF then
            Parser.succeed (Char.fromCode code)

        else
            Parser.problem "code point must be between 0 and 0x10FFFF"


addHex : Char -> Int -> Int
addHex char total =
    let
        code : Int
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
    Parser.chompWhile isWhitespace


isWhitespace : Char -> Bool
isWhitespace char =
    char == ' ' || char == '\t' || char == '\n'


isPnCharsBase : Char -> Bool
isPnCharsBase char =
    let
        code : Int
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
        code : Int
        code =
            Char.toCode char
    in
    isPnCharsU char
        || (char == '-')
        || Char.isDigit char
        || (code == 0xB7)
        || (code >= 0x0300 && code <= 0x036F)
        || (code >= 0x203F && code <= 0x2040)
