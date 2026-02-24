module Internal.Turtle exposing
    ( parse
    , Statement(..)
    , Triples(..), PredicateObjectList
    , Subject(..), Verb(..), Object(..)
    , Iri(..), BlankNode(..), Literal(..)
    )

{-|

@docs parse

@docs Statement
@docs Triples, PredicateObjectList
@docs Subject, Verb, Object
@docs Iri, BlankNode, Literal

-}

import Parser exposing ((|.), (|=), Parser)
import Set


type Statement
    = DirectivePrefixId String String
    | DirectiveBase String
    | DirectiveSparqlPrefix String String
    | DirectiveSparqlBase String
    | Triples Triples


type Triples
    = TriplesSubject Subject (List PredicateObjectList)
    | TriplesBlankNodePropertyList (List PredicateObjectList) (List PredicateObjectList)


type alias PredicateObjectList =
    { verb : Verb
    , objects : List Object
    }


type Subject
    = SubjectIri Iri
    | SubjectBlankNode BlankNode
    | SubjectCollection (List Object)


type Verb
    = Predicate Iri
    | A


type Object
    = ObjectIri Iri
    | ObjectBlankNode BlankNode
    | ObjectCollection (List Object)
    | ObjectBlankNodePropertyList (List PredicateObjectList)
    | ObjectLiteral Literal


type Iri
    = IriRef String
    | PrefixedName String String


type BlankNode
    = BlankNodeLabel String
    | Anon


type Literal
    = LiteralString String
    | LiteralLangString String String
    | LiteralTyped String Iri
    | LiteralInteger Int
    | LiteralDecimal String
    | LiteralDouble Float
    | LiteralTrue
    | LiteralFalse


parse : String -> Result (List Parser.DeadEnd) (List Statement)
parse =
    Parser.run statements


statements : Parser (List Statement)
statements =
    Parser.succeed identity
        |. whitespaceOrComment
        |= Parser.loop [] statementsHelp
        |. whitespaceOrComment
        |. Parser.end


statementsHelp : List Statement -> Parser (Parser.Step (List Statement) (List Statement))
statementsHelp statementsReversed =
    Parser.oneOf
        [ Parser.succeed (\statementNext -> Parser.Loop (statementNext :: statementsReversed))
            |= statement
            |. whitespaceOrComment
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse statementsReversed))
        ]


statement : Parser Statement
statement =
    Parser.oneOf
        [ directive
        , Parser.map Triples triples
        ]


directive : Parser Statement
directive =
    Parser.oneOf
        [ prefixId
        , base
        , sparqlPrefix
        , sparqlBase
        ]


prefixId : Parser Statement
prefixId =
    Parser.succeed DirectivePrefixId
        |. Parser.keyword "@prefix"
        |. whitespaceOrComment
        |= pnameNs
        |. whitespaceOrComment
        |= iriRef
        |. whitespaceOrComment
        |. Parser.symbol "."
        |. whitespaceOrComment


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


base : Parser Statement
base =
    Parser.succeed DirectiveBase
        |. Parser.keyword "@base"
        |. whitespaceOrComment
        |= iriRef
        |. whitespaceOrComment
        |. Parser.symbol "."


sparqlPrefix : Parser Statement
sparqlPrefix =
    Parser.succeed DirectiveSparqlPrefix
        |. Parser.keyword "PREFIX"
        |. whitespaceOrComment
        |= pnameNs
        |. whitespaceOrComment
        |= iriRef
        |. whitespaceOrComment


sparqlBase : Parser Statement
sparqlBase =
    Parser.succeed DirectiveSparqlBase
        |. Parser.keyword "BASE"
        |. whitespaceOrComment
        |= iriRef


triples : Parser Triples
triples =
    Parser.succeed identity
        |= Parser.oneOf
            [ triplesSubject
            , triplesBlankNodePropertyList
            ]
        |. whitespaceOrComment
        |. Parser.symbol "."


triplesSubject : Parser Triples
triplesSubject =
    Parser.succeed TriplesSubject
        |= subject
        |. whitespaceOrComment
        |= predicateObjectLists


subject : Parser Subject
subject =
    Parser.oneOf
        [ subjectBlankNode
        , subjectCollection
        , subjectIri
        ]


subjectIri : Parser Subject
subjectIri =
    Parser.map SubjectIri iri


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
            , spaces = whitespaceOrComment
            , item = object
            , trailing = Parser.Forbidden
            }


triplesBlankNodePropertyList : Parser Triples
triplesBlankNodePropertyList =
    Parser.succeed TriplesBlankNodePropertyList
        |. Parser.symbol "["
        |. whitespaceOrComment
        |= predicateObjectLists
        |. whitespaceOrComment
        |. Parser.symbol "]"
        |. whitespaceOrComment
        |= Parser.oneOf
            [ predicateObjectLists
            , Parser.succeed []
            ]


predicateObjectLists : Parser (List PredicateObjectList)
predicateObjectLists =
    predicateObjectListData
        |> Parser.andThen (\itemFirst -> Parser.loop [ itemFirst ] predicateObjectListHelp)


predicateObjectListHelp : List PredicateObjectList -> Parser (Parser.Step (List PredicateObjectList) (List PredicateObjectList))
predicateObjectListHelp itemsReversed =
    let
        end : Parser (Parser.Step (List PredicateObjectList) (List PredicateObjectList))
        end =
            Parser.succeed ()
                |> Parser.map (\_ -> Parser.Done (List.reverse itemsReversed))
    in
    Parser.succeed identity
        |. whitespaceOrComment
        |= Parser.oneOf
            [ Parser.succeed identity
                |. Parser.symbol ";"
                |. whitespaceOrComment
                |= Parser.oneOf
                    [ Parser.succeed (\itemNext -> Parser.Loop (itemNext :: itemsReversed))
                        |= predicateObjectListData
                    , end
                    ]
            , end
            ]


predicateObjectListData : Parser PredicateObjectList
predicateObjectListData =
    Parser.succeed PredicateObjectList
        |= verb
        |. whitespaceOrComment
        |= objects


objects : Parser (List Object)
objects =
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
        |. whitespaceOrComment
        |= Parser.oneOf
            [ Parser.succeed identity
                |. Parser.symbol ","
                |. whitespaceOrComment
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
            , spaces = whitespaceOrComment
            , item = Parser.lazy (\_ -> object)
            , trailing = Parser.Forbidden
            }


objectBlankNodePropertyList : Parser Object
objectBlankNodePropertyList =
    Parser.succeed ObjectBlankNodePropertyList
        |. Parser.symbol "["
        |. whitespaceOrComment
        |= Parser.lazy (\_ -> predicateObjectLists)
        |. whitespaceOrComment
        |. Parser.symbol "]"


objectLiteral : Parser Object
objectLiteral =
    Parser.map ObjectLiteral literal


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
        [ Parser.succeed ()
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
        , Parser.succeed ""
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
        , Parser.backtrackable
            (Parser.succeed Anon
                |. Parser.symbol "["
                |. whitespaceOrComment
                |. Parser.symbol "]"
            )
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
                    [ Parser.succeed (LiteralLangString str)
                        |. Parser.symbol "@"
                        |= langTag
                    , Parser.succeed (LiteralTyped str)
                        |. Parser.symbol "^^"
                        |= iri
                    , Parser.succeed (LiteralString str)
                    ]
            )


numericLiteral : Parser Literal
numericLiteral =
    Parser.oneOf
        [ Parser.succeed negateNumericLiteral
            |. Parser.symbol "-"
            |= numeric
        , Parser.succeed identity
            |. Parser.symbol "+"
            |= numeric
        , numeric
        ]


numeric : Parser Literal
numeric =
    Parser.succeed (\start end -> start ++ end)
        |= numericHelp
        |= Parser.oneOf
            [ Parser.succeed (\sign num -> "e" ++ sign ++ num)
                |. Parser.oneOf
                    [ Parser.symbol "e"
                    , Parser.symbol "E"
                    ]
                |= Parser.oneOf
                    [ Parser.succeed "-"
                        |. Parser.symbol "-"
                    , Parser.succeed ""
                        |. Parser.symbol "+"
                    , Parser.succeed ""
                    ]
                |= Parser.getChompedString
                    (Parser.succeed ()
                        |. Parser.chompIf Char.isDigit
                        |. Parser.chompWhile Char.isDigit
                    )
            , Parser.succeed ""
            ]
        |> Parser.andThen
            (\raw ->
                if String.isEmpty raw then
                    Parser.problem "not a valid numerical value"

                else
                    case String.toInt raw of
                        Nothing ->
                            if String.contains "e" raw || String.contains "E" raw then
                                String.toFloat raw
                                    |> Maybe.map (LiteralDouble >> Parser.succeed)
                                    |> Maybe.withDefault (Parser.problem ("'" ++ raw ++ "' is not a valid double literal"))

                            else
                                raw
                                    |> LiteralDecimal
                                    |> Parser.succeed

                        Just int ->
                            int
                                |> LiteralInteger
                                |> Parser.succeed
            )


numericHelp : Parser String
numericHelp =
    Parser.succeed (\start end -> start ++ end)
        |= Parser.getChompedString
            (Parser.succeed ()
                |. Parser.chompIf Char.isDigit
                |. Parser.chompWhile Char.isDigit
            )
        |= Parser.oneOf
            [ Parser.succeed identity
                |. Parser.backtrackable (Parser.symbol ".")
                |. Parser.chompIf Char.isDigit
                |. Parser.chompWhile Char.isDigit
                |> Parser.getChompedString
                |> Parser.andThen
                    (\raw ->
                        if String.isEmpty raw then
                            Parser.problem "the fractional part must not be empty"

                        else
                            Parser.succeed raw
                    )
            , Parser.succeed ""
            ]


negateNumericLiteral : Literal -> Literal
negateNumericLiteral l =
    case l of
        LiteralInteger value ->
            LiteralInteger (-1 * value)

        LiteralDecimal value ->
            LiteralDecimal ("-" ++ value)

        LiteralDouble value ->
            LiteralDouble (-1 * value)

        _ ->
            l


booleanLiteral : Parser Literal
booleanLiteral =
    Parser.oneOf
        [ Parser.succeed LiteralTrue
            |. Parser.keyword "true"
        , Parser.succeed LiteralFalse
            |. Parser.keyword "false"
        ]


string : Parser String
string =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.symbol "\"\"\""
            |= Parser.loop [] stringMultiline
        , Parser.succeed identity
            |. Parser.symbol "\""
            |= Parser.loop [] stringSingleline
        ]


stringMultiline : List String -> Parser (Parser.Step (List String) String)
stringMultiline revChunks =
    Parser.oneOf
        [ stringSubstitute revChunks
        , Parser.token "\"\"\""
            |> Parser.map (\_ -> Parser.Done (String.concat (List.reverse revChunks)))
        , Parser.token "\"\""
            |> Parser.map (\_ -> Parser.Loop ("\"\"" :: revChunks))
        , Parser.token "\""
            |> Parser.map (\_ -> Parser.Loop ("\"" :: revChunks))
        , stringChomp revChunks
        ]


stringSingleline : List String -> Parser (Parser.Step (List String) String)
stringSingleline revChunks =
    Parser.oneOf
        [ stringSubstitute revChunks
        , Parser.token "\""
            |> Parser.map (\_ -> Parser.Done (String.concat (List.reverse revChunks)))
        , stringChomp revChunks
        ]


stringSubstitute : List String -> Parser (Parser.Step (List String) String)
stringSubstitute revChunks =
    Parser.succeed (\chunk -> Parser.Loop (chunk :: revChunks))
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
            , Parser.succeed String.fromChar
                |. Parser.token "u"
                |= (Parser.getChompedString
                        (Parser.succeed (\_ _ _ _ -> ())
                            |. Parser.chompIf Char.isHexDigit
                            |. Parser.chompIf Char.isHexDigit
                            |. Parser.chompIf Char.isHexDigit
                            |. Parser.chompIf Char.isHexDigit
                        )
                        |> Parser.andThen codeToChar
                   )
            ]


stringChomp : List String -> Parser (Parser.Step (List String) String)
stringChomp revChunks =
    Parser.chompWhile isUninteresting
        |> Parser.getChompedString
        |> Parser.map (\chunk -> Parser.Loop (chunk :: revChunks))


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
    if length < 4 && length > 6 then
        Parser.problem ("code point must have between 4 and 6 digits: " ++ str)

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


whitespaceOrComment : Parser ()
whitespaceOrComment =
    Parser.succeed ()
        |. whitespace
        |. Parser.loop ()
            (\_ ->
                Parser.oneOf
                    [ Parser.succeed (Parser.Loop ())
                        |. Parser.lineComment "#"
                        |. whitespace
                    , Parser.succeed (Parser.Done ())
                    ]
            )


whitespace : Parser ()
whitespace =
    Parser.chompWhile isWhitespace


isWhitespace : Char -> Bool
isWhitespace char =
    char == ' ' || char == '\t' || char == '\n' || char == '\u{000D}'


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
