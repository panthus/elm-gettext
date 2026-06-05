module PluralFormsParser exposing (Expr(..), PluralForms, Value(..), interpret, parse)

import Parser exposing ((|.), (|=), Parser)


type Value
    = ValueInt Int
    | ValueBool Bool
    | Error String


type Expr
    = Let -- We can get away with only supporting one global variable named `n`
    | Int Int
    | Bool Bool
    | Mod Expr Expr -- precedence: 5, associativity: Left-to-right
    | Le Expr Expr -- precedence: 9, associativity: Left-to-right
    | Lt Expr Expr -- precedence: 9, associativity: Left-to-right
    | Ge Expr Expr -- precedence: 9, associativity: Left-to-right
    | Gt Expr Expr -- precedence: 9, associativity: Left-to-right
    | Eq Expr Expr -- precedence: 10, associativity: Left-to-right
    | Ne Expr Expr -- precedence: 10, associativity: Left-to-right
    | And Expr Expr -- precedence: 14, associativity: Left-to-right
    | Or Expr Expr -- precedence: 15, associativity: Left-to-right
    | Cond Expr Expr Expr -- precedence: 17, associativity: Right-to-left


parseLet : Parser Expr
parseLet =
    Parser.symbol "n" |> Parser.map (\_ -> Let)


parseInt : Parser Expr
parseInt =
    Parser.int |> Parser.map Int


parseBool : Parser Expr
parseBool =
    Parser.oneOf
        [ Parser.keyword "true" |> Parser.map (\_ -> Bool True)
        , Parser.keyword "false" |> Parser.map (\_ -> Bool False)
        ]


parseOperator : (Expr -> Expr -> Expr) -> String -> Expr -> Parser Expr -> Parser Expr
parseOperator expr operatorSymbol lhsExpr rhsParser =
    Parser.succeed (expr lhsExpr)
        |. Parser.symbol operatorSymbol
        |. Parser.spaces
        |= rhsParser


parseCond : Expr -> Parser Expr -> Parser Expr
parseCond condExpr parser =
    Parser.succeed (Cond condExpr)
        |. Parser.symbol "?"
        |. Parser.spaces
        |= parser
        |. Parser.spaces
        |. Parser.symbol ":"
        |. Parser.spaces
        |= parser


parseParentheses : Parser Expr -> Parser Expr
parseParentheses parser =
    Parser.succeed identity
        |. Parser.symbol "("
        |. Parser.spaces
        |= parser
        |. Parser.spaces
        |. Parser.symbol ")"


parseMod : Expr -> Parser Expr
parseMod expr =
    parseOperator Mod "%" expr parseTerm


parseRelational : Expr -> Parser Expr
parseRelational expr =
    Parser.oneOf
        [ parseOperator Le "<=" expr (Parser.lazy (\_ -> parseExpression parseMod))
        , parseOperator Lt "<" expr (Parser.lazy (\_ -> parseExpression parseMod))
        , parseOperator Ge ">=" expr (Parser.lazy (\_ -> parseExpression parseMod))
        , parseOperator Gt ">" expr (Parser.lazy (\_ -> parseExpression parseMod))
        ]


parseModAndRelational : Expr -> Parser Expr
parseModAndRelational expr =
    Parser.oneOf [ parseMod expr, parseRelational expr ]


parseOperators : Expr -> Parser Expr
parseOperators expr =
    Parser.oneOf
        [ parseMod expr
        , parseRelational expr
        , parseOperator Eq "==" expr (Parser.lazy (\_ -> parseExpression parseModAndRelational))
        , parseOperator Ne "!=" expr (Parser.lazy (\_ -> parseExpression parseModAndRelational))
        ]


parseTerm : Parser Expr
parseTerm =
    Parser.oneOf
        [ parseLet
        , parseInt
        , parseBool
        , parseParentheses (Parser.lazy (\_ -> parseExpression parseAll))
        ]


parseAll : Expr -> Parser Expr
parseAll expr =
    Parser.oneOf
        [ parseOperators expr
        , parseOperator And "&&" expr (Parser.lazy (\_ -> parseExpression parseOperators))
        , parseOperator Or "||" expr (Parser.lazy (\_ -> parseExpression parseOperators))
        , parseCond expr (Parser.lazy (\_ -> parseExpression parseAll))
        ]


{-| Parse a plural form expression as documented here:
<https://www.gnu.org/software/gettext/manual/html_node/Plural-forms.html>. It is a left-to-right associated parser using
scope limiting to meet precedence rules. Right-to-left is simulated by recursively restarting the parser without passing
previous state.

For example `1 % 2 < 3 % 1 == 2`:

  - Step 1: `Int 1` any expression starts with a term
  - Step 2: `Mod (Int 1) (Int 2)` first parameter is step 1 and `%` is lowest precedence so its scope only allows
    parsing terms, so second parameter is a term.
  - Step 3: `Lt (Mod (Int 1) (Int 2)) (Mod (Int 3) (Int 1))` first parameter is step 2 and `<` has the second precedence
    so its scope only allows terms and `%`, so second parameter is up to equals.
  - Step 4: `Eq (Lt (Mod (Int 1) (Int 2)) (Mod (Int 3) (Int 1))) (Int 2)` first parameter is step 2 and `==` has the
    third precedence so its scope allows terms, `%` and relational operators.

-}
parseExpression : (Expr -> Parser Expr) -> Parser Expr
parseExpression allowedParserInScope =
    Parser.loop Nothing (expressionStep allowedParserInScope)


expressionStep : (Expr -> Parser Expr) -> Maybe Expr -> Parser (Parser.Step (Maybe Expr) Expr)
expressionStep allowedParserInScope expr =
    Parser.succeed identity
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed (\stmt -> Parser.Loop (Just stmt))
                |= (case expr of
                        Just e ->
                            allowedParserInScope e

                        Nothing ->
                            parseTerm
                   )
            , case expr of
                Just e ->
                    Parser.succeed ()
                        |> Parser.map (\_ -> Parser.Done e)

                Nothing ->
                    Parser.problem "No parsable expression found"
            ]


type alias PluralForms =
    { nPlurals : Int, plural : Expr }


parsePluralForms : Parser PluralForms
parsePluralForms =
    Parser.succeed (\a e -> { nPlurals = a, plural = e })
        |. Parser.token "nplurals="
        |= Parser.int
        |. Parser.symbol ";"
        |. Parser.spaces
        |. Parser.token "plural="
        |= parseExpression parseAll
        |. Parser.symbol ";"


parse : String -> Result (List Parser.DeadEnd) PluralForms
parse source =
    Parser.run parsePluralForms source


interpretHelper : Expr -> Int -> Value
interpretHelper expr value =
    case expr of
        Let ->
            ValueInt value

        Int int ->
            ValueInt int

        Bool bool ->
            ValueBool bool

        Mod lhsExpr rhsExpr ->
            case ( interpretHelper lhsExpr value, interpretHelper rhsExpr value ) of
                ( ValueInt lhs, ValueInt rhs ) ->
                    ValueInt (remainderBy rhs lhs)

                _ ->
                    Error "Mod requires two inputs of type int."

        Eq lhsExpr rhsExpr ->
            case ( interpretHelper lhsExpr value, interpretHelper rhsExpr value ) of
                ( ValueInt lhs, ValueInt rhs ) ->
                    ValueBool (lhs == rhs)

                ( ValueBool lhs, ValueBool rhs ) ->
                    ValueBool (lhs == rhs)

                _ ->
                    Error "Eq requires two inputs of type int or bool."

        Ne lhsExpr rhsExpr ->
            case ( interpretHelper lhsExpr value, interpretHelper rhsExpr value ) of
                ( ValueInt lhs, ValueInt rhs ) ->
                    ValueBool (lhs /= rhs)

                ( ValueBool lhs, ValueBool rhs ) ->
                    ValueBool (lhs /= rhs)

                _ ->
                    Error "Ne requires two inputs of type int or bool."

        Lt lhsExpr rhsExpr ->
            case ( interpretHelper lhsExpr value, interpretHelper rhsExpr value ) of
                ( ValueInt lhs, ValueInt rhs ) ->
                    ValueBool (lhs < rhs)

                _ ->
                    Error "Lt requires two inputs of type int."

        Le lhsExpr rhsExpr ->
            case ( interpretHelper lhsExpr value, interpretHelper rhsExpr value ) of
                ( ValueInt lhs, ValueInt rhs ) ->
                    ValueBool (lhs <= rhs)

                _ ->
                    Error "Le requires two inputs of type int."

        Gt lhsExpr rhsExpr ->
            case ( interpretHelper lhsExpr value, interpretHelper rhsExpr value ) of
                ( ValueInt lhs, ValueInt rhs ) ->
                    ValueBool (lhs > rhs)

                _ ->
                    Error "Gt requires two inputs of type int."

        Ge lhsExpr rhsExpr ->
            case ( interpretHelper lhsExpr value, interpretHelper rhsExpr value ) of
                ( ValueInt lhs, ValueInt rhs ) ->
                    ValueBool (lhs >= rhs)

                _ ->
                    Error "Ge requires two inputs of type int."

        And lhsExpr rhsExpr ->
            case ( interpretHelper lhsExpr value, interpretHelper rhsExpr value ) of
                ( ValueBool lhs, ValueBool rhs ) ->
                    ValueBool (lhs && rhs)

                _ ->
                    Error "And requires two inputs of type bool."

        Or lhsExpr rhsExpr ->
            case ( interpretHelper lhsExpr value, interpretHelper rhsExpr value ) of
                ( ValueBool lhs, ValueBool rhs ) ->
                    ValueBool (lhs || rhs)

                _ ->
                    Error "Or requires two inputs of type bool."

        Cond pred onT onF ->
            case interpretHelper pred value of
                ValueBool True ->
                    interpretHelper onT value

                ValueBool False ->
                    interpretHelper onF value

                _ ->
                    Error "Cond requires a predicate of type bool"


{-| Plural-Forms expects to return an Int so convert Bool to 0 or 1.
-}
interpret : Expr -> Int -> Result String Int
interpret expr value =
    case interpretHelper expr value of
        ValueInt v ->
            Ok v

        ValueBool False ->
            Ok 0

        ValueBool True ->
            Ok 1

        Error error ->
            Err error
