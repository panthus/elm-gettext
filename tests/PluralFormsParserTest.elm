module PluralFormsParserTest exposing (suite)

import Expect
import PluralFormsParser exposing (Expr(..), PluralForms, Value(..), interpret, parse)
import Test exposing (Test)


source : String -> String
source expr =
    "nplurals=3; plural=" ++ expr ++ ";"


expected : Expr -> PluralForms
expected expr =
    { nPlurals = 3, plural = expr }


exprEquals : Expr -> String -> Expect.Expectation
exprEquals expr string =
    Expect.equal (expr |> expected |> Ok) (string |> source |> parse)


ast : Expr
ast =
    Cond
        (And
            (Eq (Mod Let (Int 10)) (Int 1))
            (Ne (Mod Let (Int 100)) (Int 11))
        )
        (Int 0)
        (Cond
            (And
                (And
                    (Ge (Mod Let (Int 10)) (Int 2))
                    (Le (Mod Let (Int 10)) (Int 4))
                )
                (Or
                    (Lt (Mod Let (Int 100)) (Int 10))
                    (Ge (Mod Let (Int 100)) (Int 20))
                )
            )
            (Int 1)
            (Int 2)
        )


suite : Test
suite =
    Test.describe "PluralFormsParserTest"
        [ Test.test "Interpret expression and verify it returns 0 for 1" <|
            \_ -> Expect.equal (Ok 0) (interpret ast 1)
        , Test.test "Interpret expression and verify it returns 1 for 2" <|
            \_ -> Expect.equal (Ok 1) (interpret ast 2)
        , Test.test "Interpret expression and verify it returns 2 for 14" <|
            \_ -> Expect.equal (Ok 2) (interpret ast 14)
        , Test.test "Parse expression and verify the AST" <|
            \_ -> exprEquals ast "n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2"
        , Test.test "Operator precedence: Mod vs Eq" <|
            \_ -> exprEquals (Eq (Mod Let (Int 10)) (Int 1)) "n%10==1"
        , Test.test "Operator precedence: And vs Or" <|
            \_ -> exprEquals (Or (And (Eq Let (Int 1)) (Eq Let (Int 2))) (Eq Let (Int 3))) "n==1 && n==2 || n==3"
        , Test.test "Operator precedence: Cond binds weakest" <|
            \_ -> exprEquals (Cond (Eq Let (Int 1)) (Int 2) (Int 3)) "n==1 ? 2 : 3"
        , Test.test "Operator precedence: Cond with And inside" <|
            \_ -> exprEquals (Cond (And (Eq Let (Int 1)) (Eq Let (Int 2))) (Int 3) (Int 4)) "n==1 && n==2 ? 3 : 4"
        , Test.test "Operator precedence: Parentheses override precedence" <|
            \_ -> exprEquals (And (Eq Let (Int 1)) (Cond (Eq Let (Int 2)) (Int 3) (Int 4))) "n==1 && (n==2 ? 3 : 4)"
        , Test.test "Operator precedence: Mod, Lt, Ne combined" <|
            \_ -> exprEquals (Ne (Mod Let (Int 2)) (Lt Let (Int 3))) "n%2!=n<3"
        , Test.test "Operator precedence: Ne vs Mod" <|
            \_ -> exprEquals (Ne Let (Mod Let (Int 2))) "n!=n%2"
        , Test.test "Operator precedence: Ne vs Lt" <|
            \_ -> exprEquals (Ne Let (Lt Let (Int 3))) "n!=n<3"
        , Test.test "Operator precedence: Lt vs Mod" <|
            \_ -> exprEquals (Lt Let (Mod Let (Int 2))) "n<n%2"
        , Test.test "Operator precedence: Lt vs Ne" <|
            \_ -> exprEquals (Ne (Lt Let Let) (Int 3)) "n<n!=3"
        , Test.test "Operator precedence: Or vs Cond" <|
            \_ -> exprEquals (Cond (Or (Eq Let (Int 1)) (Eq Let (Int 2))) (Int 3) (Int 4)) "n==1 || n==2 ? 3 : 4"
        , Test.test "Operator precedence: And vs Cond" <|
            \_ -> exprEquals (Cond (And (Eq Let (Int 1)) (Eq Let (Int 2))) (Int 3) (Int 4)) "n==1 && n==2 ? 3 : 4"
        , Test.test "Operator precedence: Lt vs Eq" <|
            \_ -> exprEquals (Eq (Lt Let (Int 2)) (Int 3)) "n<2==3"
        , Test.test "Operator precedence: Lt vs And" <|
            \_ -> exprEquals (And (Lt Let (Int 2)) (Lt Let (Int 3))) "n<2 && n<3"
        , Test.test "Associativity: And is left-to-right" <|
            \_ -> exprEquals (And (And (Eq Let (Int 1)) (Eq Let (Int 2))) (Eq Let (Int 3))) "n==1 && n==2 && n==3"
        , Test.test "Associativity: Or is left-to-right" <|
            \_ -> exprEquals (Or (Or (Eq Let (Int 1)) (Eq Let (Int 2))) (Eq Let (Int 3))) "n==1 || n==2 || n==3"
        , Test.test "Associativity: Cond is right-to-left" <|
            \_ -> exprEquals (Cond (Eq Let (Int 1)) (Cond (Eq Let (Int 2)) (Int 3) (Int 4)) (Int 5)) "n==1 ? n==2 ? 3 : 4 : 5"
        , Test.test "Associativity: Mod left-to-right" <|
            \_ -> exprEquals (Mod (Mod Let (Int 10)) (Int 2)) "n%10%2"
        , Test.test "Associativity: Eq left-to-right" <|
            \_ -> exprEquals (Eq (Eq Let (Int 1)) (Int 2)) "n==1==2"
        , Test.test "Parentheses with nested conditionals" <|
            \_ -> exprEquals (Cond (Eq Let (Int 1)) (Cond (Eq Let (Int 2)) (Int 3) (Int 4)) (Int 5)) "n==1 ? (n==2 ? 3 : 4) : 5"
        ]
