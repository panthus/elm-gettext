module GetTextTest exposing (suite)

import Bytes.Decode as D
import Expect
import GetText
import MoFile
import Test


translations : GetText.Translations
translations =
    D.decode GetText.parser MoFile.moFile |> Maybe.withDefault GetText.default


suite : Test.Test
suite =
    Test.describe "GetText"
        [ Test.test "getLanguage returns the correct language" <|
            \_ -> Expect.equal MoFile.parsedMoFile.language (GetText.getLanguage translations)

        --t
        , Test.test "t returns the translated text with formatting applied" <|
            \_ -> Expect.equal "Hoi Jay" (GetText.t translations "Hello {name}" [ ( "name", "Jay" ) ])
        , Test.test "t returns the translated text" <|
            \_ -> Expect.equal "Hoi" (GetText.t translations "Hello" [])
        , Test.test "t returns the default text if the translation cannot be found" <|
            \_ -> Expect.equal "Welcome" (GetText.t translations "Welcome" [])
        , Test.test "t leaves format items that are not found" <|
            \_ -> Expect.equal "Hoi {name}" (GetText.t translations "Hello {name}" [])

        --tn
        , Test.test "tn returns singular when n = 1" <|
            \_ -> Expect.equal "Appel" (GetText.tn translations 1 "Apple" "Apples" [])
        , Test.test "tn returns plural when n != 1" <|
            \_ -> Expect.equal "Appels" (GetText.tn translations 2 "Apple" "Apples" [])
        , Test.test "tn applies formatting for singular" <|
            \_ -> Expect.equal "1 appel" (GetText.tn translations 1 "{amount} apple" "{amount} apples" [ ( "amount", "1" ) ])
        , Test.test "tn applies formatting for plural" <|
            \_ -> Expect.equal "5 appels" (GetText.tn translations 5 "{amount} apple" "{amount} apples" [ ( "amount", "5" ) ])
        , Test.test "tn returns the default singular text if the translation cannot be found when n = 1" <|
            \_ -> Expect.equal "Banana" (GetText.tn translations 1 "Banana" "Bananas" [])
        , Test.test "tn returns the default plural text if the translation cannot be found when n != 1" <|
            \_ -> Expect.equal "Bananas" (GetText.tn translations 2 "Banana" "Bananas" [])

        --tp
        , Test.test "tp returns context translation with formatting" <|
            \_ -> Expect.equal "Jay is verwend" (GetText.tp translations "person" "{name} is spoiled" [ ( "name", "Jay" ) ])
        , Test.test "tp returns context translation without formatting" <|
            \_ -> Expect.equal "Bedorven" (GetText.tp translations "food" "Spoiled" [])
        , Test.test "tp returns the default text if the translation cannot be found" <|
            \_ -> Expect.equal "Welcome" (GetText.tp translations "greeting" "Welcome" [])

        --tpn
        , Test.test "tpn returns singular context translation" <|
            \_ -> Expect.equal "Is verwend" (GetText.tpn translations "person" 1 "Is spoiled" "Are spoiled" [])
        , Test.test "tpn returns plural context translation" <|
            \_ -> Expect.equal "Zijn verwend" (GetText.tpn translations "person" 2 "Is spoiled" "Are spoiled" [])
        , Test.test "tpn applies formatting for singular context" <|
            \_ -> Expect.equal "1 is bedorven" (GetText.tpn translations "food" 1 "{amount} is spoiled" "{amount} are spoiled" [ ( "amount", "1" ) ])
        , Test.test "tpn applies formatting for plural context" <|
            \_ -> Expect.equal "5 zijn bedorven" (GetText.tpn translations "food" 5 "{amount} is spoiled" "{amount} are spoiled" [ ( "amount", "5" ) ])
        , Test.test "tpn returns the default singular text if the translation cannot be found when n = 1" <|
            \_ -> Expect.equal "Banana" (GetText.tpn translations "fruit" 1 "Banana" "Bananas" [])
        , Test.test "tpn returns the default plural text if the translation cannot be found when n != 1" <|
            \_ -> Expect.equal "Bananas" (GetText.tpn translations "fruit" 2 "Banana" "Bananas" [])
        ]
