module GetText exposing (Translations, default, getLanguage, parser, t, tn, tp, tpn)

import Array
import Bytes.Decode as D
import Dict
import MoFileParser exposing (MoFile)
import PluralFormsParser exposing (Expr(..), Value(..))
import Regex exposing (Regex)


{-| The translations for a specific language.
-}
type Translations
    = Translations MoFile


{-| Takes a binary [MO file](https://www.gnu.org/software/gettext/manual/html_node/MO-Files.html) and parses the file so
it can be used for translations.

The MO file is required to have a Language and Plural-Forms
[header](https://www.gnu.org/software/gettext/manual/html_node/Header-Entry.html) so we can parse it.

-}
parser : D.Decoder Translations
parser =
    MoFileParser.parser |> D.map Translations


{-| Create the default `Translations`. It defaults to language `en` and the English plural forms rules. It doesn't
contain any translations and will use the default text and pluralText specified by the `t*` functions.
-}
default : Translations
default =
    Translations
        { headers = Dict.empty
        , pluralForms = { nPlurals = 2, plural = Ne Let (Int 1) }
        , language = "en"
        , translations = Dict.empty
        }


{-| The language of the translations, in format `ll`, `ll_CC` or `ll_CC@variant` where:

  - `ll` is an ISO 639 two-letter language code (lowercase)
  - `CC` is an ISO 3166 two-letter country code (uppercase)
  - `variant` is a variant designator (lowercase)

See the [Language header](https://www.gnu.org/software/gettext/manual/html_node/Header-Entry.html) for more info.

-}
getLanguage : Translations -> String
getLanguage (Translations translations) =
    translations.language


formatRegex : Regex
formatRegex =
    Regex.fromString "{(\\w+)}" |> Maybe.withDefault Regex.never


{-| The text can contain format items in format `{<name>}` where `<name>` can be letters, numbers and underscore.
The format items must have a unique name, if there are duplicates the last format item wins. If the format
item does not exist it will remain.
-}
format : List ( String, String ) -> String -> String
format args text =
    let
        dictArgs =
            Dict.fromList args
    in
    Regex.replace formatRegex
        (\m ->
            m.submatches
                |> List.head
                |> Maybe.withDefault Nothing
                |> Maybe.map (\a -> Dict.get a dictArgs |> Maybe.withDefault ("{" ++ a ++ "}"))
                |> Maybe.withDefault ""
        )
        text


tHelper : Translations -> Maybe String -> String -> List ( String, String ) -> String
tHelper (Translations translations) context text args =
    translations.translations
        |> Dict.get (context |> Maybe.map (\c -> c ++ "\u{0004}" ++ text) |> Maybe.withDefault text)
        |> Maybe.andThen (Array.get 0)
        |> Maybe.withDefault text
        |> format args


{-| Returns the translation of the given text and formats it with the given args. If the translation cannot be found the
default text is returned.

The text can contain format items in format `{<name>}` where `<name>` can be letters, numbers and underscore.
The format items must have a unique name, if there are duplicates the last format item wins. If the format
item does not exist it will remain.

    t translations "Hello {name}" [ ( "name", "Jay" ) ]

    t translations "Hello" []

-}
t : Translations -> String -> List ( String, String ) -> String
t translations text args =
    tHelper translations Nothing text args


tnHelper : Translations -> Maybe String -> Int -> String -> String -> List ( String, String ) -> String
tnHelper (Translations translations) context n text pluralText args =
    (case PluralFormsParser.interpret translations.pluralForms.plural n of
        Ok v ->
            translations.translations
                |> Dict.get (context |> Maybe.map (\c -> c ++ "\u{0004}" ++ text) |> Maybe.withDefault text)
                |> Maybe.andThen (Array.get v)

        _ ->
            Nothing
    )
        |> Maybe.withDefault
            (if n /= 1 then
                pluralText

             else
                text
            )
        |> format args


{-| Returns the translation of the given text or pluralText based on passing amount n to the
[plural-forms rule](https://www.gnu.org/software/gettext/manual/html_node/Translating-plural-forms.html) for the
language and formats it with the given args. Note that it expects the default language to only have 2 plural forms, but
translations can have any amount of plural forms. If the translation cannot be found the default text or pluralText is
returned.

The text and pluralText can contain format items in format `{<name>}` where `<name>` can be letters, numbers and
underscore. The format items must have a unique name. If there are duplicates the last format item wins. If the format
item does not exist it will remain.

    tn translations 1 "Apple" "Apples" []

    tn translations 1 "{amount} apple" "{amount} apples" [ ( "amount", "1" ) ]

    tn translations 5 "{amount} apple" "{amount} apples" [ ( "amount", "5" ) ]

-}
tn : Translations -> Int -> String -> String -> List ( String, String ) -> String
tn translations n text pluralText args =
    tnHelper translations Nothing n text pluralText args


{-| Returns the translation of the given text and formats it with the given args. The context is used to indicate to the
translator that text requires a particular translation. Only use contexts when the translation depends on the context
such that the same text is normally only translated once. Note empty context is different from no context. If the
translation cannot be found the default text is returned.

The text can contain format items in format `{<name>}` where `<name>` can be letters, numbers and underscore.
The format items must have a unique name, if there are duplicates the last format item wins. If the format
item does not exist it will remain.

    tp translations "person" "{name} is spoiled" [ ( "name", "Jay" ) ]

    tp translations "food" "Spoiled" []

-}
tp : Translations -> String -> String -> List ( String, String ) -> String
tp translations context text args =
    tHelper translations (Just context) text args


{-| Returns the translation of the given text or pluralText based on passing amount n to the
[plural-forms rule](https://www.gnu.org/software/gettext/manual/html_node/Translating-plural-forms.html) for the
language and formats it with the given args. Note that it expects the default language to only have 2 plural forms, but
translations can have any amount of plural forms. If the translation cannot be found the default text or pluralText is
returned.

The context is used to indicate to the translator that text and pluralText require a particular translation. Only use
contexts when the translation depends on the context such that the same text is normally only translated once. Note
empty context is different from no context.

The text and pluralText can contain format items in format `{<name>}` where `<name>` can be letters, numbers and
underscore. The format items must have a unique name. If there are duplicates the last format item wins. If the format
item does not exist it will remain.

    tpn translations "person" 1 "Is spoiled" "Are spoiled" []

    tpn translations "food" 1 "{amount} is spoiled" "{amount} are spoiled" [ ( "amount", "1" ) ]

    tpn translations "food" 5 "{amount} is spoiled" "{amount} are spoiled" [ ( "amount", "5" ) ]

-}
tpn : Translations -> String -> Int -> String -> String -> List ( String, String ) -> String
tpn translations context n text pluralText args =
    tnHelper translations (Just context) n text pluralText args
