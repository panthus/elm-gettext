module Translation exposing (main)

import Browser
import GetText as T
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http


type Msg
    = SelectLanguage String
    | RetrievedTranslations (Result Http.Error T.Translations)


main : Program () T.Translations Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }


init : flags -> ( T.Translations, Cmd Msg )
init _ =
    ( T.default, Cmd.none )


update : Msg -> T.Translations -> ( T.Translations, Cmd Msg )
update msg model =
    case msg of
        SelectLanguage "nl_NL" ->
            ( model, Http.get { url = "./locale/nl_NL.mo", expect = Http.expectBytes RetrievedTranslations T.parser } )

        SelectLanguage _ ->
            ( T.default, Cmd.none )

        RetrievedTranslations result ->
            ( result |> Result.withDefault model, Cmd.none )


view : T.Translations -> Html Msg
view translations =
    article []
        [ h1 [] [ T.t translations "Example translations" [] |> text ]
        , p []
            [ label [ for "language" ] [ T.t translations "Select language" [] |> text ]
            , select [ id "language", onInput SelectLanguage ]
                [ option [ value "nl_NL", selected (T.getLanguage translations == "nl_NL") ] [ T.t translations "Dutch" [] |> text ]
                , option [ value "en", selected (T.getLanguage translations == "en") ] [ T.t translations "English" [] |> text ]
                ]
            ]
        , p [] [ T.t translations "Hello {name}" [ ( "name", "Jay" ) ] |> text ]
        , p [] [ T.tp translations "food" "Spoiled" [] |> text ]
        , p [] [ T.tn translations 1 "Apple" "Apples" [] |> text ]
        , p [] [ T.tpn translations "food" 2 "{amount} is spoiled" "{amount} are spoiled" [ ( "amount", "2" ) ] |> text ]
        ]
