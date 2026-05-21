module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (type_, placeholder, value)

type alias Model =
    { texta : String
    , textb : String
    }

initialModel : Model
initialModel =
    { texta = ""
    , textb = ""
    }

type Msg
    = TextA String
    | TextB String

update : Msg -> Model -> Model
update msg model =
    case msg of
        TextA str ->
            { model | texta = str }

        TextB str ->
            { model | textb = str }

view : Model -> Html Msg
view model =
    let
        result =
            case ( String.toInt model.texta, String.toInt model.textb ) of
                ( Just a, Just b ) ->
                    div [] [ text ("Sum is: " ++ String.fromInt (a + b)) ]

                _ ->
                    div [] [ text "Invalid input" ]
    in
    div []
        [ input [ placeholder "First number",  onInput TextA, value model.texta ] []
        , div [] []
        , input [ placeholder "Second number", onInput TextB, value model.textb ] []
        , result
        ]

main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }