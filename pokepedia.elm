module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, input)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick, onInput)

type alias Model =
    { temperature : String, mode : Mode }

initialModel : Model
initialModel =
    { temperature = ""
    , mode = CelciusToFahrenheit
    }

type Mode
    = CelciusToFahrenheit
    | FahrenheitToCelcius

type Msg
    = Temperature String
    | Switch

modeLabel : Mode -> String
modeLabel mode =
    case mode of
        CelciusToFahrenheit -> "Celsius to Fahrenheit"
        FahrenheitToCelcius -> "Fahrenheit to Celsius"

update : Msg -> Model -> Model
update msg model =
    case msg of
        Temperature temp ->
            { model | temperature = temp }

        Switch ->
            { model | mode =
                if model.mode == CelciusToFahrenheit then
                    FahrenheitToCelcius
                else
                    CelciusToFahrenheit
            }

convert : Mode -> String -> String
convert mode temp =
    case String.toFloat temp of
        Nothing -> "Invalid input"
        Just t ->
            case mode of
                CelciusToFahrenheit ->
                    String.fromFloat (t * 9 / 5 + 32) ++ " °F"
                FahrenheitToCelcius ->
                    String.fromFloat ((t - 32) * 5 / 9) ++ " °C"

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Switch ] [ text ("Convert from: " ++ modeLabel model.mode) ]
        , div [] []
        , input [ onInput Temperature, placeholder "Enter temperature" ] []
        , div [] [ text (convert model.mode model.temperature) ]
        ]

main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


