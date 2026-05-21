module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http

-- MODEL

type Model
    = Idle String           -- waiting; String = current URL in textbox
    | LoadingJoke String    -- fetching; String = URL used
    | ShowingJoke String    -- success
    | ErrorPage String      -- failure

init : () -> ( Model, Cmd Msg )
init _ =
    ( Idle "", Cmd.none )   -- no fetch on start

-- MSG

type Msg
    = MsgChangeUrl String
    | MsgFetch
    | MsgGotJoke (Result Http.Error String)

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgChangeUrl newUrl ->
            let
                -- preserve current URL in textbox regardless of state
                _ = model
            in
            ( Idle newUrl, Cmd.none )

        MsgFetch ->
            case model of
                Idle url ->
                    ( LoadingJoke url, fetchFrom url )
                _ ->
                    ( model, Cmd.none )

        MsgGotJoke result ->
            case result of
                Ok joke ->
                    ( ShowingJoke joke, Cmd.none )
                Err err ->
                    ( ErrorPage (Debug.toString err), Cmd.none )

-- HTTP

fetchFrom : String -> Cmd Msg
fetchFrom url =
    Http.get
        { url = url
        , expect = Http.expectString MsgGotJoke
        }

-- VIEW

view : Model -> Html Msg
view model =
    let
        currentUrl =
            case model of
                Idle url      -> url
                LoadingJoke u -> u
                _             -> ""
    in
    div []
        [ input
            [ value currentUrl
            , placeholder "Enter API URL..."
            , onInput MsgChangeUrl
            ]
            []
        , button [ onClick MsgFetch ] [ text "Fetch" ]
        , div []
            [ case model of
                Idle _        -> text ""
                LoadingJoke _ -> text "Loading..."
                ShowingJoke j -> text j
                ErrorPage e   -> text ("Error: " ++ e)
            ]
        ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

-- MAIN

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }