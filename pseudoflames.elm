module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, p, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (value, placeholder)

type alias Model =
    { noteArr : List String
    , currNote : String
    }

initialModel : Model
initialModel =
    { noteArr = []
    , currNote = ""
    }

type Msg
    = Submit
    | RemoveNote Int
    | UpdateCurrNote String

update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateCurrNote str ->
            { model | currNote = str }

        Submit ->
            if String.isEmpty (String.trim model.currNote) then
                model
            else
                { model
                    | noteArr = model.noteArr ++ [ model.currNote ]
                    , currNote = ""
                }

        RemoveNote idx ->
            { model | noteArr = List.take idx model.noteArr ++ List.drop (idx + 1) model.noteArr }

view : Model -> Html Msg
view model =
    let
        noteItems : List (Html Msg)
        noteItems =
            List.indexedMap
                (\idx note ->
                    p []
                        [ text (note ++ "   ")
                        , button [ onClick (RemoveNote idx) ] [ text "Remove Note" ]
                        ]
                )
                model.noteArr

        emptyMsg : List (Html Msg)
        emptyMsg =
            if List.isEmpty model.noteArr then
                [ p [] [ text "No notes yet!" ] ]
            else
                []
    in
    div []
        (emptyMsg
            ++ noteItems
            ++ [ input
                    [ onInput UpdateCurrNote
                    , value model.currNote
                    , placeholder "Enter a note..."
                    ]
                    []
               , button [ onClick Submit ] [ text "Add Note" ]
               ]
        )

main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }