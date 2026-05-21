module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onInput)
import Http
import Json.Decode as Decode exposing (Decoder)
import Set exposing (Set)

-- TYPES

type alias PokemonSummary =
    { name : String
    , id : String
    }

type alias PokemonData =
    { name : String
    , types : List String
    , height : Int
    , weight : Int
    , sprite : String
    }

type alias Model =
    { query : String
    , activeGenerations : Set Int
    , generationCache : Dict Int (List PokemonSummary)
    , pokemonCache : Dict String PokemonData
    , results : List PokemonData
    , loading : Bool
    , pendingFetches : Int
    }

initialModel : Model
initialModel =
    { query = ""
    , activeGenerations = Set.fromList [ 1 ]
    , generationCache = Dict.empty
    , pokemonCache = Dict.empty
    , results = []
    , loading = False
    , pendingFetches = 0
    }

type Msg
    = UpdateQuery String
    | ToggleGeneration Int Bool
    | GotGeneration Int (Result Http.Error (List PokemonSummary))
    | GotPokemon String (Result Http.Error PokemonData)

-- DECODERS

summaryListDecoder : Decoder (List PokemonSummary)
summaryListDecoder =
    Decode.field "pokemon_species"
        (Decode.list
            (Decode.map2 PokemonSummary
                (Decode.field "name" Decode.string)
                (Decode.at [ "url" ] Decode.string
                    |> Decode.map extractId
                )
            )
        )

extractId : String -> String
extractId url =
    url
        |> String.split "/"
        |> List.filter (not << String.isEmpty)
        |> List.reverse
        |> List.head
        |> Maybe.withDefault ""

pokemonDecoder : Decoder PokemonData
pokemonDecoder =
    Decode.map5 PokemonData
        (Decode.field "name" Decode.string)
        (Decode.field "types"
            (Decode.list (Decode.at [ "type", "name" ] Decode.string))
        )
        (Decode.field "height" Decode.int)
        (Decode.field "weight" Decode.int)
        (Decode.at [ "sprites", "front_default" ] Decode.string)

-- UPDATE

baseUrl : String
baseUrl =
    "https://pokeapi.co/api/v2"

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateQuery q ->
            let
                newModel =
                    { model | query = q, results = [], loading = True }
            in
            ( newModel, fetchAllActiveGenerations newModel )

        ToggleGeneration gen checked ->
            let
                newGens =
                    if checked then
                        Set.insert gen model.activeGenerations
                    else
                        Set.remove gen model.activeGenerations

                newModel =
                    { model
                        | activeGenerations = newGens
                        , results = []
                        , loading = True
                    }
            in
            ( newModel, fetchAllActiveGenerations newModel )

        GotGeneration genNum (Ok summaries) ->
            let
                updatedCache =
                    Dict.insert genNum summaries model.generationCache

                modelWithCache =
                    { model | generationCache = updatedCache }

                query =
                    String.toLower model.query

                matching =
                    if String.isEmpty query then
                        []
                    else
                        List.filter
                            (\s -> String.startsWith query s.name)
                            summaries
                            
                ( cachedResults, fetchCmds ) =
                    List.foldl
                        (\summary ( acc, cmds ) ->
                            case Dict.get summary.name model.pokemonCache of
                                Just cached ->
                                    ( acc ++ [ cached ], cmds )

                                Nothing ->
                                    ( acc
                                    , cmds
                                        ++ [ Http.get
                                                { url = baseUrl ++ "/pokemon/" ++ summary.id
                                                , expect = Http.expectJson (GotPokemon summary.name) pokemonDecoder
                                                }
                                           ]
                                    )
                        )
                        ( [], [] )
                        matching

                newPending =
                    model.pendingFetches - 1 + List.length fetchCmds

                newResults =
                    model.results ++ cachedResults

                stillLoading =
                    newPending > 0
            in
            ( { modelWithCache
                | results = newResults
                , pendingFetches = newPending
                , loading = stillLoading
              }
            , Cmd.batch fetchCmds
            )

        GotGeneration _ (Err _) ->
            ( { model
                | pendingFetches = model.pendingFetches - 1
                , loading = model.pendingFetches - 1 > 0
              }
            , Cmd.none
            )

        GotPokemon speciesName (Ok pokemon) ->
            let
                newCache =
                    Dict.insert speciesName pokemon model.pokemonCache

                newPending =
                    model.pendingFetches - 1
            in
            ( { model
                | pokemonCache = newCache
                , results = model.results ++ [ pokemon ]
                , pendingFetches = newPending
                , loading = newPending > 0
              }
            , Cmd.none
            )

        GotPokemon _ (Err _) ->
            let
                newPending =
                    model.pendingFetches - 1
            in
            ( { model
                | pendingFetches = newPending
                , loading = newPending > 0
              }
            , Cmd.none
            )


fetchAllActiveGenerations : Model -> Cmd Msg
fetchAllActiveGenerations model =
    if String.isEmpty (String.toLower model.query) then
        Cmd.none
    else
        model.activeGenerations
            |> Set.toList
            |> List.map
                (\gen ->
                    case Dict.get gen model.generationCache of
                        Just _ ->
                            -- Re-trigger GotGeneration with cached data
                            Http.get
                                { url = baseUrl ++ "/generation/" ++ String.fromInt gen
                                , expect = Http.expectJson (GotGeneration gen) summaryListDecoder
                                }

                        Nothing ->
                            Http.get
                                { url = baseUrl ++ "/generation/" ++ String.fromInt gen
                                , expect = Http.expectJson (GotGeneration gen) summaryListDecoder
                                }
                )
            |> Cmd.batch

-- VIEW


viewTypeTag : String -> Html Msg
viewTypeTag t =
    span
        [ 
        ]
        [ text t ]

viewCard : PokemonData -> Html Msg
viewCard pokemon =
    div
        [] [ img
            [ src pokemon.sprite
            , alt pokemon.name
            ] []
        , p [] [ text pokemon.name ]
        , div [] (List.map viewTypeTag pokemon.types)
        , div []
            [ span [] [ text (String.fromFloat (toFloat pokemon.height / 10) ++ " m") ]
            , span [] [ text "·" ]
            , span [] [ text (String.fromFloat (toFloat pokemon.weight / 10) ++ " kg") ]
            ]
        ]

viewCheckbox : Set Int -> Int -> Html Msg
viewCheckbox active gen =
    label
        [] [ input
            [ type_ "checkbox"
            , checked (Set.member gen active)
            , onCheck (ToggleGeneration gen)
            ] []
        , text ("Gen " ++ String.fromInt gen)
        ]

view : Model -> Html Msg
view model =
    div
        [] [ div [] [ h1 [] [ text "Pokédex" ]
        , input
            [ type_ "text"
            , placeholder "Search Pokémon by name..."
            , value model.query
            , onInput UpdateQuery
            ] []
        , div []
            (List.map (viewCheckbox model.activeGenerations) (List.range 1 9))
        , if String.isEmpty model.query then
            p [] [ text "Type a Pokémon name to begin searching." ]
        else if model.loading && List.isEmpty model.results then
            p [] [ text "Loading..." ]
        else if not model.loading && List.isEmpty model.results then
            p [] [ text "No Pokémon found." ]
        else
            div []
                [ if model.loading then
                    p [] [ text "Loading more..." ]
                        else
                    text ""
                , div [] (List.map viewCard model.results)
                ]
            ]
        ]

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }