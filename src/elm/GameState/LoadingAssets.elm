module GameState.LoadingAssets exposing (..)

import Data.Config exposing (Config)
import Http
import Actor.Actor as Actor
import Canvas
import Dict
import Task
import Html exposing (Html, div, text)


type alias Model =
    { config : Config
    , levelConfig : Actor.LevelConfig
    , images : Actor.CanvasImages
    }


type Action
    = Stay Model
    | Failed String
    | Success Actor.LevelConfig Actor.CanvasImages


type Msg
    = LoadImageResponse String (Result Canvas.Error Canvas.Canvas)


init : Config -> Actor.LevelConfig -> ( Model, Cmd Msg )
init config levelConfig =
    { config = config
    , levelConfig = levelConfig
    , images = Dict.empty
    }
        ! [ downloadImages levelConfig.images ]


update : Msg -> Model -> Action
update msg model =
    case msg of
        LoadImageResponse name (Ok canvas) ->
            { model | images = Dict.insert name canvas model.images }
                |> asAction

        LoadImageResponse name (Err error) ->
            Failed <| "Error loading image" ++ (toString error)


view : Model -> Html Msg
view model =
    div [] [ text "Loading assets..." ]


asAction : Model -> Action
asAction model =
    if downloaded model == total model then
        Success model.levelConfig model.images
    else
        Stay model


total : Model -> Int
total model =
    dictLength model.levelConfig.images


downloaded : Model -> Int
downloaded model =
    dictLength model.images


dictLength : Dict.Dict comparable a -> Int
dictLength dict =
    dict
        |> Dict.toList
        |> List.length


downloadImages : Dict.Dict String String -> Cmd Msg
downloadImages images =
    Dict.toList images
        |> List.map
            (\( name, src ) ->
                Task.attempt (LoadImageResponse name) (Canvas.loadImage src)
            )
        |> Cmd.batch
