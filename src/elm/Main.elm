module Main exposing (main)

import Html exposing (Html, text, br, div, button)
import Html.Events exposing (onClick)
import Keyboard
import Time
import Char
import List.Extra
import Dict exposing (Dict)
import Maybe.Extra
import Data.Position exposing (Position)
import Data.Config exposing (Config)
import InputController
import Actor.Actor as Actor exposing (Level)
import Json.Decode
import Canvas
import Task
import AnimationFrame
import Text
import GameState.MainMenu as MainMenu
import GameState.LoadingLevel as LoadingLevel
import GameState.LoadingAssets as LoadingAssets
import GameState.PlayingLevel.PlayingLevel as PlayingLevel
import Actor.Decoder


type alias Model =
    { config : Config
    , flags : Json.Decode.Value
    , inputModel : InputController.Model
    , gameState : GameState
    , gameSpeed : Maybe Int
    , currentTick : Int
    , timeBuffer : Int
    , maxUpdatesPerView : Int
    , debug : Bool
    }


type GameState
    = MainMenuState MainMenu.Model
    | LoadingLevelState LoadingLevel.Model
    | LoadingAssetsState LoadingAssets.Model
    | PlayingLevelState PlayingLevel.Model
    | ErrorState String


main : Program Json.Decode.Value Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = GameSpeed (Maybe Int)
    | InputControllerMsg InputController.Msg
    | LoadingLevelMsg LoadingLevel.Msg
    | LoadingAssetsMsg LoadingAssets.Msg
    | AnimationFrameUpdate Time.Time


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        config =
            { width = 12
            , height = 12
            }
    in
        { config = config
        , flags = flags
        , inputModel = InputController.init
        , gameState = MainMenuState <| MainMenu.init config
        , gameSpeed = Just 41
        , currentTick = 0
        , timeBuffer = 0
        , maxUpdatesPerView = 4
        , debug = True
        }
            ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.gameState ) of
        ( GameSpeed gameSpeed, _ ) ->
            { model | gameSpeed = gameSpeed } ! []

        ( InputControllerMsg subMsg, _ ) ->
            { model
                | inputModel =
                    InputController.update subMsg model.inputModel
            }
                ! []

        ( LoadingLevelMsg subMsg, LoadingLevelState subModel ) ->
            case LoadingLevel.update subMsg subModel of
                LoadingLevel.Stay newModel ->
                    { model | gameState = LoadingLevelState newModel } ! []

                LoadingLevel.Failed error ->
                    { model | gameState = ErrorState error } ! []

                LoadingLevel.Success levelConfig ->
                    if Dict.toList levelConfig.images |> List.isEmpty then
                        gotoPlayingLevel levelConfig Dict.empty model
                    else
                        gotoLoadingAssets levelConfig model

        ( LoadingAssetsMsg subMsg, LoadingAssetsState subModel ) ->
            case LoadingAssets.update subMsg subModel of
                LoadingAssets.Stay newModel ->
                    { model | gameState = LoadingAssetsState newModel } ! []

                LoadingAssets.Failed error ->
                    { model | gameState = ErrorState error } ! []

                LoadingAssets.Success levelConfig images ->
                    gotoPlayingLevel levelConfig images model

        ( AnimationFrameUpdate time, _ ) ->
            updateGameState time model

        ( _, _ ) ->
            model ! []


gotoPlayingLevel : Actor.LevelConfig -> Actor.CanvasImages -> Model -> ( Model, Cmd Msg )
gotoPlayingLevel levelConfig images model =
    PlayingLevel.init model.config levelConfig images
        |> PlayingLevelState
        |> setGameState model
        |> flip (!) []


gotoLoadingAssets : Actor.LevelConfig -> Model -> ( Model, Cmd Msg )
gotoLoadingAssets levelConfig model =
    case LoadingAssets.init model.config levelConfig of
        ( subModel, subCmd ) ->
            ( setGameState model (LoadingAssetsState subModel), Cmd.map LoadingAssetsMsg subCmd )


updateGameState : Time.Time -> Model -> ( Model, Cmd Msg )
updateGameState time model =
    case model.gameSpeed of
        Just gameSpeed ->
            List.foldr
                (\_ ( model, cmd ) ->
                    case model.gameState of
                        MainMenuState stateModel ->
                            case MainMenu.updateTick model.inputModel stateModel of
                                MainMenu.Stay newModel ->
                                    newModel
                                        |> MainMenuState
                                        |> setGameState model
                                        |> setInputModel (InputController.resetWasPressed model.inputModel)
                                        |> increaseCurrentTick
                                        |> flip (!) [ cmd ]

                                MainMenu.LoadLevel name ->
                                    loadLevel model cmd name

                                MainMenu.LoadFlags ->
                                    case Json.Decode.decodeValue Actor.Decoder.levelConfigDecoder model.flags of
                                        Err error ->
                                            ErrorState error
                                                |> setGameState model
                                                |> setInputModel (InputController.resetWasPressed model.inputModel)
                                                |> increaseCurrentTick
                                                |> flip (!) [ cmd ]

                                        Ok levelConfig ->
                                            -- Need to update the ACC here..
                                            if Dict.toList levelConfig.images |> List.isEmpty then
                                                gotoPlayingLevel levelConfig Dict.empty model
                                            else
                                                gotoLoadingAssets levelConfig model

                        PlayingLevelState stateModel ->
                            case PlayingLevel.updateTick model.currentTick model.inputModel stateModel of
                                PlayingLevel.Stay newModel ->
                                    newModel
                                        |> PlayingLevelState
                                        |> setGameState model
                                        |> setInputModel (InputController.resetWasPressed model.inputModel)
                                        |> increaseCurrentTick
                                        |> flip (!) [ cmd ]

                                PlayingLevel.LoadLevel name ->
                                    loadLevel model cmd name

                                PlayingLevel.GotoMainMenu ->
                                    MainMenu.init model.config
                                        |> MainMenuState
                                        |> setGameState model
                                        |> increaseCurrentTick
                                        |> flip (!) [ cmd ]

                        -- Other states do not have updateTick
                        _ ->
                            ( model, cmd )
                )
                (model ! [])
                (List.take model.maxUpdatesPerView <| List.repeat ((model.timeBuffer + (round time)) // gameSpeed) ())
                |> (\( newModel, newCmd ) ->
                        ( updateTimeBuffer (round time) gameSpeed newModel, newCmd )
                   )

        Nothing ->
            model ! []


loadLevel : Model -> Cmd Msg -> String -> ( Model, Cmd Msg )
loadLevel model cmd name =
    let
        ( newModel, newCmd ) =
            LoadingLevel.init model.config name
    in
        newModel
            |> LoadingLevelState
            |> setGameState model
            |> setInputModel (InputController.resetWasPressed model.inputModel)
            |> increaseCurrentTick
            |> flip (!) [ cmd, Cmd.map LoadingLevelMsg newCmd ]


setGameState : Model -> GameState -> Model
setGameState model gameState =
    { model | gameState = gameState }


setInputModel : InputController.Model -> Model -> Model
setInputModel inputModel model =
    { model | inputModel = inputModel }


updateTimeBuffer : Int -> Int -> Model -> Model
updateTimeBuffer time gameSpeed model =
    { model | timeBuffer = (model.timeBuffer + time) % gameSpeed }


increaseCurrentTick : Model -> Model
increaseCurrentTick model =
    { model | currentTick = model.currentTick + 1 }


view : Model -> Html Msg
view model =
    div
        []
        [ (case model.gameState of
            MainMenuState subModel ->
                MainMenu.view subModel

            LoadingLevelState subModel ->
                LoadingLevel.view subModel |> Html.map LoadingLevelMsg

            LoadingAssetsState subModel ->
                LoadingAssets.view subModel |> Html.map LoadingAssetsMsg

            PlayingLevelState subModel ->
                PlayingLevel.view model.currentTick subModel

            ErrorState error ->
                text <| "ERROR: " ++ error
          )
        , if model.debug then
            debugView
          else
            text ""
        ]


debugView : Html Msg
debugView =
    div
        []
        [ text "GameTick speed:"
        , br [] []
        , div
            []
            [ button [ onClick <| GameSpeed Nothing ] [ text "Off" ]
            , button [ onClick <| GameSpeed <| Just 10000 ] [ text "0.1 fps" ]
            , button [ onClick <| GameSpeed <| Just 5000 ] [ text "0.5 fps" ]
            , button [ onClick <| GameSpeed <| Just 1000 ] [ text "1 fps" ]
            , button [ onClick <| GameSpeed <| Just 83 ] [ text "12 fps" ]
            , button [ onClick <| GameSpeed <| Just 41 ] [ text "24 fps" ]
            ]
        , div
            []
            [ text "Movement: Arrow Keys"
            , br [] []
            , text "Submit: A"
            , br [] []
            , text "Cancel: S"
            , br [] []
            , text "Start: Z"
            , br [] []
            , text "Select: X"
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        gameSpeedSub =
            case model.gameSpeed of
                Just _ ->
                    [ AnimationFrame.diffs AnimationFrameUpdate
                    ]

                Nothing ->
                    []
    in
        List.append
            [ Sub.map InputControllerMsg (InputController.subscriptions model.inputModel)
            ]
            gameSpeedSub
            |> Sub.batch
