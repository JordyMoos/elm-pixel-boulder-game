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
import GameState.MainMenu
import GameState.LoadingLevel
import GameState.LoadingAssets
import GameState.PlayingLevel


type alias Model =
    { config : Config
    , inputModel : InputController.Model
    , gameState : GameState
    , gameSpeed : Maybe Int
    , currentTick : Int
    , timeBuffer : Int
    , debug : Bool
    }


type GameState
    = MainMenu GameState.MainMenu.Model
    | LoadingLevel GameState.LoadingLevel.Model
    | LoadingAssets GameState.LoadingAssets.Model
    | PlayingLevel GameState.PlayingLevel.Model
    | Error String


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
    | LoadingLevelMsg GameState.LoadingLevel.Msg
    | LoadingAssetsMsg GameState.LoadingAssets.Msg
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
        , inputModel = InputController.init
        , gameState = MainMenu <| GameState.MainMenu.init config
        , gameSpeed = Just 41
        , currentTick = 0
        , timeBuffer = 0
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

        ( LoadingLevelMsg subMsg, LoadingLevel subModel ) ->
            case GameState.LoadingLevel.update subMsg subModel of
                GameState.LoadingLevel.Stay newModel ->
                    { model | gameState = LoadingLevel newModel } ! []

                GameState.LoadingLevel.Failed error ->
                    { model | gameState = Error error } ! []

                GameState.LoadingLevel.Success levelConfig ->
                    if Dict.toList levelConfig.images |> List.isEmpty then
                        gotoPlayingLevel levelConfig Dict.empty model
                    else
                        gotoLoadingAssets levelConfig model

        ( LoadingAssetsMsg subMsg, LoadingAssets subModel ) ->
            case GameState.LoadingAssets.update subMsg subModel of
                GameState.LoadingAssets.Stay newModel ->
                    { model | gameState = LoadingAssets newModel } ! []

                GameState.LoadingAssets.Failed error ->
                    { model | gameState = Error error } ! []

                GameState.LoadingAssets.Success levelConfig images ->
                    gotoPlayingLevel levelConfig images model

        ( AnimationFrameUpdate time, _ ) ->
            updateGameState time model

        ( _, _ ) ->
            model ! []


gotoPlayingLevel : Actor.LevelConfig -> Actor.CanvasImages -> Model -> ( Model, Cmd Msg )
gotoPlayingLevel levelConfig images model =
    GameState.PlayingLevel.init model.config levelConfig images
        |> PlayingLevel
        |> setGameState model
        |> flip (!) []


gotoLoadingAssets : Actor.LevelConfig -> Model -> ( Model, Cmd Msg )
gotoLoadingAssets levelConfig model =
    case GameState.LoadingAssets.init model.config levelConfig of
        ( subModel, subCmd ) ->
            ( setGameState model (LoadingAssets subModel), Cmd.map LoadingAssetsMsg subCmd )


updateGameState : Time.Time -> Model -> ( Model, Cmd Msg )
updateGameState time model =
    case model.gameSpeed of
        Just gameSpeed ->
            List.foldr
                (\_ ( model, cmd ) ->
                    case model.gameState of
                        MainMenu stateModel ->
                            case GameState.MainMenu.updateTick model.inputModel stateModel of
                                GameState.MainMenu.Stay newModel ->
                                    newModel
                                        |> MainMenu
                                        |> setGameState model
                                        |> setInputModel (InputController.resetWasPressed model.inputModel)
                                        |> increaseCurrentTick
                                        |> flip (!) [ cmd ]

                                GameState.MainMenu.LoadLevel name ->
                                    let
                                        ( newModel, newCmd ) =
                                            GameState.LoadingLevel.init model.config name
                                    in
                                        newModel
                                            |> LoadingLevel
                                            |> setGameState model
                                            |> setInputModel (InputController.resetWasPressed model.inputModel)
                                            |> increaseCurrentTick
                                            |> flip (!) [ cmd, Cmd.map LoadingLevelMsg newCmd ]

                        PlayingLevel stateModel ->
                            case GameState.PlayingLevel.updateTick model.currentTick model.inputModel stateModel of
                                GameState.PlayingLevel.Stay newModel ->
                                    newModel
                                        |> PlayingLevel
                                        |> setGameState model
                                        |> setInputModel (InputController.resetWasPressed model.inputModel)
                                        |> increaseCurrentTick
                                        |> flip (!) [ cmd ]

                                GameState.PlayingLevel.GotoMainMenu ->
                                    GameState.MainMenu.init model.config
                                        |> MainMenu
                                        |> setGameState model
                                        |> setInputModel (InputController.resetWasPressed model.inputModel)
                                        |> increaseCurrentTick
                                        |> flip (!) [ cmd ]

                        -- Other states do not have updateTick
                        _ ->
                            ( model, cmd )
                )
                (model ! [])
                (List.repeat ((model.timeBuffer + (round time)) // gameSpeed) ())
                |> (\( newModel, newCmd ) ->
                        ( updateTimeBuffer (round time) gameSpeed newModel, newCmd )
                   )

        Nothing ->
            model ! []


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
            MainMenu subModel ->
                GameState.MainMenu.view subModel

            LoadingLevel subModel ->
                GameState.LoadingLevel.view subModel |> Html.map LoadingLevelMsg

            LoadingAssets subModel ->
                GameState.LoadingAssets.view subModel |> Html.map LoadingAssetsMsg

            PlayingLevel subModel ->
                GameState.PlayingLevel.view model.currentTick subModel

            Error error ->
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
        [ text "Hint: Use the Arrow Keys"
        , br [] []
        , text "GameTick speed:"
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
