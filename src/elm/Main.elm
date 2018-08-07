module Main exposing (main)

import Html exposing (Html, text, br, div, button)
import Html.Events exposing (onClick)
import Keyboard
import Time
import Char
import List.Extra
import Dict exposing (Dict)
import Maybe.Extra
import Data.Common exposing (Tick, Position)
import Data.Config exposing (Config)
import InputController
import Actor exposing (Level)
import UpdateLoop
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
    , debug : Bool
    , gameSpeed : Maybe Int
    , currentTick : Tick
    , inputController : InputController.Model
    , timeBuffer : Int
    , gameState : GameState
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
        , debug = True
        , gameSpeed = Just 41
        , currentTick = 0
        , timeBuffer = 0
        , gameState = MainMenu <| GameState.MainMenu.init config
        }
            ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.gameState ) of
        ( GameSpeed gameSpeed, _ ) ->
            { model | gameSpeed = gameSpeed } ! []

        ( InputControllerMsg subMsg, _ ) ->
            { model
                | inputController =
                    InputController.update subMsg model.inputController
            }
                ! []

        ( LoadingLevelMsg subMsg, LoadingLevel subModel ) ->
            case GameState.LoadingLevel.update subMsg subModel of
                GameState.LoadingLevel.Stay newModel ->
                    { model | gameState = LoadingLevel newModel }

                GameState.LoadingLevel.Failed error ->
                    { model | gameState = Error error }

                GameState.LoadingLevel.Success levelConfig ->
                    if Dict.toList levelConfig.images |> List.isEmpty then
                        gotoPlayLevel model.config levelConfig Dict.empty
                    else
                        gotoLoadAssets model.config levelConfig

        ( LoadingAssetsMsg subMsg, LoadingAssets subModel ) ->
            case GameState.LoadingAssets.update subMsg subModel of
                GameState.LoadingAssets.Stay newModel ->
                    { model | gameState = LoadingAssets newModel }

                GameState.LoadingAssets.Failed error ->
                    { model | gameState = Error error }

                GameState.LoadingAssets.Success levelConfig images ->
                    gotoPlayLevel levelConfig images

        ( AnimationFrameUpdate time, _ ) ->
            updateGameState model.gameState time

        ( _, _ ) ->
            model ! []


gotoPlayLevel : Actor.LevelConfig -> Actor.CanvasImages -> Model -> Model
gotoPlayLevel levelConfig images model =
    model


gotoLoadAssets : Actor.LevelConfig -> Model
gotoLoadAssets levelConfig model =
    model



--        AnimationFrameUpdate time ->
--            case model.gameSpeed of
--                Just gameSpeed ->
--                    case model.gameState of
--                        MainMenu ->
--                            model ! []
--
--                        PlayLevel level ->
--                            List.foldr
--                                (\_ model ->
--                                    { model
--                                        | inputController = InputController.resetWasPressed model.inputController
--                                        , level = UpdateLoop.update (InputController.getCurrentDirection model.inputController) model.level
--                                        , currentTick = model.currentTick + 1
--                                    }
--                                )
--                                model
--                                (List.repeat ((model.timeBuffer + (round time)) // gameSpeed) ())
--                                |> updateTimeBuffer (round time) gameSpeed
--                                |> flip (!) []
--
--                Nothing ->
--                    model ! []


updateGameState : Time.Time -> Model -> ( Model, Cmd Msg )
updateGameState time model =
    case model.gameState of
        MainMenu stateModel ->
            case GameState.MainMenu.updateTick model.inputModel stateModel of
                GameState.MainMenu.Stay newModel ->
                    newModel
                        |> MainMenu
                        |> setGameState model
                        |> setInputModel (InputController.resetWasPressed model.inputModel)
                        |> flip (!) []

                GameState.MainMenu.LoadLevel name ->
                    let
                        ( newModel, newCmd ) =
                            GameState.LoadingLevel.init model.config name
                    in
                        newModel
                            |> LoadingLevel
                            |> setGameState model
                            |> setInputModel (InputController.resetWasPressed model.inputModel)
                            |> flip (!) [ newCmd ]

        _ ->
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


view : Model -> Html Msg
view model =
    div
        []
        [ (case model.gameState of
            MainMenu subModel ->
                GameState.MainMenu.view model.currentTick subModel

            LoadingLevel subModel ->
                GameState.LoadingLevel.view subModel

            LoadingAssets subModel ->
                GameState.LoadingAssets.view subModel

            PlayingLevel subModel ->
                GameState.PlayingLevel.view subModel

            Error error ->
                text <| "ERROR: " ++ error
          )
        , if model.debug then
            debugView
          else
            text ""
        ]


renderMenu : Html Msg
renderMenu =
    div [] []


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
            [ Sub.map InputControllerMsg (InputController.subscriptions model.inputController)
            ]
            gameSpeedSub
            |> Sub.batch
