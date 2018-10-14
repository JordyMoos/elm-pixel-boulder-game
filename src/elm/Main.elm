module Main exposing (main)

import Actor.Actor as Actor exposing (Level)
import Actor.Decoder
import Browser
import Browser.Events
import Char
import Data.Config exposing (Config)
import Data.Position exposing (Position)
import Dict exposing (Dict)
import GameState.LoadingLevel as LoadingLevel
import GameState.MainMenu as MainMenu
import GameState.PlayingLevel.PlayingLevel as PlayingLevel
import Html exposing (Html, br, button, div, text)
import Html.Events exposing (onClick)
import InputController
import Json.Decode
import List.Extra
import Maybe.Extra
import Svg
import Svg.Attributes
import Task
import Text


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
    | PlayingLevelState PlayingLevel.Model
    | ErrorState String


main : Program Json.Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = GameSpeed (Maybe Int)
    | InputControllerMsg InputController.Msg
    | LoadingLevelMsg LoadingLevel.Msg
    | AnimationFrameUpdate Float


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        config =
            { width = 12
            , height = 12
            , pixelSize = 32
            }
    in
    ( { config = config
      , flags = flags
      , inputModel = InputController.init
      , gameState = MainMenuState <| MainMenu.init config
      , gameSpeed = Just 41
      , currentTick = 0
      , timeBuffer = 0
      , maxUpdatesPerView = 4
      , debug = True
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.gameState ) of
        ( GameSpeed gameSpeed, _ ) ->
            ( { model | gameSpeed = gameSpeed }
            , Cmd.none
            )

        ( InputControllerMsg subMsg, _ ) ->
            ( { model
                | inputModel =
                    InputController.update subMsg model.inputModel
              }
            , Cmd.none
            )

        ( LoadingLevelMsg subMsg, LoadingLevelState subModel ) ->
            case LoadingLevel.update subMsg subModel of
                LoadingLevel.Stay newModel ->
                    ( { model | gameState = LoadingLevelState newModel }
                    , Cmd.none
                    )

                LoadingLevel.Failed error ->
                    ( { model | gameState = ErrorState error }
                    , Cmd.none
                    )

                LoadingLevel.Success levelConfig ->
                    gotoPlayingLevel levelConfig model

        ( AnimationFrameUpdate timeDelta, _ ) ->
            updateGameState timeDelta model

        ( _, _ ) ->
            ( model
            , Cmd.none
            )


gotoPlayingLevel : Actor.LevelConfig -> Model -> ( Model, Cmd Msg )
gotoPlayingLevel levelConfig model =
    PlayingLevel.init model.config levelConfig
        |> PlayingLevelState
        |> setGameState model
        |> (\a ->
                (\updatedModel cmds ->
                    ( updatedModel
                    , Cmd.batch cmds
                    )
                )
                    a
                    []
           )


updateGameState : Float -> Model -> ( Model, Cmd Msg )
updateGameState timeDelta givenModel =
    case givenModel.gameSpeed of
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
                                        |> (\a ->
                                                (\updatedModel cmds ->
                                                    ( updatedModel
                                                    , Cmd.batch cmds
                                                    )
                                                )
                                                    a
                                                    [ cmd ]
                                           )

                                MainMenu.LoadLevel name ->
                                    loadLevel model cmd name

                                MainMenu.LoadFlags ->
                                    case Json.Decode.decodeValue Actor.Decoder.levelConfigDecoder model.flags of
                                        Err error ->
                                            ErrorState (Debug.toString error)
                                                |> setGameState model
                                                |> setInputModel (InputController.resetWasPressed model.inputModel)
                                                |> increaseCurrentTick
                                                |> (\a ->
                                                        (\updatedModel cmds ->
                                                            ( updatedModel
                                                            , Cmd.batch cmds
                                                            )
                                                        )
                                                            a
                                                            [ cmd ]
                                                   )

                                        Ok levelConfig ->
                                            -- Need to update the ACC here..
                                            gotoPlayingLevel levelConfig model

                        PlayingLevelState stateModel ->
                            case PlayingLevel.updateTick model.currentTick model.inputModel stateModel of
                                PlayingLevel.Stay newModel ->
                                    newModel
                                        |> PlayingLevelState
                                        |> setGameState model
                                        |> setInputModel (InputController.resetWasPressed model.inputModel)
                                        |> increaseCurrentTick
                                        |> (\a ->
                                                (\updatedModel cmds ->
                                                    ( updatedModel
                                                    , Cmd.batch cmds
                                                    )
                                                )
                                                    a
                                                    [ cmd ]
                                           )

                                PlayingLevel.LoadLevel name ->
                                    loadLevel model cmd name

                                PlayingLevel.GotoMainMenu ->
                                    MainMenu.init model.config
                                        |> MainMenuState
                                        |> setGameState model
                                        |> increaseCurrentTick
                                        |> (\a ->
                                                (\updatedModel cmds ->
                                                    ( updatedModel
                                                    , Cmd.batch cmds
                                                    )
                                                )
                                                    a
                                                    [ cmd ]
                                           )

                        -- Other states do not have updateTick
                        _ ->
                            ( model, cmd )
                )
                ( givenModel
                , Cmd.none
                )
                (List.take givenModel.maxUpdatesPerView <| List.repeat ((givenModel.timeBuffer + round timeDelta) // gameSpeed) ())
                |> (\( newModel, newCmd ) ->
                        ( updateTimeBuffer (round timeDelta) gameSpeed newModel, newCmd )
                   )

        Nothing ->
            ( givenModel
            , Cmd.none
            )


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
        |> (\a ->
                (\updatedModel cmds ->
                    ( updatedModel
                    , Cmd.batch cmds
                    )
                )
                    a
                    [ cmd, Cmd.map LoadingLevelMsg newCmd ]
           )


setGameState : Model -> GameState -> Model
setGameState model gameState =
    { model | gameState = gameState }


setInputModel : InputController.Model -> Model -> Model
setInputModel inputModel model =
    { model | inputModel = inputModel }


updateTimeBuffer : Int -> Int -> Model -> Model
updateTimeBuffer time gameSpeed model =
    { model | timeBuffer = modBy gameSpeed (model.timeBuffer + time) }


increaseCurrentTick : Model -> Model
increaseCurrentTick model =
    { model | currentTick = model.currentTick + 1 }


view : Model -> Html Msg
view model =
    div
        []
        [ case model.gameState of
            MainMenuState subModel ->
                MainMenu.view subModel

            LoadingLevelState subModel ->
                LoadingLevel.view subModel |> Html.map LoadingLevelMsg

            PlayingLevelState subModel ->
                PlayingLevel.view model.currentTick subModel

            ErrorState error ->
                text <| "ERROR: " ++ error
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
                    [ Browser.Events.onAnimationFrameDelta AnimationFrameUpdate
                    ]

                Nothing ->
                    []
    in
    List.append
        [ Sub.map InputControllerMsg (InputController.subscriptions model.inputModel)
        ]
        gameSpeedSub
        |> Sub.batch
