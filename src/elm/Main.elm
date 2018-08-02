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
import InputController
import Actor exposing (Level)
import UpdateLoop
import CanvasRenderer
import Json.Decode
import Canvas
import Task
import AnimationFrame


type alias Model =
    { level : Level
    , width : Int
    , height : Int
    , debug : Bool
    , gameSpeed : Maybe Int
    , currentTick : Tick
    , inputController : InputController.Model
    , timeBuffer : Int
    , state : GameState
    }


type GameState
    = MainMenu
    | PlayLevel Level


main : Program Json.Decode.Value Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = InputControllerMsg InputController.Msg
    | ActorMsg Actor.Msg
    | GameSpeed (Maybe Int)
    | AnimationFrameUpdate Time.Time


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        width =
            12

        height =
            12

        levelConfig =
            case Json.Decode.decodeValue Actor.levelConfigDecoder flags of
                Ok levelConfig ->
                    levelConfig

                Err error ->
                    Debug.crash error

        ( level, levelCmd ) =
            Actor.init levelConfig width height
    in
        { level = level
        , width = width
        , height = height
        , inputController = InputController.init
        , debug = True
        , gameSpeed = Just 41
        , currentTick = 0
        , timeBuffer = 0
        , gameState = MainMenu
        }
            ! [ Cmd.map ActorMsg levelCmd
              ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputControllerMsg subMsg ->
            { model
                | inputController =
                    InputController.update subMsg model.inputController
            }
                ! []

        ActorMsg subMsg ->
            { model
                | level =
                    Actor.update subMsg model.level
            }
                ! []

        GameSpeed gameSpeed ->
            { model | gameSpeed = gameSpeed } ! []

        AnimationFrameUpdate time ->
            case model.gameSpeed of
                Just gameSpeed ->
                    case model.gameState of
                        MainMenu ->
                            model ! []

                        PlayLevel level ->
                            List.foldr
                                (\_ model ->
                                    { model
                                        | inputController = InputController.resetWasPressed model.inputController
                                        , level = UpdateLoop.update (InputController.getCurrentDirection model.inputController) model.level
                                        , currentTick = model.currentTick + 1
                                    }
                                )
                                model
                                (List.repeat ((model.timeBuffer + (round time)) // gameSpeed) ())
                                |> updateTimeBuffer (round time) gameSpeed
                                |> flip (!) []

                Nothing ->
                    model ! []


updateTimeBuffer : Int -> Int -> Model -> Model
updateTimeBuffer time gameSpeed model =
    { model | timeBuffer = (model.timeBuffer + time) % gameSpeed }


view : Model -> Html Msg
view { currentTick, level, debug, gameState } =
    div
        []
        [ (case gameState of
            MainMenu ->
                renderMenu

            PlayLevel level ->
                CanvasRenderer.view currentTick level
          )
        , if debug then
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
