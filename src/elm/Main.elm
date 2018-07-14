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


defaultCameraBorderSize : Int
defaultCameraBorderSize =
    3


type alias Model =
    { level : Level
    , width : Int
    , height : Int
    , debug : Bool
    , gameSpeed : Maybe Time.Time
    , currentTick : Tick
    , inputController : InputController.Model
    }


type alias Flags =
    { debug : Bool
    , scene : List String
    }


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = InputControllerMsg InputController.Msg
    | GameTick Time.Time
    | GameSpeed (Maybe Time.Time)


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        width =
            12

        height =
            12

        level =
            List.indexedMap
                (,)
                flags.scene
                |> List.foldr
                    (\( y, line ) level ->
                        List.indexedMap
                            (,)
                            (String.toList line)
                            |> List.foldr
                                (\( x, char ) level ->
                                    case Char.toUpper char of
                                        '#' ->
                                            Actor.addStrongWall x y level

                                        '|' ->
                                            Actor.addWall x y level

                                        '.' ->
                                            Actor.addDirt x y level

                                        'P' ->
                                            Actor.addPlayer x y defaultCameraBorderSize level

                                        'O' ->
                                            Actor.addRock x y level

                                        '0' ->
                                            Actor.addRock x y level

                                        '*' ->
                                            Actor.addDiamond x y level

                                        'E' ->
                                            Actor.addEnemy x y level

                                        '=' ->
                                            Actor.addDynamite x y level

                                        _ ->
                                            level
                                )
                                level
                    )
                    { actors = Dict.fromList []
                    , positionIndex = Dict.fromList []
                    , nextActorId = 1
                    , diamonds =
                        { total = 0
                        , collected = 0
                        }
                    , view =
                        { position = { x = 0, y = 0 }
                        , width = width
                        , height = height
                        }
                    }
    in
        { level = level
        , width = width
        , height = height
        , inputController = InputController.init
        , debug = flags.debug
        , gameSpeed = Just <| 40 * Time.millisecond
        , currentTick = 0
        }
            ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputControllerMsg subMsg ->
            { model
                | inputController =
                    InputController.update subMsg model.inputController
            }
                ! []

        GameSpeed gameSpeed ->
            { model | gameSpeed = gameSpeed } ! []

        GameTick _ ->
            let
                _ =
                    Debug.log "GameTick" ""
            in
                { model
                    | inputController = InputController.resetWasPressed model.inputController
                    , level = UpdateLoop.update (InputController.getCurrentDirection model.inputController) model.level
                    , currentTick = model.currentTick + 1
                }
                    ! []


view : Model -> Html Msg
view model =
    div
        []
        [ CanvasRenderer.view model.currentTick model.level
        , debugView model
        ]


debugView : Model -> Html Msg
debugView model =
    if model.debug then
        div
            []
            [ text "Hint: Use the Arrow Keys"
            , br [] []
            , text "GameTick speed:"
            , br [] []
            , div
                []
                [ button [ onClick <| GameSpeed Nothing ] [ text "Off" ]
                , button [ onClick <| GameSpeed (Just <| 10 * Time.second) ] [ text "0.1 fps" ]
                , button [ onClick <| GameSpeed (Just <| 5 * Time.second) ] [ text "0.5 fps" ]
                , button [ onClick <| GameSpeed (Just <| 1 * Time.second) ] [ text "1 fps" ]
                , button [ onClick <| GameSpeed (Just <| 80 * Time.millisecond) ] [ text "12 fps" ]
                , button [ onClick <| GameSpeed (Just <| 40 * Time.millisecond) ] [ text "24 fps" ]
                ]
            ]
    else
        text ""


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        gameSpeedSub =
            case model.gameSpeed of
                Just delay ->
                    [ Time.every delay GameTick ]

                Nothing ->
                    []
    in
        List.append
            [ Sub.map InputControllerMsg (InputController.subscriptions model.inputController) ]
            gameSpeedSub
            |> Sub.batch
