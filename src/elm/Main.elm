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


type alias Model =
    { level : Level
    , width : Int
    , height : Int
    , debug : Bool
    , gameSpeed : Maybe Time.Time
    , currentTick : Tick
    , inputController : InputController.Model
    }


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
    | GameTick Time.Time
    | GameSpeed (Maybe Time.Time)


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
    in
        { level = Actor.init levelConfig width height
        , width = width
        , height = height
        , inputController = InputController.init
        , debug = True
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
