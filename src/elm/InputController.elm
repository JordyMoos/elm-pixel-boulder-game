port module InputController exposing
    ( Key(..)
    , KeyStatus(..)
    , KeyStatuses
    , Model
    , Msg
    , getCurrentDirection
    , getOrderedPressedKeys
    , init
    , isKeyPressed
    , resetWasPressed
    , submitKey
    , subscriptions
    , update
    )

import Browser.Events
import Data.Direction as Direction exposing (Direction)
import Dict exposing (Dict)
import Json.Decode as Decode
import Maybe.Extra
import Ports


type alias Model =
    { keys : KeyStatuses
    , counter : Int
    }


type alias KeyCode =
    String


type alias KeyStatuses =
    Dict KeyCode KeyStatus


type KeyStatus
    = NotPressed
    | WasPressed Int
    | IsPressed Int


type Msg
    = KeyPressed KeyCode
    | KeyDown KeyCode
    | KeyUp KeyCode


keyMap : Dict KeyCode Key
keyMap =
    Dict.fromList
        [ ( leftArrow, LeftKey )
        , ( upArrow, UpKey )
        , ( rightArrow, RightKey )
        , ( downArrow, DownKey )
        , ( submitKey, SubmitKey )
        , ( cancelKey, CancelKey )
        , ( startKey, StartKey )
        , ( escKey, StartKey )
        , ( selectKey, SelectKey )
        ]


type Key
    = LeftKey
    | UpKey
    | RightKey
    | DownKey
    | SubmitKey
    | CancelKey
    | StartKey
    | SelectKey


leftArrow : KeyCode
leftArrow =
    "ArrowLeft"


upArrow : KeyCode
upArrow =
    "ArrowUp"


rightArrow : KeyCode
rightArrow =
    "ArrowRight"


downArrow : KeyCode
downArrow =
    "ArrowDown"


escKey : KeyCode
escKey =
    "Escape"


submitKey : KeyCode
submitKey =
    "a"


cancelKey : KeyCode
cancelKey =
    "s"


startKey : KeyCode
startKey =
    "z"


selectKey : KeyCode
selectKey =
    "x"


keyCodeToDirection : Dict KeyCode Direction
keyCodeToDirection =
    Dict.fromList
        [ ( leftArrow, Direction.Left )
        , ( upArrow, Direction.Up )
        , ( rightArrow, Direction.Right )
        , ( downArrow, Direction.Down )
        ]


init : Model
init =
    { keys =
        Dict.fromList
            [ ( leftArrow, NotPressed )
            , ( upArrow, NotPressed )
            , ( rightArrow, NotPressed )
            , ( downArrow, NotPressed )
            , ( submitKey, NotPressed )
            , ( cancelKey, NotPressed )
            , ( startKey, NotPressed )
            , ( escKey, NotPressed )
            , ( selectKey, NotPressed )
            ]
    , counter = 0
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        KeyPressed keyCode ->
            handleIsPressed keyCode model

        KeyDown keyCode ->
            handleIsPressed keyCode model

        KeyUp keyCode ->
            handleWasPressed keyCode model


getCurrentDirection : Model -> Maybe Direction
getCurrentDirection model =
    keyCodeToDirection
        |> Dict.toList
        |> List.map
            (\( code, direction ) ->
                model.keys
                    |> getCounter code
                    |> Maybe.map (\counter -> ( counter, direction ))
            )
        |> Maybe.Extra.values
        |> List.sortBy (\( code, direction ) -> code)
        |> List.head
        |> Maybe.map Tuple.second


isKeyPressed : Model -> KeyCode -> Bool
isKeyPressed model keyCode =
    case Dict.get keyCode model.keys of
        Just (IsPressed _) ->
            True

        Just (WasPressed _) ->
            True

        _ ->
            False


resetWasPressed : Model -> Model
resetWasPressed model =
    { model
        | keys =
            Dict.map
                (\keyCode status ->
                    case status of
                        WasPressed _ ->
                            NotPressed

                        _ ->
                            status
                )
                model.keys
    }


handleIsPressed : KeyCode -> Model -> Model
handleIsPressed keyCode model =
    model
        |> incrementCounter
        |> (\updatedModel ->
                { updatedModel
                    | keys =
                        updateKey keyCode (IsPressed updatedModel.counter) updatedModel.keys
                }
           )


handleWasPressed : KeyCode -> Model -> Model
handleWasPressed keyCode model =
    case Dict.get keyCode model.keys of
        Just (IsPressed counter) ->
            { model
                | keys = updateKey keyCode (WasPressed counter) model.keys
            }

        _ ->
            model


incrementCounter : Model -> Model
incrementCounter model =
    { model | counter = model.counter + 1 }


getCounter : KeyCode -> KeyStatuses -> Maybe Int
getCounter keyCode keys =
    Dict.get keyCode keys
        |> Maybe.andThen
            (\keyStatus ->
                case keyStatus of
                    IsPressed counter ->
                        Just counter

                    WasPressed counter ->
                        Just counter

                    NotPressed ->
                        Nothing
            )


updateKey : KeyCode -> KeyStatus -> KeyStatuses -> KeyStatuses
updateKey keyCode status keys =
    -- Do not store keys we do not care about
    if Dict.member keyCode keys then
        Dict.insert keyCode status keys

    else
        keys


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyPress (Decode.map KeyPressed (Decode.field "key" Decode.string))
        , Browser.Events.onKeyDown (Decode.map KeyDown (Decode.field "key" Decode.string))
        , Browser.Events.onKeyUp (Decode.map KeyUp (Decode.field "key" Decode.string))
        , Ports.keyDown KeyDown
        , Ports.keyUp KeyUp
        ]


getOrderedPressedKeys : Model -> List Key
getOrderedPressedKeys model =
    model.keys
        |> Dict.toList
        |> List.sortBy
            (\( key, status ) ->
                case status of
                    NotPressed ->
                        0

                    WasPressed tick ->
                        tick

                    IsPressed tick ->
                        tick
            )
        |> List.filter (\( key, status ) -> status /= NotPressed)
        |> List.map Tuple.first
        |> List.map (\a -> Dict.get a keyMap)
        |> Maybe.Extra.values
