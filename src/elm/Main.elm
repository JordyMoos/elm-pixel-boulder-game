module Main exposing (main)

import Html exposing (..)
import Dict
import List.Extra
import Keyboard
import Time


type alias Model =
    { level : Level
    , width : Int
    , height : Int
    , keys : Keys
    }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = NoOp
    | KeyPressed Keyboard.KeyCode
    | KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode
    | GameTick Time.Time


type alias Keys =
    { left : KeyStatus
    , right : KeyStatus
    , up : KeyStatus
    , down : KeyStatus
    }


type KeyStatus
    = NotPressed
    | WasPressed
    | IsPressed


type alias Actor =
    { id : Int
    , components : Dict.Dict String Component
    }


type Component
    = TransformComponent Int Int
    | RenderComponent String


type alias Level =
    { actors : List Actor
    }


init : ( Model, Cmd Msg )
init =
    { level =
        { actors =
            [ createPlayer 5 8
            , createRock 0 0
            , createRock 10 0
            , createRock 8 2
            , createRock 7 7
            , createRock 4 2
            , createDiamond 8 11
            ]
        }
    , width = 12
    , height = 12
    , keys =
        { left = NotPressed
        , right = NotPressed
        , up = NotPressed
        , down = NotPressed
        }
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPressed keyCode ->
            (updateKeyState model IsPressed keyCode) ! []

        KeyDown keyCode ->
            (updateKeyState model IsPressed keyCode) ! []

        KeyUp keyCode ->
            (updateKeyState model WasPressed keyCode) ! []

        GameTick time ->
            let
                keys =
                    model.keys

                handlePressedKey keyStatus =
                    case keyStatus of
                        WasPressed ->
                            NotPressed

                        other ->
                            other

                handledPressedKeys =
                    { left = handlePressedKey keys.left
                    , right = handlePressedKey keys.right
                    , up = handlePressedKey keys.up
                    , down = handlePressedKey keys.down
                    }
            in
                { model | keys = handledPressedKeys } ! []

        NoOp ->
            model ! []


view : Model -> Html Msg
view model =
    div
        []
        (List.range 0 (model.height - 1)
            |> List.map
                (\y ->
                    List.range 0 (model.width - 1)
                        |> List.map
                            (\x ->
                                let
                                    maybeActor =
                                        List.Extra.find
                                            (\actor ->
                                                Dict.get "transform" actor.components
                                                    |> Maybe.andThen
                                                        (\component ->
                                                            case component of
                                                                TransformComponent cx cy ->
                                                                    Just <| cx == x && cy == y

                                                                _ ->
                                                                    Nothing
                                                        )
                                                    |> Maybe.withDefault False
                                            )
                                            model.level.actors
                                in
                                    case maybeActor of
                                        Just actor ->
                                            Dict.get "render" actor.components
                                                |> Maybe.andThen
                                                    (\component ->
                                                        case component of
                                                            RenderComponent string ->
                                                                Just <| text <| String.concat [ "[", string, "]" ]

                                                            _ ->
                                                                Nothing
                                                    )
                                                |> Maybe.withDefault empty

                                        Nothing ->
                                            empty
                            )
                        |> List.append [ br [] [] ]
                )
            |> List.concat
        )


createWall : Int -> Int -> Actor
createWall x y =
    { id = 0
    , components =
        Dict.fromList
            [ ( "transform", TransformComponent x y )
            , ( "render", RenderComponent "@" )
            ]
    }


createDirt : Int -> Int -> Actor
createDirt x y =
    { id = 0
    , components =
        Dict.fromList
            [ ( "transform", TransformComponent x y )
            , ( "render", RenderComponent "." )
            ]
    }


createPlayer : Int -> Int -> Actor
createPlayer x y =
    { id = 0
    , components =
        Dict.fromList
            [ ( "transform", TransformComponent x y )
            , ( "render", RenderComponent "P" )
            ]
    }


createDead : Int -> Int -> Actor
createDead x y =
    { id = 0
    , components =
        Dict.fromList
            [ ( "transform", TransformComponent x y )
            , ( "render", RenderComponent "X" )
            ]
    }


createRock : Int -> Int -> Actor
createRock x y =
    { id = 0
    , components =
        Dict.fromList
            [ ( "transform", TransformComponent x y )
            , ( "render", RenderComponent "O" )
            ]
    }


createDiamond : Int -> Int -> Actor
createDiamond x y =
    { id = 0
    , components =
        Dict.fromList
            [ ( "transform", TransformComponent x y )
            , ( "render", RenderComponent "*" )
            ]
    }


empty : Html Msg
empty =
    text "[ ]"


wall : Html Msg
wall =
    text "[@]"


dirt : Html Msg
dirt =
    text "[.]"


player : Html Msg
player =
    text "[P]"


dead : Html Msg
dead =
    text "[X]"


rock : Html Msg
rock =
    text "[O]"


diamond : Html Msg
diamond =
    text "[*]"


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.presses KeyPressed
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , Time.every (500 * Time.millisecond) GameTick
        ]


updateKeyState : Model -> KeyStatus -> Keyboard.KeyCode -> Model
updateKeyState model status keyCode =
    let
        keys =
            model.keys
    in
        case keyCode of
            37 ->
                let
                    newKeys =
                        { keys | left = status }
                in
                    { model | keys = newKeys }

            38 ->
                let
                    newKeys =
                        { keys | up = status }
                in
                    { model | keys = newKeys }

            39 ->
                let
                    newKeys =
                        { keys | right = status }
                in
                    { model | keys = newKeys }

            40 ->
                let
                    newKeys =
                        { keys | down = status }
                in
                    { model | keys = newKeys }

            _ ->
                model
