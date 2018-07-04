module Main exposing (main)

import Html exposing (..)
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
    , components : List Component
    }


type alias TransformComponentData =
    { x : Int
    , y : Int
    }


type alias RenderComponentData =
    { string : String
    }


type alias MoveComponentData =
    ()


type Component
    = TransformComponent TransformComponentData
    | RenderComponent RenderComponentData
    | MoveComponent MoveComponentData


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
                level =
                    model.level

                actors =
                    level.actors

                keys =
                    model.keys

                newActors =
                    List.foldr
                        (\actor actors ->
                            let
                                updatedActor =
                                    List.foldr
                                        (\component actor ->
                                            actor
                                        )
                                        actor
                                        actor.components
                            in
                                actors
                         -- Need update actor in actors
                        )
                        actors
                        actors

                newLevel =
                    { level | actors = newActors }

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
                { model
                    | keys = handledPressedKeys
                    , level = newLevel
                }
                    ! []

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
                                                getTransformComponent actor.components
                                                    |> Maybe.andThen
                                                        (\componentData ->
                                                            Just <| componentData.x == x && componentData.y == y
                                                        )
                                                    |> Maybe.withDefault False
                                            )
                                            model.level.actors
                                in
                                    case maybeActor of
                                        Just actor ->
                                            getRenderComponent actor.components
                                                |> Maybe.andThen
                                                    (\componentData ->
                                                        Just <| text <| String.concat [ "[", componentData.string, "]" ]
                                                    )
                                                |> Maybe.withDefault empty

                                        Nothing ->
                                            empty
                            )
                        |> List.append [ br [] [] ]
                )
            |> List.concat
        )


getTransformComponent : List Component -> Maybe TransformComponentData
getTransformComponent components =
    List.Extra.find
        (\component ->
            case component of
                TransformComponent _ ->
                    True

                _ ->
                    False
        )
        components
        |> Maybe.andThen
            (\component ->
                case component of
                    TransformComponent data ->
                        Just data

                    _ ->
                        Nothing
            )


getRenderComponent : List Component -> Maybe RenderComponentData
getRenderComponent components =
    List.Extra.find
        (\component ->
            case component of
                RenderComponent _ ->
                    True

                _ ->
                    False
        )
        components
        |> Maybe.andThen
            (\component ->
                case component of
                    RenderComponent data ->
                        Just data

                    _ ->
                        Nothing
            )


createWall : Int -> Int -> Actor
createWall x y =
    { id = 0
    , components =
        [ TransformComponent { x = x, y = y }
        , RenderComponent { string = "#" }
        , MoveComponent ()
        ]
    }


createDirt : Int -> Int -> Actor
createDirt x y =
    { id = 0
    , components =
        [ TransformComponent { x = x, y = y }
        , RenderComponent { string = "." }
        ]
    }


createPlayer : Int -> Int -> Actor
createPlayer x y =
    { id = 0
    , components =
        [ TransformComponent { x = x, y = y }
        , RenderComponent { string = "P" }
        ]
    }


createDead : Int -> Int -> Actor
createDead x y =
    { id = 0
    , components =
        [ TransformComponent { x = x, y = y }
        , RenderComponent { string = "X" }
        ]
    }


createRock : Int -> Int -> Actor
createRock x y =
    { id = 0
    , components =
        [ TransformComponent { x = x, y = y }
        , RenderComponent { string = "O" }
        ]
    }


createDiamond : Int -> Int -> Actor
createDiamond x y =
    { id = 0
    , components =
        [ TransformComponent { x = x, y = y }
        , RenderComponent { string = "*" }
        ]
    }


empty : Html Msg
empty =
    text "[ ]"


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
