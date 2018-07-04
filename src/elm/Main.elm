module Main exposing (main)

import Html exposing (..)
import Keyboard
import Time
import Dict exposing (Dict)
import Dict.Extra


type alias Model =
    { level : Level
    , width : Int
    , height : Int
    , keys : Keys
    , nextActorId : Int
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
    , components : Dict String Component
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
    { actors : Dict Int Actor
    }


init : ( Model, Cmd Msg )
init =
    { level =
        { actors =
            Dict.fromList
                [ ( 1, createPlayer 1 5 8 )
                , ( 2, createRock 2 0 0 )

                --            , createRock 10 0
                --            , createRock 8 2
                --            , createRock 7 7
                --            , createRock 4 2
                --            , createDiamond 8 11
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
    , nextActorId = 2
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
                        (\( actorId, actor ) actors ->
                            let
                                updatedActor =
                                    List.foldr
                                        (\( key, component ) actor ->
                                            case component of
                                                MoveComponent _ ->
                                                    handleUpdateMoveComponent keys actor

                                                _ ->
                                                    actor
                                        )
                                        actor
                                        (Dict.toList actor.components)
                            in
                                Dict.insert actorId updatedActor actors
                         -- Need update actor in actors
                        )
                        actors
                        (Dict.toList actors)

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


handleUpdateMoveComponent : Keys -> Actor -> Actor
handleUpdateMoveComponent keys actor =
    case getTransformComponent actor.components of
        Just data ->
            let
                newX =
                    if isMoving keys.left then
                        data.x - 1
                    else if isMoving keys.right then
                        data.x + 1
                    else
                        data.x

                newY =
                    if isMoving keys.up then
                        data.y - 1
                    else if isMoving keys.down then
                        data.y + 1
                    else
                        data.y

                newComponents =
                    Dict.insert
                        "transform"
                        (TransformComponent { data | x = newX, y = newY })
                        actor.components
            in
                { actor | components = newComponents }

        _ ->
            actor


isMoving : KeyStatus -> Bool
isMoving status =
    case status of
        NotPressed ->
            False

        _ ->
            True


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
                                        Dict.Extra.find
                                            (\actorId actor ->
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
                                        Just ( actorId, actor ) ->
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


getTransformComponent : Dict String Component -> Maybe TransformComponentData
getTransformComponent components =
    Dict.get "transform" components
        |> Maybe.andThen
            (\component ->
                case component of
                    TransformComponent data ->
                        Just data

                    _ ->
                        Nothing
            )


getRenderComponent : Dict String Component -> Maybe RenderComponentData
getRenderComponent components =
    Dict.get "render" components
        |> Maybe.andThen
            (\component ->
                case component of
                    RenderComponent data ->
                        Just data

                    _ ->
                        Nothing
            )



--createWall : Int -> Int -> Actor
--createWall x y =
--    { id = 0
--    , components =
--        [ TransformComponent { x = x, y = y }
--        , RenderComponent { string = "#" }
--        ]
--    }
--createDirt : Int -> Int -> Actor
--createDirt x y =
--    { id = 0
--    , components =
--        [ TransformComponent { x = x, y = y }
--        , RenderComponent { string = "." }
--        ]
--    }


createPlayer : Int -> Int -> Int -> Actor
createPlayer id x y =
    { id = id
    , components =
        Dict.fromList
            [ ( "transform", TransformComponent { x = x, y = y } )
            , ( "render", RenderComponent { string = "P" } )
            , ( "move", MoveComponent () )
            ]
    }



--createDead : Int -> Int -> Actor
--createDead x y =
--    { id = 0
--    , components =
--        [ TransformComponent { x = x, y = y }
--        , RenderComponent { string = "X" }
--        ]
--    }


createRock : Int -> Int -> Int -> Actor
createRock id x y =
    { id = id
    , components =
        Dict.fromList
            [ ( "transform", TransformComponent { x = x, y = y } )
            , ( "render", RenderComponent { string = "O" } )
            ]
    }



--createDiamond : Int -> Int -> Actor
--createDiamond x y =
--    { id = 0
--    , components =
--        [ TransformComponent { x = x, y = y }
--        , RenderComponent { string = "*" }
--        ]
--    }


empty : Html Msg
empty =
    text "[ ]"


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.presses KeyPressed
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , Time.every (200 * Time.millisecond) GameTick
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
