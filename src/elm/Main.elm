module Main exposing (main)

import Html exposing (..)
import Keyboard
import Time
import Dict exposing (Dict)
import Dict.Extra
import Maybe.Extra


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
    , components : Dict String Component
    }


type alias TransformComponentData =
    { x : Int
    , y : Int
    }


type alias CurrentPositionRenderComponentData =
    { token : String
    }


type alias AdditionalPositionsRenderComponentData =
    { positions : List RenderablePosition
    }


type alias RenderablePosition =
    { xOffset : Int
    , yOffset : Int
    , token : String
    }


type alias PlayerInputComponentData =
    ()


type Component
    = TransformComponent TransformComponentData
    | CurrentPositionRenderComponent CurrentPositionRenderComponentData
    | AdditionalPositionRenderComponent AdditionalPositionsRenderComponentData
    | PlayerInputComponent PlayerInputComponentData


type alias Level =
    { actors : Dict Int Actor
    , nextActorId : Int
    }


init : ( Model, Cmd Msg )
init =
    let
        level =
            { actors = Dict.fromList []
            , nextActorId = 1
            }
                |> addPlayer 5 8
                |> addRock 1 1
    in
        { level = level
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
                        (\( actorId, actor ) actors ->
                            let
                                updatedActor =
                                    List.foldr
                                        (\( key, component ) actor ->
                                            case component of
                                                PlayerInputComponent _ ->
                                                    handleUpdatePlayerInputComponent keys actor

                                                _ ->
                                                    actor
                                        )
                                        actor
                                        (Dict.toList actor.components)
                            in
                                Dict.insert actorId updatedActor actors
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


handleUpdatePlayerInputComponent : Keys -> Actor -> Actor
handleUpdatePlayerInputComponent keys actor =
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
                                getPixel x y model.level.actors
                                    |> Maybe.withDefault empty
                            )
                        |> List.append [ br [] [] ]
                )
            |> List.concat
        )


getPixel : Int -> Int -> Dict Int Actor -> Maybe (Html Msg)
getPixel x y actors =
    Dict.foldr
        (\actorId actor acc ->
            (getCurrentPositionRenderComponent actor.components
                |> Maybe.andThen
                    (\renderData ->
                        getTransformComponent actor.components
                            |> Maybe.andThen
                                (\transformData ->
                                    if transformData.x == x && transformData.y == y then
                                        Just renderData.token
                                    else
                                        Nothing
                                )
                    )
            )
                :: (getAdditionalPositionRenderComponent actor.components
                        |> Maybe.andThen
                            (\renderData ->
                                List.map
                                    (\{ xOffset, yOffset, token } ->
                                        getTransformComponent actor.components
                                            |> Maybe.andThen
                                                (\transformData ->
                                                    if transformData.x + xOffset == x && transformData.y + yOffset == y then
                                                        Just token
                                                    else
                                                        Nothing
                                                )
                                    )
                                    renderData.positions
                                    |> Maybe.Extra.values
                                    |> List.head
                            )
                   )
                :: acc
        )
        []
        actors
        |> Maybe.Extra.values
        |> List.head
        |> Maybe.andThen
            (\token ->
                Just <| text <| "[" ++ token ++ "]"
            )



--getPixelOld : Int -> Int -> List Actor -> Maybe (Html Msg)
--getPixelOld x y actors =
--    let
--        maybeActor =
--            Dict.Extra.find
--                (\actorId actor ->
--                    getTransformComponent actor.components
--                        |> Maybe.andThen
--                            (\componentData ->
--                                Just <| componentData.x == x && componentData.y == y
--                            )
--                        |> Maybe.withDefault False
--                )
--                actors
--    in
--        case maybeActor of
--            Just ( actorId, actor ) ->
--                getCurrentPositionRenderComponent actor.components
--                    |> Maybe.andThen
--                        (\componentData ->
--                            Just <| text <| String.concat [ "[", componentData.token, "]" ]
--                        )
--
--            Nothing ->
--                Nothing


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


getCurrentPositionRenderComponent : Dict String Component -> Maybe CurrentPositionRenderComponentData
getCurrentPositionRenderComponent components =
    Dict.get "render" components
        |> Maybe.andThen
            (\component ->
                case component of
                    CurrentPositionRenderComponent data ->
                        Just data

                    _ ->
                        Nothing
            )


getAdditionalPositionRenderComponent : Dict String Component -> Maybe AdditionalPositionsRenderComponentData
getAdditionalPositionRenderComponent components =
    Dict.get "additional-render" components
        |> Maybe.andThen
            (\component ->
                case component of
                    AdditionalPositionRenderComponent data ->
                        Just data

                    _ ->
                        Nothing
            )


addPlayer : Int -> Int -> Level -> Level
addPlayer x y level =
    let
        actors =
            Dict.insert
                level.nextActorId
                (createPlayer level.nextActorId x y)
                level.actors
    in
        { level | actors = actors, nextActorId = level.nextActorId + 1 }


createPlayer : Int -> Int -> Int -> Actor
createPlayer id x y =
    { id = id
    , components =
        Dict.fromList
            [ ( "transform", TransformComponent { x = x, y = y } )
            , ( "render", CurrentPositionRenderComponent { token = "P" } )
            , ( "additional-render"
              , AdditionalPositionRenderComponent
                    { positions =
                        [ { xOffset = -1
                          , yOffset = 0
                          , token = "p"
                          }
                        , { xOffset = 1
                          , yOffset = 0
                          , token = "p"
                          }
                        ]
                    }
              )
            , ( "player-input", PlayerInputComponent () )
            ]
    }


addRock : Int -> Int -> Level -> Level
addRock x y level =
    let
        actors =
            Dict.insert
                level.nextActorId
                (createRock level.nextActorId x y)
                level.actors
    in
        { level | actors = actors, nextActorId = level.nextActorId + 1 }


createRock : Int -> Int -> Int -> Actor
createRock id x y =
    { id = id
    , components =
        Dict.fromList
            [ ( "transform", TransformComponent { x = x, y = y } )
            , ( "render", CurrentPositionRenderComponent { token = "O" } )
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
