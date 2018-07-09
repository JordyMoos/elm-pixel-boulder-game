module Main exposing (main)

import Html exposing (Html, text, br, div, button)
import Html.Events exposing (onClick)
import Keyboard
import Time
import List.Extra
import Dict exposing (Dict)
import Dict.Extra
import Maybe.Extra
import Color exposing (Color)
import Canvas
import Canvas.Point


type alias Tick =
    Int


movingTicks : Tick
movingTicks =
    5


pixelSize : Int
pixelSize =
    30


type alias Model =
    { level : Level
    , width : Int
    , height : Int
    , keys : Keys
    , debug : Bool
    , gameSpeed : Maybe Time.Time
    , currentTick : Tick
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
    | GameSpeed (Maybe Time.Time)


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
    { position : Position
    , movingState : MovingState
    }


type alias TransformRenderComponentData =
    { color : Color
    }


type alias AdditionalPositionsRenderComponentData =
    { positions : List RenderablePosition
    }


type alias RenderablePosition =
    { offset : Position
    , color : Color
    }


type alias Position =
    { x : Int
    , y : Int
    }


type alias MovingTowardsData =
    { position : Position
    , startTick : Tick
    , endTick : Tick
    , completionPercentage : Float
    }


type MovingState
    = NotMoving
    | MovingTowards MovingTowardsData


type Shape
    = Circle
    | Square


type Direction
    = Left
    | Up
    | Right
    | Down


type alias PhysicsComponentData =
    { mass : Int
    , shape : Shape
    , affectedByGravity : Bool
    }


type alias AIComponentData =
    { previousDirection : Direction }


type alias CameraComponentData =
    { borderSize : Int
    }


type Component
    = TransformComponent TransformComponentData
    | TransformRenderComponent TransformRenderComponentData
    | AdditionalPositionRenderComponent AdditionalPositionsRenderComponentData
    | PlayerInputComponent
    | DiamondCollectorComponent
    | DiamondComponent
    | SquashableComponent
    | CanSquashComponent
    | PhysicsComponent PhysicsComponentData
    | RigidComponent
    | AIComponent AIComponentData
    | CameraComponent CameraComponentData


type alias Level =
    { actors : Dict Int Actor
    , nextActorId : Int
    , diamonds :
        { total : Int
        , collected : Int
        }
    , view :
        { position : Position
        , width : Int
        , height : Int
        }
    }


init : ( Model, Cmd Msg )
init =
    let
        width =
            12

        height =
            12

        level =
            { actors = Dict.fromList []
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
                |> addPlayer 3 3 3
                |> addDiamond 0 10
                |> addRock 4 6
                |> addRock 6 6
                |> addDirt 4 4
                |> addDirt 5 4
                |> addDirt 6 4
                |> addDiamond 6 3
                |> addDirt 2 7
                |> addDirt 3 7
                |> addDirt 4 7
                |> addDirt 5 7
                |> addDirt 6 7
                |> addDirt 6 7
                |> addDirt 7 7
                |> addDirt 8 7
                |> addDirt 9 7
                |> addDirt 10 7
                |> addDirt 11 7
                |> addDirt 12 7
                |> addDirt 13 7
                |> addDirt 14 7
                |> addDirt 15 7
                |> addDirt 3 7
                |> addDirt 2 8
                |> addDirt 8 8
                |> addDirt 10 8
                |> addDirt 2 9
                |> addDirt 3 9
                |> addDirt 4 9
                |> addDirt 5 9
                |> addDirt 6 9
                |> addDirt 6 9
                |> addDirt 7 9
                |> addDirt 8 9
                |> addDirt 9 9
                |> addDirt 10 9
                |> addDirt 11 9
                |> addDirt 12 9
                |> addDirt 13 9
                |> addDirt 14 9
                |> addDirt 15 9
                |> addDirt 15 8
                --                |> addEnemy 5 8
                --                |> addEnemy 9 8
                |> addWall 0 11
                |> addWall 1 11
                |> addWall 2 11
                |> addWall 3 11
                |> addWall 4 11
                |> addWall 5 11
                |> addWall 6 11
                |> addWall 7 11
                |> addWall 8 11
                |> addWall 9 11
                |> addWall 10 11
                |> addWall 11 11
    in
        { level = level
        , width = width
        , height = height
        , keys =
            { left = NotPressed
            , right = NotPressed
            , up = NotPressed
            , down = NotPressed
            }
        , debug = True
        , gameSpeed = Nothing
        , currentTick = 0
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

        GameSpeed gameSpeed ->
            { model | gameSpeed = gameSpeed } ! []

        GameTick _ ->
            let
                currentTick =
                    model.currentTick + 1

                level =
                    model.level

                actors =
                    level.actors

                keys =
                    model.keys

                maybeInputForce =
                    calculateInputForce keys

                newLevel =
                    List.foldr
                        (\( actorId, actor ) level ->
                            let
                                ( _, updatedLevel ) =
                                    List.foldr
                                        (\( key, component ) ( actor, level ) ->
                                            let
                                                updatedLevel =
                                                    case component of
                                                        PlayerInputComponent ->
                                                            maybeInputForce
                                                                |> Maybe.andThen (\direction -> applyForce currentTick level actor direction)
                                                                |> Maybe.withDefault level

                                                        TransformComponent transformData ->
                                                            case transformData.movingState of
                                                                MovingTowards movingData ->
                                                                    handleMovingTowards currentTick transformData movingData actor
                                                                        |> updateActor level actor.id

                                                                _ ->
                                                                    level

                                                        DiamondCollectorComponent ->
                                                            tryToCollectDiamond level actor

                                                        CanSquashComponent ->
                                                            trySquashingThings level actor

                                                        PhysicsComponent physics ->
                                                            tryApplyPhysics currentTick level actor physics

                                                        AIComponent ai ->
                                                            tryApplyAI currentTick level actor ai

                                                        CameraComponent camera ->
                                                            tryMoveCamera level actor camera

                                                        _ ->
                                                            level
                                            in
                                                ( actor, updatedLevel )
                                        )
                                        ( actor, level )
                                        (Dict.toList actor.components)
                            in
                                updatedLevel
                        )
                        level
                        (Dict.toList actors)

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
                    , currentTick = currentTick
                }
                    ! []

        NoOp ->
            model ! []


applyForce : Tick -> Level -> Actor -> Direction -> Maybe Level
applyForce currentTick level actor direction =
    getTransformComponent actor.components
        |> Maybe.andThen
            -- Can only apply force if not already moving
            (\transformData ->
                case transformData.movingState of
                    NotMoving ->
                        Just transformData

                    _ ->
                        Nothing
            )
        |> Maybe.andThen
            -- Can only move if that spot is not already taken
            (\transformData ->
                let
                    offset =
                        getOffsetFromDirection direction

                    newPosition =
                        addPositions transformData.position offset
                in
                    case getActorWhoClaimed level.actors newPosition of
                        Nothing ->
                            Just ( transformData, newPosition, level )

                        Just otherActor ->
                            if hasRigidComponent actor.components then
                                tryToPush currentTick level actor transformData otherActor direction
                            else
                                Just ( transformData, newPosition, level )
            )
        |> Maybe.andThen
            (\( transformData, newPosition, level ) ->
                Just <| handleMovement currentTick level actor transformData newPosition
            )


tryToPush : Tick -> Level -> Actor -> TransformComponentData -> Actor -> Direction -> Maybe ( TransformComponentData, Position, Level )
tryToPush currentTick level actor transformData otherActor direction =
    -- Can not push something that is already moving
    getTransformComponent otherActor.components
        |> Maybe.andThen
            (\otherTransformData ->
                case otherTransformData.movingState of
                    NotMoving ->
                        Just otherTransformData

                    _ ->
                        Nothing
            )
        |> Maybe.andThen
            (\otherTransformData ->
                let
                    pushedToPosition =
                        addPositions otherTransformData.position (getOffsetFromDirection direction)
                in
                    if isEmpty level pushedToPosition then
                        let
                            updatedLevel =
                                handleMovement
                                    currentTick
                                    level
                                    otherActor
                                    otherTransformData
                                    pushedToPosition
                        in
                            Just ( transformData, otherTransformData.position, updatedLevel )
                    else
                        Nothing
            )


handleMovement : Tick -> Level -> Actor -> TransformComponentData -> Position -> Level
handleMovement currentTick level actor transformData newPosition =
    let
        newComponents =
            Dict.insert
                "transform"
                (TransformComponent
                    { transformData
                        | movingState =
                            MovingTowards
                                { position = newPosition
                                , startTick = currentTick
                                , endTick = currentTick + movingTicks
                                , completionPercentage = 0.0
                                }
                    }
                )
                actor.components

        newActors =
            Dict.insert
                actor.id
                { actor | components = newComponents }
                level.actors
    in
        { level | actors = newActors }


hasRigidComponent : Dict String Component -> Bool
hasRigidComponent =
    Dict.member "rigid"


getActorWhoClaimed : Dict Int Actor -> Position -> Maybe Actor
getActorWhoClaimed actors position =
    Dict.Extra.find
        (\actorId actor ->
            getTransformComponent actor.components
                |> Maybe.andThen
                    (\transformData ->
                        if transformData.position == position then
                            Just actor
                        else
                            case transformData.movingState of
                                MovingTowards towardsData ->
                                    if towardsData.position == position then
                                        Just actor
                                    else
                                        Nothing

                                _ ->
                                    Nothing
                    )
                |> Maybe.Extra.isJust
        )
        actors
        |> Maybe.andThen
            (\( actorId, actor ) ->
                Just actor
            )


getDirectionFromID : Int -> Direction
getDirectionFromID id =
    case id % 4 of
        0 ->
            Left

        1 ->
            Up

        2 ->
            Right

        _ ->
            Down


getIDFromDirection : Direction -> Int
getIDFromDirection direction =
    case direction of
        Left ->
            0

        Up ->
            1

        Right ->
            2

        Down ->
            3


getOffsetFromDirection : Direction -> Position
getOffsetFromDirection direction =
    case direction of
        Left ->
            { x = -1, y = 0 }

        Up ->
            { x = 0, y = -1 }

        Right ->
            { x = 1, y = 0 }

        Down ->
            { x = 0, y = 1 }


subtractPositions : Position -> Position -> Position
subtractPositions =
    calculatePosition (-)


addPositions : Position -> Position -> Position
addPositions =
    calculatePosition (+)


calculatePosition : (Int -> Int -> Int) -> Position -> Position -> Position
calculatePosition method pos1 pos2 =
    { x = method pos1.x pos2.x
    , y = method pos1.y pos2.y
    }


calculateInputForce : Keys -> Maybe Direction
calculateInputForce keys =
    if isMoving keys.left then
        Just Left
    else if isMoving keys.up then
        Just Up
    else if isMoving keys.right then
        Just Right
    else if isMoving keys.down then
        Just Down
    else
        Nothing


tryApplyAI : Tick -> Level -> Actor -> AIComponentData -> Level
tryApplyAI currentTick level actor ai =
    getTransformComponent actor.components
        |> Maybe.andThen
            (\transformData ->
                case transformData.movingState of
                    NotMoving ->
                        Just transformData

                    _ ->
                        -- Can not update ai when already moving
                        Nothing
            )
        |> Maybe.andThen
            (\transformData ->
                let
                    previousDirectionId =
                        getIDFromDirection ai.previousDirection
                in
                    -- Find next direction
                    List.Extra.find
                        (\( transformData, direction ) ->
                            -- Check if that position is free
                            let
                                position =
                                    addPositions transformData.position (getOffsetFromDirection direction)
                            in
                                isEmpty level position
                        )
                        [ ( transformData, getDirectionFromID <| previousDirectionId - 3 )
                        , ( transformData, getDirectionFromID <| previousDirectionId - 4 )
                        , ( transformData, getDirectionFromID <| previousDirectionId - 5 )
                        , ( transformData, getDirectionFromID <| previousDirectionId - 6 )
                        ]
            )
        |> Maybe.andThen
            (\( transformData, direction ) ->
                let
                    offset =
                        getOffsetFromDirection direction

                    newPosition =
                        addPositions transformData.position offset

                    newComponents =
                        Dict.insert
                            "ai"
                            (AIComponent
                                { previousDirection = direction
                                }
                            )
                            actor.components

                    newActor =
                        { actor | components = newComponents }
                in
                    Just <| handleMovement currentTick level newActor transformData newPosition
            )
        |> Maybe.withDefault level


tryMoveCamera : Level -> Actor -> CameraComponentData -> Level
tryMoveCamera level actor camera =
    getTransformComponent actor.components
        |> Maybe.andThen
            (\transformData ->
                let
                    view =
                        level.view

                    viewPosition =
                        view.position

                    position =
                        transformData.position

                    x =
                        if position.x - camera.borderSize <= viewPosition.x then
                            position.x - camera.borderSize
                        else if position.x - view.width + camera.borderSize > viewPosition.x - 1 then
                            position.x - view.width + camera.borderSize + 1
                        else
                            viewPosition.x

                    y =
                        if position.y - camera.borderSize <= viewPosition.y then
                            position.y - camera.borderSize
                        else if position.y - view.height + camera.borderSize >= viewPosition.y - 1 then
                            position.y - view.height + camera.borderSize + 1
                        else
                            viewPosition.y

                    newViewPosition =
                        { viewPosition
                            | x = x
                            , y = y
                        }

                    newView =
                        { view | position = newViewPosition }
                in
                    Just { level | view = newView }
            )
        |> Maybe.withDefault level


tryApplyPhysics : Tick -> Level -> Actor -> PhysicsComponentData -> Level
tryApplyPhysics currentTick level actor physics =
    if physics.affectedByGravity then
        getTransformComponent actor.components
            |> Maybe.andThen
                (\transformData ->
                    case transformData.movingState of
                        NotMoving ->
                            Just transformData

                        _ ->
                            -- Can not update physics when already moving
                            Nothing
                )
            |> Maybe.andThen
                (\transformData ->
                    -- Get the actor below us
                    let
                        offset =
                            getOffsetFromDirection Down

                        belowPosition =
                            addPositions transformData.position offset
                    in
                        case getActorWhoClaimed level.actors belowPosition of
                            Nothing ->
                                -- Do movement
                                Just ( transformData, belowPosition )

                            Just belowActor ->
                                -- Checking if actor can roll over
                                case physics.shape of
                                    Circle ->
                                        case getPhysicsComponent belowActor.components of
                                            Just belowPhysics ->
                                                case belowPhysics.shape of
                                                    Circle ->
                                                        if canRollLeft level transformData then
                                                            let
                                                                offset =
                                                                    getOffsetFromDirection Left

                                                                leftPosition =
                                                                    addPositions transformData.position offset
                                                            in
                                                                Just ( transformData, leftPosition )
                                                        else if canRollRight level transformData then
                                                            let
                                                                offset =
                                                                    getOffsetFromDirection Right

                                                                rightPosition =
                                                                    addPositions transformData.position offset
                                                            in
                                                                Just ( transformData, rightPosition )
                                                        else
                                                            Nothing

                                                    _ ->
                                                        Nothing

                                            _ ->
                                                Nothing

                                    _ ->
                                        Nothing
                )
            |> Maybe.andThen
                (\( transformData, newPosition ) ->
                    Just <| handleMovement currentTick level actor transformData newPosition
                )
            |> Maybe.withDefault level
    else
        level


canRollLeft : Level -> TransformComponentData -> Bool
canRollLeft =
    canRoll Left


canRollRight : Level -> TransformComponentData -> Bool
canRollRight =
    canRoll Right


canRoll : Direction -> Level -> TransformComponentData -> Bool
canRoll direction level transformData =
    let
        sidePosition =
            addPositions transformData.position <| getOffsetFromDirection direction

        sideBottomPosition =
            addPositions sidePosition <| getOffsetFromDirection Down
    in
        isEmpty level sidePosition && isEmpty level sideBottomPosition


isEmpty : Level -> Position -> Bool
isEmpty level position =
    getActorWhoClaimed level.actors position
        |> Maybe.Extra.isNothing


tryToCollectDiamond : Level -> Actor -> Level
tryToCollectDiamond level focusedActor =
    case getTransformComponent focusedActor.components of
        Just focusedTransformData ->
            Dict.foldr
                (\actorId actor level ->
                    if Dict.member "diamond" actor.components then
                        case getTransformComponent actor.components of
                            Just diamondTransformData ->
                                if diamondTransformData.position == focusedTransformData.position then
                                    let
                                        newActors =
                                            Dict.remove actorId level.actors

                                        diamonds =
                                            level.diamonds

                                        newDiamonds =
                                            { diamonds | collected = diamonds.collected + 1 }
                                    in
                                        { level
                                            | actors = newActors
                                            , diamonds = newDiamonds
                                        }
                                else
                                    level

                            _ ->
                                level
                    else
                        level
                )
                level
                level.actors

        Nothing ->
            level


trySquashingThings : Level -> Actor -> Level
trySquashingThings level focusedActor =
    case getTransformComponent focusedActor.components of
        Just focusedTransformData ->
            Dict.foldr
                (\actorId actor level ->
                    if Dict.member "squashable" actor.components then
                        case getTransformComponent actor.components of
                            Just squashableTransformData ->
                                if squashableTransformData.position == focusedTransformData.position then
                                    let
                                        newActors =
                                            Dict.remove actorId level.actors
                                    in
                                        { level | actors = newActors }
                                else
                                    level

                            _ ->
                                level
                    else
                        level
                )
                level
                level.actors

        Nothing ->
            level


updateActor : Level -> Int -> Actor -> Level
updateActor level actorId actor =
    let
        newActors =
            Dict.insert
                actorId
                actor
                level.actors
    in
        { level | actors = newActors }


handleMovingTowards : Tick -> TransformComponentData -> MovingTowardsData -> Actor -> Actor
handleMovingTowards currentTick transformData towardsData actor =
    if currentTick >= towardsData.endTick then
        let
            newComponents =
                Dict.insert
                    "transform"
                    (TransformComponent
                        { position = towardsData.position
                        , movingState = NotMoving
                        }
                    )
                    actor.components
        in
            { actor | components = newComponents }
    else
        let
            newComponents =
                Dict.insert
                    "transform"
                    (TransformComponent
                        { transformData
                            | movingState =
                                MovingTowards
                                    { towardsData
                                        | completionPercentage = calculateCompletionPercentage towardsData.startTick towardsData.endTick currentTick
                                    }
                        }
                    )
                    actor.components
        in
            { actor | components = newComponents }


calculateCompletionPercentage : Tick -> Tick -> Tick -> Float
calculateCompletionPercentage startTick endTick currentTick =
    100 / (toFloat (endTick - startTick)) * (toFloat (currentTick - startTick))


isMoving : KeyStatus -> Bool
isMoving status =
    case status of
        NotPressed ->
            False

        _ ->
            True


view : Model -> Html Msg
view model =
    let
        view =
            model.level.view
    in
        div
            []
            [ Canvas.initialize (Canvas.Size (model.width * pixelSize) (model.height * pixelSize))
                |> Canvas.batch
                    (List.range view.position.y (view.position.y + view.height - 1)
                        |> List.map
                            (\y ->
                                List.range view.position.x (view.position.x + view.height - 1)
                                    |> List.map
                                        (\x ->
                                            getPixel view.position { x = x, y = y } model.level.actors
                                                |> Maybe.withDefault []
                                        )
                                    |> List.concat
                            )
                        |> List.concat
                    )
                |> Canvas.toHtml []
            , debugView model
            ]


debugView : Model -> Html Msg
debugView model =
    if model.debug then
        div
            []
            [ text "GameTick speed:"
            , div
                []
                [ button [ onClick <| GameSpeed Nothing ] [ text "Off" ]
                , button [ onClick <| GameSpeed (Just <| 10 * Time.second) ] [ text "0.1 fps" ]
                , button [ onClick <| GameSpeed (Just <| 5 * Time.second) ] [ text "0.5 fps" ]
                , button [ onClick <| GameSpeed (Just <| 1 * Time.second) ] [ text "1 fps" ]
                , button [ onClick <| GameSpeed (Just <| 80 * Time.millisecond) ] [ text "12 fps" ]
                , button [ onClick <| GameSpeed (Just <| 40 * Time.millisecond) ] [ text "24 fps" ]
                , button [ onClick <| GameSpeed (Just <| 16 * Time.millisecond) ] [ text "60 fps" ]
                ]
            ]
    else
        text ""


getPixel : Position -> Position -> Dict Int Actor -> Maybe (List Canvas.DrawOp)
getPixel viewPosition position actors =
    Dict.foldr
        (\actorId actor acc ->
            (getTransformRenderComponent actor.components
                |> Maybe.andThen
                    (\renderData ->
                        getTransformComponent actor.components
                            |> Maybe.andThen
                                (\transformData ->
                                    if transformData.position == position then
                                        case transformData.movingState of
                                            MovingTowards towardsData ->
                                                Just <| calculateColor renderData.color (100.0 - towardsData.completionPercentage)

                                            _ ->
                                                Just renderData.color
                                    else
                                        case transformData.movingState of
                                            MovingTowards towardsData ->
                                                if towardsData.position == position then
                                                    Just <| calculateColor renderData.color towardsData.completionPercentage
                                                else
                                                    Nothing

                                            _ ->
                                                Nothing
                                )
                    )
            )
                :: (getAdditionalPositionRenderComponent actor.components
                        |> Maybe.andThen
                            (\renderData ->
                                List.map
                                    (\{ offset, color } ->
                                        getTransformComponent actor.components
                                            |> Maybe.andThen
                                                (\transformData ->
                                                    if addPositions transformData.position offset == position then
                                                        Just color
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
        |> List.foldr
            (\color acc ->
                case acc of
                    Nothing ->
                        Just color

                    Just accColor ->
                        Just <| combineColors color accColor
            )
            Nothing
        |> Maybe.andThen
            (\color ->
                Just <| asPixel viewPosition position color
            )


combineColors : Color -> Color -> Color
combineColors color1 color2 =
    let
        rgba1 =
            Color.toRgb color1

        rgba2 =
            Color.toRgb color2

        combinedRgba =
            { red = round <| (toFloat (rgba1.red + rgba2.red)) / 2
            , green = round <| (toFloat (rgba1.green + rgba2.green)) / 2
            , blue = round <| (toFloat (rgba1.blue + rgba2.blue)) / 2
            , alpha = (rgba1.alpha + rgba2.alpha) / 2
            }
    in
        Color.rgba combinedRgba.red combinedRgba.green combinedRgba.blue combinedRgba.alpha


calculateColor : Color -> Float -> Color
calculateColor color percentage =
    let
        rgba =
            Color.toRgb color

        newRgba =
            { rgba | alpha = (percentage / 100) }
    in
        Color.rgba newRgba.red newRgba.green newRgba.blue newRgba.alpha


asPixel : Position -> Position -> Color -> List Canvas.DrawOp
asPixel viewPosition position color =
    [ Canvas.FillStyle color
    , Canvas.FillRect
        (Canvas.Point.fromInts ( (position.x - viewPosition.x) * pixelSize, (position.y - viewPosition.y) * pixelSize ))
        (Canvas.Size pixelSize pixelSize)
    ]


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


getPhysicsComponent : Dict String Component -> Maybe PhysicsComponentData
getPhysicsComponent components =
    Dict.get "physics" components
        |> Maybe.andThen
            (\component ->
                case component of
                    PhysicsComponent data ->
                        Just data

                    _ ->
                        Nothing
            )


getTransformRenderComponent : Dict String Component -> Maybe TransformRenderComponentData
getTransformRenderComponent components =
    Dict.get "render" components
        |> Maybe.andThen
            (\component ->
                case component of
                    TransformRenderComponent data ->
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


addPlayer : Int -> Int -> Int -> Level -> Level
addPlayer x y borderSize level =
    let
        actors =
            Dict.insert
                level.nextActorId
                (createPlayer level.nextActorId x y borderSize)
                level.actors
    in
        { level | actors = actors, nextActorId = level.nextActorId + 1 }


createPlayer : Int -> Int -> Int -> Int -> Actor
createPlayer id x y borderSize =
    { id = id
    , components =
        Dict.fromList
            [ ( "transform", TransformComponent { position = { x = x, y = y }, movingState = NotMoving } )
            , ( "render", TransformRenderComponent { color = Color.darkGreen } )
            , ( "player-input", PlayerInputComponent )
            , ( "diamond-collector", DiamondCollectorComponent )
            , ( "can-squash", CanSquashComponent )
            , ( "rigid", RigidComponent )
            , ( "physics"
              , PhysicsComponent
                    { mass = 10
                    , shape = Square
                    , affectedByGravity = False
                    }
              )
            , ( "camera", CameraComponent { borderSize = borderSize } )
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
            [ ( "transform", TransformComponent { position = { x = x, y = y }, movingState = NotMoving } )
            , ( "render", TransformRenderComponent { color = Color.darkGray } )
            , ( "rigid", RigidComponent )
            , ( "physics"
              , PhysicsComponent
                    { mass = 20
                    , shape = Circle
                    , affectedByGravity = True
                    }
              )
            ]
    }


addEnemy : Int -> Int -> Level -> Level
addEnemy x y level =
    let
        actors =
            Dict.insert
                level.nextActorId
                (createEnemy level.nextActorId x y)
                level.actors
    in
        { level | actors = actors, nextActorId = level.nextActorId + 1 }


createEnemy : Int -> Int -> Int -> Actor
createEnemy id x y =
    { id = id
    , components =
        Dict.fromList
            [ ( "transform", TransformComponent { position = { x = x, y = y }, movingState = NotMoving } )
            , ( "render", TransformRenderComponent { color = Color.darkOrange } )
            , ( "rigid", RigidComponent )
            , ( "physics"
              , PhysicsComponent
                    { mass = 20
                    , shape = Circle
                    , affectedByGravity = False
                    }
              )
            , ( "ai"
              , AIComponent
                    { previousDirection = Right }
              )
            ]
    }


addDirt : Int -> Int -> Level -> Level
addDirt x y level =
    let
        actors =
            Dict.insert
                level.nextActorId
                (createDirt level.nextActorId x y)
                level.actors
    in
        { level | actors = actors, nextActorId = level.nextActorId + 1 }


createDirt : Int -> Int -> Int -> Actor
createDirt id x y =
    { id = id
    , components =
        Dict.fromList
            [ ( "transform", TransformComponent { position = { x = x, y = y }, movingState = NotMoving } )
            , ( "render", TransformRenderComponent { color = Color.lightBrown } )
            , ( "squashable", SquashableComponent )
            , ( "physics"
              , PhysicsComponent
                    { mass = 1
                    , shape = Square
                    , affectedByGravity = False
                    }
              )
            ]
    }


addWall : Int -> Int -> Level -> Level
addWall x y level =
    let
        actors =
            Dict.insert
                level.nextActorId
                (createWall level.nextActorId x y)
                level.actors
    in
        { level | actors = actors, nextActorId = level.nextActorId + 1 }


createWall : Int -> Int -> Int -> Actor
createWall id x y =
    { id = id
    , components =
        Dict.fromList
            [ ( "transform", TransformComponent { position = { x = x, y = y }, movingState = NotMoving } )
            , ( "render", TransformRenderComponent { color = Color.black } )
            , ( "rigid", RigidComponent )
            , ( "physics"
              , PhysicsComponent
                    { mass = 100
                    , shape = Square
                    , affectedByGravity = False
                    }
              )
            ]
    }


addDiamond : Int -> Int -> Level -> Level
addDiamond x y level =
    let
        diamonds =
            level.diamonds

        newDiamonds =
            { diamonds | total = diamonds.total + 1 }

        actors =
            Dict.insert
                level.nextActorId
                (createDiamond level.nextActorId x y)
                level.actors
    in
        { level
            | actors = actors
            , nextActorId = level.nextActorId + 1
            , diamonds = newDiamonds
        }


createDiamond : Int -> Int -> Int -> Actor
createDiamond id x y =
    { id = id
    , components =
        Dict.fromList
            [ ( "transform", TransformComponent { position = { x = x, y = y }, movingState = NotMoving } )
            , ( "render", TransformRenderComponent { color = Color.blue } )
            , ( "diamond", DiamondComponent )
            , ( "physics"
              , PhysicsComponent
                    { mass = 100
                    , shape = Circle
                    , affectedByGravity = True
                    }
              )
            ]
    }


empty : Html Msg
empty =
    text "[ ]"


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        sub =
            [ Keyboard.presses KeyPressed
            , Keyboard.downs KeyDown
            , Keyboard.ups KeyUp
            ]

        newSub =
            case model.gameSpeed of
                Just delay ->
                    Time.every delay GameTick :: sub

                Nothing ->
                    sub
    in
        Sub.batch newSub


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
