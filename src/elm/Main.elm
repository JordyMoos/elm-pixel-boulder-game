module Main exposing (main)

import Html exposing (Html, text, br, div, button)
import Html.Events exposing (onClick)
import Keyboard
import Time
import Char
import List.Extra
import Dict exposing (Dict)
import Maybe.Extra
import Color exposing (Color)
import Canvas
import Canvas.Point
import Data.Common exposing (Tick, Position)
import InputController
import UpdateLoop


pixelSize : Int
pixelSize =
    30


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
                                            addStrongWall x y level

                                        '|' ->
                                            addWall x y level

                                        '.' ->
                                            addDirt x y level

                                        'P' ->
                                            addPlayer x y defaultCameraBorderSize level

                                        'O' ->
                                            addRock x y level

                                        '0' ->
                                            addRock x y level

                                        '*' ->
                                            addDiamond x y level

                                        'E' ->
                                            addEnemy x y level

                                        '=' ->
                                            addExplosive x y level

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
        , gameSpeed = Nothing -- Just <| 40 * Time.millisecond
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
                maybeInputDirection =
                    InputController.getCurrentDirection
            in
                { model
                    | inputController = InputController.resetWasPressed model.inputController
                    , level = UpdateLoop.update maybeInputDirection model.level
                    , currentTick = model.currentTick + 1
                }
                    ! []


handleDamageComponent : Actor -> DamageComponentData -> Level -> Level
handleDamageComponent actor damageData level =
    level
        |> tryDoDamage actor
        |> removeExplosionIfEnded actor damageData


removeExplosionIfEnded : Actor -> DamageComponentData -> Level -> Level
removeExplosionIfEnded actor damageData level =
    if damageData.remainingTicks > 0 then
        let
            updatedComponents =
                Dict.insert
                    "damage"
                    (DamageComponent { damageData | remainingTicks = damageData.remainingTicks - 1 })
                    actor.components
        in
            updateActor
                level
                actor.id
                { actor | components = updatedComponents }
    else
        removeActor actor.id level


tryDoDamage : Actor -> Level -> Level
tryDoDamage damageDealingActor level =
    getTransformComponent damageDealingActor.components
        |> Maybe.andThen
            (\transformData ->
                List.foldr
                    (\actor level ->
                        if damageDealingActor.id == actor.id then
                            level
                        else
                            -- @todo not everything should be able to be destroyed
                            removeActor actor.id level
                    )
                    level
                    (getActorsThatAffect transformData.position level)
                    |> Just
            )
        |> Maybe.withDefault level


removeActor : Int -> Level -> Level
removeActor actorId level =
    let
        remainingActors =
            Dict.remove
                actorId
                level.actors
    in
        { level | actors = remainingActors }


isEmpty : Level -> Position -> Bool
isEmpty level position =
    getActorWhoClaimed position level
        |> Maybe.Extra.isNothing



-- @todo can remove actorId, it is already in actor


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


removeActorIdFromPosition : Position -> ActorId -> Level -> Level
removeActorIdFromPosition { x, y } actorIdToRemove level =
    let
        newPositionIndex =
            Dict.update
                ( x, y )
                (\maybeActorIds ->
                    Maybe.withDefault [] maybeActorIds
                        |> List.filter
                            (\actorId ->
                                not <| actorId == actorIdToRemove
                            )
                        |> Just
                )
                level.positionIndex
    in
        { level | positionIndex = newPositionIndex }


addActorIdToPosition : Position -> ActorId -> Level -> Level
addActorIdToPosition { x, y } actorIdToAdd level =
    let
        newPositionIndex =
            Dict.update
                ( x, y )
                (\maybeActorIds ->
                    Maybe.withDefault [] maybeActorIds
                        |> (::) actorIdToAdd
                        |> Just
                )
                level.positionIndex
    in
        { level | positionIndex = newPositionIndex }


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
                                            getPixel model.currentTick view.position { x = x, y = y } model.level
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
                , button [ onClick <| GameSpeed (Just <| 16 * Time.millisecond) ] [ text "60 fps" ]
                ]
            ]
    else
        text ""


getPixel : Tick -> Position -> Position -> Level -> Maybe (List Canvas.DrawOp)
getPixel tick viewPosition position level =
    getActorsThatAffect position level
        |> List.foldr
            (\actor acc ->
                (getTransformRenderComponent actor.components
                    |> Maybe.andThen
                        (\renderData ->
                            getTransformComponent actor.components
                                |> Maybe.andThen
                                    (\transformData ->
                                        if transformData.position == position then
                                            case transformData.movingState of
                                                MovingTowards towardsData ->
                                                    Just <| calculateColor (getColor tick renderData) (100.0 - towardsData.completionPercentage)

                                                _ ->
                                                    Just (getColor tick renderData)
                                        else
                                            case transformData.movingState of
                                                MovingTowards towardsData ->
                                                    if towardsData.position == position then
                                                        Just <| calculateColor (getColor tick renderData) towardsData.completionPercentage
                                                    else
                                                        Nothing

                                                _ ->
                                                    Nothing
                                    )
                        )
                )
                    :: acc
            )
            []
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


getColor : Tick -> TransformRenderComponentData -> Color
getColor tick renderData =
    round ((toFloat tick) / (toFloat (max renderData.ticksPerColor 1)))
        % (max 1 <| List.length renderData.colors)
        |> (flip List.Extra.getAt) renderData.colors
        |> Maybe.withDefault noColor


noColor : Color
noColor =
    Color.white


combineColors : Color -> Color -> Color
combineColors color1 color2 =
    let
        rgba1 =
            Color.toRgb color1

        rgba2 =
            Color.toRgb color2

        intAlpha1 =
            round (rgba1.alpha * 100.0)

        intAlpha2 =
            round (rgba2.alpha * 100.0)

        combinedRgba =
            { red = round <| (toFloat ((rgba1.red * intAlpha1) + (rgba2.red * intAlpha2))) / (toFloat (max 1 (intAlpha1 + intAlpha2)))
            , green = round <| (toFloat ((rgba1.green * intAlpha1) + (rgba2.green * intAlpha2))) / (toFloat (max 1 (intAlpha1 + intAlpha2)))
            , blue = round <| (toFloat ((rgba1.blue * intAlpha1) + (rgba2.blue * intAlpha2))) / (toFloat (max 1 (intAlpha1 + intAlpha2)))
            , alpha = (max rgba1.alpha rgba2.alpha)
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


addPlayer : Int -> Int -> Int -> Level -> Level
addPlayer x y borderSize level =
    let
        actors =
            Dict.insert
                level.nextActorId
                (createPlayer level.nextActorId x y borderSize)
                level.actors

        view =
            level.view

        -- Cheats @todo if the camera component is too far away at the start. Then the view wont be moved
        -- Because the component will not get an update
        updatedView =
            { view | position = { x = x - borderSize, y = y - borderSize } }
    in
        { level
            | actors = actors
            , positionIndex = addActorIdToPositionIndex ( x, y ) level.nextActorId level.positionIndex
            , nextActorId = level.nextActorId + 1
            , view = updatedView
        }


createPlayer : Int -> Int -> Int -> Int -> Actor
createPlayer id x y borderSize =
    { id = id
    , components =
        Dict.fromList
            [ ( "transform", TransformComponent { position = { x = x, y = y }, movingState = NotMoving } )
            , ( "render", TransformRenderComponent { colors = [ Color.darkGreen ], ticksPerColor = 1 } )
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
            , ( "explodable", ExplodableComponent )
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
        { level
            | actors = actors
            , positionIndex = addActorIdToPositionIndex ( x, y ) level.nextActorId level.positionIndex
            , nextActorId = level.nextActorId + 1
        }


createRock : Int -> Int -> Int -> Actor
createRock id x y =
    { id = id
    , components =
        Dict.fromList
            [ ( "transform", TransformComponent { position = { x = x, y = y }, movingState = NotMoving } )
            , ( "render", TransformRenderComponent { colors = [ Color.darkGray ], ticksPerColor = 1 } )
            , ( "rigid", RigidComponent )
            , ( "physics"
              , PhysicsComponent
                    { mass = 20
                    , shape = Circle
                    , affectedByGravity = True
                    }
              )
            , ( "downsmash", DownSmashComponent { wasMovingDown = False } )
            ]
    }


addExplosive : Int -> Int -> Level -> Level
addExplosive x y level =
    let
        actors =
            Dict.insert
                level.nextActorId
                (createExplosive level.nextActorId x y)
                level.actors
    in
        { level
            | actors = actors
            , positionIndex = addActorIdToPositionIndex ( x, y ) level.nextActorId level.positionIndex
            , nextActorId = level.nextActorId + 1
        }


createExplosive : Int -> Int -> Int -> Actor
createExplosive id x y =
    { id = id
    , components =
        Dict.fromList
            [ ( "transform", TransformComponent { position = { x = x, y = y }, movingState = NotMoving } )
            , ( "render", TransformRenderComponent { colors = [ Color.red ], ticksPerColor = 1 } )
            , ( "rigid", RigidComponent )
            , ( "explodable", ExplodableComponent )
            , ( "physics"
              , PhysicsComponent
                    { mass = 10
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
        { level
            | actors = actors
            , positionIndex = addActorIdToPositionIndex ( x, y ) level.nextActorId level.positionIndex
            , nextActorId = level.nextActorId + 1
        }


createEnemy : Int -> Int -> Int -> Actor
createEnemy id x y =
    { id = id
    , components =
        Dict.fromList
            [ ( "transform", TransformComponent { position = { x = x, y = y }, movingState = NotMoving } )
            , ( "render", TransformRenderComponent { colors = [ Color.darkOrange ], ticksPerColor = 1 } )
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
            , ( "explodable", ExplodableComponent )
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
        { level
            | actors = actors
            , positionIndex = addActorIdToPositionIndex ( x, y ) level.nextActorId level.positionIndex
            , nextActorId = level.nextActorId + 1
        }


createDirt : Int -> Int -> Int -> Actor
createDirt id x y =
    { id = id
    , components =
        Dict.fromList
            [ ( "transform", TransformComponent { position = { x = x, y = y }, movingState = NotMoving } )
            , ( "render", TransformRenderComponent { colors = [ Color.lightBrown ], ticksPerColor = 1 } )
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


addExplosion : Int -> Int -> Level -> Level
addExplosion x y level =
    let
        actors =
            Dict.insert
                level.nextActorId
                (createExplosion level.nextActorId x y)
                level.actors
    in
        { level
            | actors = actors
            , positionIndex = addActorIdToPositionIndex ( x, y ) level.nextActorId level.positionIndex
            , nextActorId = level.nextActorId + 1
        }


createExplosion : Int -> Int -> Int -> Actor
createExplosion id x y =
    { id = id
    , components =
        Dict.fromList
            [ ( "transform", TransformComponent { position = { x = x, y = y }, movingState = NotMoving } )
            , ( "render", TransformRenderComponent { colors = [ Color.red, Color.darkOrange, Color.yellow ], ticksPerColor = 2 } )
            , ( "damage", DamageComponent { remainingTicks = 8 } )
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
        { level
            | actors = actors
            , positionIndex = addActorIdToPositionIndex ( x, y ) level.nextActorId level.positionIndex
            , nextActorId = level.nextActorId + 1
        }


createWall : Int -> Int -> Int -> Actor
createWall id x y =
    { id = id
    , components =
        Dict.fromList
            [ ( "transform", TransformComponent { position = { x = x, y = y }, movingState = NotMoving } )
            , ( "render", TransformRenderComponent { colors = [ Color.rgb 98 100 87 ], ticksPerColor = 1 } )
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


addStrongWall : Int -> Int -> Level -> Level
addStrongWall x y level =
    let
        actors =
            Dict.insert
                level.nextActorId
                (createStrongWall level.nextActorId x y)
                level.actors
    in
        { level
            | actors = actors
            , positionIndex = addActorIdToPositionIndex ( x, y ) level.nextActorId level.positionIndex
            , nextActorId = level.nextActorId + 1
        }


createStrongWall : Int -> Int -> Int -> Actor
createStrongWall id x y =
    { id = id
    , components =
        Dict.fromList
            [ ( "transform", TransformComponent { position = { x = x, y = y }, movingState = NotMoving } )
            , ( "render", TransformRenderComponent { colors = [ Color.black ], ticksPerColor = 1 } )
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
            , positionIndex = addActorIdToPositionIndex ( x, y ) level.nextActorId level.positionIndex
            , nextActorId = level.nextActorId + 1
            , diamonds = newDiamonds
        }


createDiamond : Int -> Int -> Int -> Actor
createDiamond id x y =
    { id = id
    , components =
        Dict.fromList
            [ ( "transform", TransformComponent { position = { x = x, y = y }, movingState = NotMoving } )
            , ( "render", TransformRenderComponent { colors = [ Color.blue, Color.lightBlue ], ticksPerColor = 12 } )
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
