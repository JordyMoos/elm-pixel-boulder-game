module Actor
    exposing
        ( ActorId
        , Actor
        , Level
          -- Actor
        , getActorById
        , getActorsByPosition
        , getActorIdsByPosition
        , getActorIdsByXY
          -- Components
        , Component(..)
          -- Updates
        , updatePlayerInputComponent
        , updateDiamondCollectorComponent
        , updateCanSquashComponent
        , updatePhysicsComponent
        , updateAiComponent
        , updateCameraComponent
        , updateTransformComponent
        , updateDownSmashComponent
        , updateDamageComponent
          -- Actor creation
        , addStrongWall
        , addWall
        , addDirt
        , addPlayer
        , addRock
        , addDiamond
        , addEnemy
        , addExplosive
        )

import Dict exposing (Dict)
import Data.Common exposing (Position, Direction, Tick)
import Color exposing (Color)
import Maybe.Extra
import List.Extra


type alias ActorId =
    Int


type alias Components =
    Dict String Component


type alias Actor =
    { id : ActorId
    , components : Components
    }


type alias Actors =
    Dict ActorId Actor


type alias View =
    { position : Position
    , width : Int
    , height : Int
    }


type alias Diamonds =
    { total : Int
    , collected : Int
    }


type alias PositionIndex =
    Dict ( Int, Int ) (List ActorId)


type alias Level =
    { actors : Actors
    , positionIndex : PositionIndex
    , nextActorId : Int
    , diamonds : Diamonds
    , view : View
    }


type Component
    = TransformComponent TransformComponentData
    | RenderComponent RenderComponentData
    | PlayerInputComponent
    | DiamondCollectorComponent
    | DiamondComponent
    | SquashableComponent
    | CanSquashComponent
    | PhysicsComponent PhysicsComponentData
    | RigidComponent
    | AiComponent AiComponentData
    | CameraComponent CameraComponentData
    | ExplodableComponent
    | DownSmashComponent DownSmashComponentData
    | DamageComponent DamageComponentData



{-

   Actor Mutation

-}


updateComponents : Actor -> Components -> Actor
updateComponents actor components =
    { actor | components = components }


updateActor : Actors -> Actor -> Actors
updateActor actors actor =
    Dict.insert
        actor.id
        actor
        actors


updateActors : Level -> Actors -> Level
updateActors level actors =
    { level | actors = actors }



{-

   Actor Query

-}


getActorById : ActorId -> Level -> Maybe Actor
getActorById actorId level =
    Dict.get
        actorId
        level.actors


getActorsByPosition : Position -> Level -> List Actor
getActorsByPosition position level =
    getActorIdsByPosition position level
        |> List.map
            (\actorId ->
                getActorById actorId level
            )
        |> Maybe.Extra.values


getActorIdsByPosition : Position -> Level -> List ActorId
getActorIdsByPosition position =
    getActorIdsByXY position.x position.y


getActorIdsByXY : Int -> Int -> Level -> List ActorId
getActorIdsByXY x y level =
    Dict.get
        ( x, y )
        level.positionIndex
        |> Maybe.withDefault []


getActorsThatAffect : Position -> Level -> List Actor
getActorsThatAffect position level =
    List.map
        (\position ->
            getActorIdsByPosition position level
        )
        [ position
        , addPosition position <| getOffsetFromDirection Data.Common.Left
        , addPosition position <| getOffsetFromDirection Data.Common.Up
        , addPosition position <| getOffsetFromDirection Data.Common.Right
        , addPosition position <| getOffsetFromDirection Data.Common.Down
        ]
        |> List.concat
        |> List.map
            (\actorId ->
                getActorById actorId level
            )
        |> Maybe.Extra.values
        |> List.filter
            (\actor ->
                getTransformComponent actor
                    |> Maybe.andThen
                        (\transformData ->
                            if transformData.position == position then
                                Just True
                            else
                                case transformData.movingState of
                                    MovingTowards towardsData ->
                                        Just <| towardsData.position == position

                                    NotMoving ->
                                        Nothing
                        )
                    |> Maybe.withDefault False
            )


isEmpty : Position -> Level -> Bool
isEmpty position level =
    getActorsThatAffect position level
        |> List.isEmpty



{-

   Actor Remove

-}


removeActor : Actor -> Level -> Level
removeActor actor level =
    level
        |> (\level ->
                getPositionFromComponents actor.components
                    |> Maybe.andThen
                        (\position ->
                            Just <| removeActorFromIndex position actor.id level
                        )
                    |> Maybe.withDefault level
           )
        |> (\level ->
                Dict.remove actor.id level.actors
                    |> updateActors level
           )


removeActorWithPosition : Position -> ActorId -> Level -> Level
removeActorWithPosition position actorId level =
    level
        |> removeActorFromIndex position actorId
        |> (\level ->
                Dict.remove actorId level.actors
                    |> updateActors level
           )


removeActorFromIndex : Position -> ActorId -> Level -> Level
removeActorFromIndex position actorId level =
    Dict.update
        ( position.x, position.y )
        (\maybeActorIds ->
            case maybeActorIds of
                Just actorIds ->
                    List.Extra.remove
                        actorId
                        actorIds
                        |> Just

                Nothing ->
                    Nothing
        )
        level.positionIndex
        |> updatePositionIndex level


addActorToIndex : Position -> ActorId -> Level -> Level
addActorToIndex position actorId level =
    Dict.update
        ( position.x, position.y )
        (\maybeActorIds ->
            case maybeActorIds of
                Just actorIds ->
                    actorId
                        :: actorIds
                        |> List.Extra.unique
                        |> Just

                Nothing ->
                    Nothing
        )
        level.positionIndex
        |> updatePositionIndex level


updatePositionIndex : Level -> PositionIndex -> Level
updatePositionIndex level positionIndex =
    { level | positionIndex = positionIndex }



{-

   TransformComponent

-}


type alias TransformComponentData =
    { position : Position
    , movingState : MovingState
    }


type alias MovingTowardsData =
    { position : Position
    , totalTickCount : Int
    , tickCountLeft : Int
    , completionPercentage : Float
    }


type MovingState
    = NotMoving
    | MovingTowards MovingTowardsData


movingTicks : Tick
movingTicks =
    5


updateTransformComponent : TransformComponentData -> Actor -> Level -> Level
updateTransformComponent transformData actor level =
    getMovingTowardsData transformData
        |> Maybe.andThen
            (\towardsData ->
                if towardsData.tickCountLeft > 0 then
                    -- Still moving
                    Dict.insert
                        "transform"
                        (TransformComponent
                            { transformData
                                | movingState =
                                    MovingTowards
                                        { towardsData
                                            | completionPercentage = calculateCompletionPercentage towardsData.totalTickCount towardsData.tickCountLeft
                                        }
                            }
                        )
                        actor.components
                        |> updateComponents actor
                        |> updateActor level.actors
                        |> updateActors level
                        |> Just
                else
                    -- Finished moving
                    Dict.insert
                        "transform"
                        (TransformComponent
                            { position = towardsData.position
                            , movingState = NotMoving
                            }
                        )
                        actor.components
                        |> updateComponents actor
                        |> updateActor level.actors
                        |> updateActors level
                        |> removeActorFromIndex transformData.position actor.id
                        |> addActorToIndex towardsData.position actor.id
                        |> Just
            )
        |> Maybe.withDefault level


calculateCompletionPercentage : Int -> Int -> Float
calculateCompletionPercentage totalTickCount tickCountLeft =
    100 / (toFloat (totalTickCount)) * (toFloat (totalTickCount - tickCountLeft))


getTransformComponent : Actor -> Maybe TransformComponentData
getTransformComponent actor =
    Dict.get "transform" actor.components
        |> Maybe.andThen
            (\component ->
                case component of
                    TransformComponent data ->
                        Just data

                    _ ->
                        Nothing
            )


getPositionFromComponents : Components -> Maybe Position
getPositionFromComponents components =
    Dict.get "transform" components
        |> Maybe.andThen
            (\component ->
                case component of
                    TransformComponent data ->
                        Just data.position

                    _ ->
                        Nothing
            )


getMovingTowardsData : TransformComponentData -> Maybe MovingTowardsData
getMovingTowardsData transformData =
    case transformData.movingState of
        MovingTowards towardsData ->
            Just towardsData

        NotMoving ->
            Nothing


isMoving : TransformComponentData -> Bool
isMoving transformData =
    getMovingTowardsData transformData
        |> Maybe.Extra.isJust


isNotMoving : TransformComponentData -> Bool
isNotMoving transformComponent =
    isMoving transformComponent |> not


getNewPosition : Direction -> TransformComponentData -> ( TransformComponentData, Position )
getNewPosition direction transformData =
    ( transformData, addPosition transformData.position (getOffsetFromDirection direction) )


isMovingDown : TransformComponentData -> Bool
isMovingDown transformData =
    getMovingTowardsData transformData
        |> Maybe.Extra.filter
            (\towardsData ->
                addPosition transformData.position (getOffsetFromDirection Data.Common.Down) == towardsData.position
            )
        |> Maybe.Extra.isJust


startMovingTowards : Actor -> TransformComponentData -> Position -> Level -> Level
startMovingTowards actor transformData newPosition level =
    Dict.insert
        "transform"
        (TransformComponent
            { transformData
                | movingState =
                    MovingTowards
                        { position = newPosition
                        , totalTickCount = movingTicks
                        , tickCountLeft = 0
                        , completionPercentage = 0.0
                        }
            }
        )
        actor.components
        |> updateComponents actor
        |> updateActor level.actors
        |> updateActors level



{-

   PlayerInputComponent

-}


updatePlayerInputComponent : Maybe Direction -> Actor -> Level -> Level
updatePlayerInputComponent maybeDirection actor level =
    maybeDirection
        |> Maybe.andThen
            (\direction ->
                handleDirection direction actor level
            )
        |> Maybe.withDefault level


handleDirection : Direction -> Actor -> Level -> Maybe Level
handleDirection direction actor level =
    getTransformComponent actor
        |> Maybe.Extra.toList
        |> List.filter isNotMoving
        |> List.map (getNewPosition direction)
        |> List.head
        |> Maybe.andThen
            (\( transformData, position ) ->
                case getActorsThatAffect position level of
                    [] ->
                        Just ( transformData, position, level )

                    [ otherActor ] ->
                        if hasRigidComponent otherActor then
                            tryToPush direction actor transformData otherActor level
                        else
                            Just ( transformData, position, level )

                    _ ->
                        -- @todo what to do if two actors are here?
                        Nothing
            )
        |> Maybe.andThen
            (\( transformData, newPosition, level ) ->
                Just <| startMovingTowards actor transformData newPosition level
            )


tryToPush : Direction -> Actor -> TransformComponentData -> Actor -> Level -> Maybe ( TransformComponentData, Position, Level )
tryToPush direction actor transformData otherActor level =
    getPhysicsComponent otherActor
        |> Maybe.Extra.toList
        |> List.filter isCircle
        |> List.head
        |> Maybe.andThen
            (\physics ->
                getTransformComponent otherActor
            )
        |> Maybe.Extra.toList
        |> List.filter isNotMoving
        |> List.map (getNewPosition direction)
        |> List.head
        |> Maybe.andThen
            (\( otherTransformData, pushedToPosition ) ->
                if isEmpty pushedToPosition level then
                    Just
                        ( transformData
                        , otherTransformData.position
                        , startMovingTowards otherActor otherTransformData pushedToPosition level
                        )
                else
                    Nothing
            )



{-

   PhysicsComponent

-}


type alias PhysicsComponentData =
    { strength : Int
    , shape : Shape
    , affectedByGravity : Bool
    }


type Shape
    = Circle
    | Square


updatePhysicsComponent : PhysicsComponentData -> Actor -> Level -> Level
updatePhysicsComponent physics actor level =
    -- Checking if this actors requirements are met
    [ physics ]
        |> List.filter .affectedByGravity
        |> List.filter isCircle
        |> List.map
            (\physics ->
                getTransformComponent actor
            )
        |> Maybe.Extra.values
        |> List.filter isNotMoving
        -- Check for possible actions
        |> List.concatMap
            (\transformData ->
                [ ( transformData
                  , Data.Common.Down
                  , [ isEmpty <| addPosition transformData.position (getOffsetFromDirection Data.Common.Down) ]
                  )
                , ( transformData
                  , Data.Common.Left
                  , [ isEmpty <| addPosition transformData.position (getOffsetFromDirection Data.Common.Left)
                    , isEmpty <|
                        addPositions
                            [ transformData.position
                            , (getOffsetFromDirection Data.Common.Left)
                            , (getOffsetFromDirection Data.Common.Down)
                            ]
                    , isCircleAt <| addPosition transformData.position (getOffsetFromDirection Data.Common.Down)
                    ]
                  )
                , ( transformData
                  , Data.Common.Right
                  , [ isEmpty <| addPosition transformData.position (getOffsetFromDirection Data.Common.Right)
                    , isEmpty <|
                        addPositions
                            [ transformData.position
                            , (getOffsetFromDirection Data.Common.Right)
                            , (getOffsetFromDirection Data.Common.Down)
                            ]
                    , isCircleAt <| addPosition transformData.position (getOffsetFromDirection Data.Common.Down)
                    ]
                  )
                ]
            )
        |> List.Extra.find
            (\( transformData, _, predicates ) ->
                List.all
                    (\predicate ->
                        predicate level
                    )
                    predicates
            )
        |> Maybe.andThen
            (\( transformData, direction, _ ) ->
                Just <|
                    startMovingTowards actor
                        transformData
                        (addPosition transformData.position <| getOffsetFromDirection direction)
                        level
            )
        |> Maybe.withDefault
            level


isCircleAt : Position -> Level -> Bool
isCircleAt position level =
    getActorsByPosition position level
        |> List.map getPhysicsComponent
        |> Maybe.Extra.values
        |> List.filter isCircle
        |> List.isEmpty
        |> not


getPhysicsComponent : Actor -> Maybe PhysicsComponentData
getPhysicsComponent actor =
    Dict.get "physics" actor.components
        |> Maybe.andThen
            (\component ->
                case component of
                    PhysicsComponent data ->
                        Just data

                    _ ->
                        Nothing
            )


isCircle : PhysicsComponentData -> Bool
isCircle physicsData =
    case physicsData.shape of
        Circle ->
            True

        _ ->
            False



{-

   DiamondCollectorComponent

-}


updateDiamondCollectorComponent : Actor -> Level -> Level
updateDiamondCollectorComponent collectorActor level =
    getTransformComponent collectorActor
        |> Maybe.Extra.toList
        |> List.map .position
        |> List.concatMap
            (\position ->
                getActorsByPosition position level
                    |> List.map
                        (\actor ->
                            ( position, actor )
                        )
            )
        |> List.foldr
            (\( position, actor ) level ->
                if Dict.member "diamond" actor.components then
                    level
                        |> removeActorWithPosition position actor.id
                        |> collectDiamond
                else
                    level
            )
            level


collectDiamond : Level -> Level
collectDiamond level =
    { level | diamonds = incrementDiamondsCollected level.diamonds }


incrementDiamondsCollected : Diamonds -> Diamonds
incrementDiamondsCollected diamonds =
    { diamonds | collected = diamonds.collected + 1 }


incrementTotalDiamonds : Diamonds -> Diamonds
incrementTotalDiamonds diamonds =
    { diamonds | total = diamonds.total + 1 }


updateDiamonds : Diamonds -> Level -> Level
updateDiamonds diamonds level =
    { level | diamonds = diamonds }



{-

   CanSquashComponent

-}


updateCanSquashComponent : Actor -> Level -> Level
updateCanSquashComponent squashingActor level =
    getTransformComponent squashingActor
        |> Maybe.Extra.toList
        |> List.map .position
        |> List.concatMap
            (\position ->
                getActorsByPosition position level
                    |> List.map
                        (\actor ->
                            ( position, actor )
                        )
            )
        |> List.foldr
            (\( position, actor ) level ->
                if Dict.member "squashable" actor.components then
                    removeActorWithPosition position actor.id level
                else
                    level
            )
            level



{-

   RigidComponent

-}


hasRigidComponent : Actor -> Bool
hasRigidComponent actor =
    Dict.member "rigid" actor.components



{-

   AiComponent

-}


type alias AiComponentData =
    { previousDirection : Direction
    }


updateAiComponent : AiComponentData -> Actor -> Level -> Level
updateAiComponent ai actor level =
    getTransformComponent actor
        |> Maybe.Extra.toList
        |> List.filter isNotMoving
        |> List.concatMap
            (\transformData ->
                [ ( transformData, getDirectionFromID <| (getIDFromDirection ai.previousDirection) - 3 )
                , ( transformData, getDirectionFromID <| (getIDFromDirection ai.previousDirection) - 4 )
                , ( transformData, getDirectionFromID <| (getIDFromDirection ai.previousDirection) - 5 )
                , ( transformData, getDirectionFromID <| (getIDFromDirection ai.previousDirection) - 6 )
                ]
            )
        |> List.Extra.find
            (\( transformData, direction ) ->
                isEmpty
                    (addPosition transformData.position (getOffsetFromDirection direction))
                    level
            )
        |> Maybe.andThen
            (\( transformData, direction ) ->
                Dict.insert
                    "ai"
                    (AiComponent
                        { ai | previousDirection = direction }
                    )
                    actor.components
                    |> updateComponents actor
                    |> (\actor ->
                            startMovingTowards
                                actor
                                transformData
                                (addPosition transformData.position <| getOffsetFromDirection direction)
                                level
                       )
                    |> Just
            )
        |> Maybe.withDefault level



{-

   CameraComponent

-}


type alias CameraComponentData =
    { borderSize : Int
    }


updateCameraComponent : CameraComponentData -> Actor -> Level -> Level
updateCameraComponent camera actor level =
    getTransformComponent actor
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
                in
                    Just { level | view = updateViewPosition newViewPosition view }
            )
        |> Maybe.withDefault level


updateView : View -> Level -> Level
updateView view level =
    { level | view = view }


updateViewPosition : Position -> View -> View
updateViewPosition position view =
    { view | position = position }



{-

   DamageComponent

-}


type alias DamageComponentData =
    { remainingTicks : Int
    }


updateDamageComponent : DamageComponentData -> Actor -> Level -> Level
updateDamageComponent damageData actor level =
    level
        |> tryDoDamage actor
        |> removeExplosionIfEnded actor damageData


tryDoDamage : Actor -> Level -> Level
tryDoDamage damageDealingActor level =
    getTransformComponent damageDealingActor
        |> Maybe.Extra.toList
        |> List.concatMap
            (\transformData ->
                getActorsThatAffect transformData.position level
            )
        |> List.filter
            (\actor ->
                actor.id /= damageDealingActor.id
            )
        |> List.foldr
            (\actor level ->
                removeActor actor level
            )
            level


removeExplosionIfEnded : Actor -> DamageComponentData -> Level -> Level
removeExplosionIfEnded actor damageData level =
    if damageData.remainingTicks > 0 then
        Dict.insert
            "damage"
            (DamageComponent { damageData | remainingTicks = damageData.remainingTicks - 1 })
            actor.components
            |> updateComponents actor
            |> updateActor level.actors
            |> updateActors level
    else
        removeActor actor level



{-

   DownSmashComponent

-}


type alias DownSmashComponentData =
    { wasMovingDown : Bool
    }


updateDownSmashComponent : DownSmashComponentData -> Actor -> Level -> Level
updateDownSmashComponent downSmashData actor level =
    getTransformComponent actor
        |> Maybe.andThen
            (\transformData ->
                Just ( transformData.position, isMovingDown transformData )
            )
        |> Maybe.andThen
            (\( position, movingDown ) ->
                (case ( movingDown, downSmashData.wasMovingDown ) of
                    ( False, True ) ->
                        getActorsByPosition
                            (addPosition position <| getOffsetFromDirection Data.Common.Down)
                            level
                            |> List.filter hasExplodableComponent
                            |> List.foldr
                                (\downActor level ->
                                    level
                                        |> createBigExplosion (addPosition position <| getOffsetFromDirection Data.Common.Down)
                                        |> removeActorWithPosition (addPosition position <| getOffsetFromDirection Data.Common.Down) downActor.id
                                )
                                level

                    _ ->
                        level
                )
                    -- Update the WasMovingDown
                    |> (\level ->
                            updateDownSmash
                                { downSmashData | wasMovingDown = movingDown }
                                actor
                                level
                       )
                    |> Just
            )
        |> Maybe.withDefault level


updateDownSmash : DownSmashComponentData -> Actor -> Level -> Level
updateDownSmash downSmashData actor level =
    Dict.insert
        "downsmash"
        (DownSmashComponent downSmashData)
        actor.components
        |> updateComponents actor
        |> updateActor level.actors
        |> updateActors level


createBigExplosion : Position -> Level -> Level
createBigExplosion position level =
    List.foldr
        (\position level ->
            level |> addExplosion position.x position.y
        )
        level
        [ addPositions [ position, getOffsetFromDirection Data.Common.Left, getOffsetFromDirection Data.Common.Up ]
        , addPositions [ position, getOffsetFromDirection Data.Common.Up ]
        , addPositions [ position, getOffsetFromDirection Data.Common.Right, getOffsetFromDirection Data.Common.Up ]
        , addPositions [ position, getOffsetFromDirection Data.Common.Left ]
        , position
        , addPositions [ position, getOffsetFromDirection Data.Common.Right ]
        , addPositions [ position, getOffsetFromDirection Data.Common.Left, getOffsetFromDirection Data.Common.Down ]
        , addPositions [ position, getOffsetFromDirection Data.Common.Down ]
        , addPositions [ position, getOffsetFromDirection Data.Common.Right, getOffsetFromDirection Data.Common.Down ]
        ]



{-

   RenderComponent

-}


type alias RenderComponentData =
    { colors : List Color
    , ticksPerColor : Int
    }



{-

   ExplodableComponent

-}


hasExplodableComponent : Actor -> Bool
hasExplodableComponent actor =
    Dict.member "explodable" actor.components



{-

   Add Actors

-}


addActor : Components -> Level -> Level
addActor components level =
    level
        |> incrementNextActorId
        |> (\level ->
                ( level
                , { id = level.nextActorId
                  , components = components
                  }
                )
           )
        -- Add actor to the index
        |> (\( level, actor ) ->
                getPositionFromComponents components
                    |> Maybe.andThen
                        (\position ->
                            Just <| ( addActorToIndex position actor.id level, actor )
                        )
                    |> Maybe.withDefault
                        ( level, actor )
           )
        -- Add actor to the actors
        |> (\( level, actor ) ->
                updateActor level.actors actor
                    |> updateActors level
           )


incrementNextActorId : Level -> Level
incrementNextActorId level =
    { level | nextActorId = level.nextActorId + 1 }


addPlayer : Int -> Int -> Int -> Level -> Level
addPlayer x y borderSize level =
    level
        |> addActor
            (Dict.fromList
                [ ( "transform", TransformComponent { position = { x = x, y = y }, movingState = NotMoving } )
                , ( "render", RenderComponent { colors = [ Color.darkGreen ], ticksPerColor = 1 } )
                , ( "player-input", PlayerInputComponent )
                , ( "diamond-collector", DiamondCollectorComponent )
                , ( "can-squash", CanSquashComponent )
                , ( "rigid", RigidComponent )
                , ( "physics"
                  , PhysicsComponent
                        { strength = 10
                        , shape = Square
                        , affectedByGravity = False
                        }
                  )
                , ( "camera", CameraComponent { borderSize = borderSize } )
                , ( "explodable", ExplodableComponent )
                ]
            )
        |> (\level ->
                level.view
                    |> updateViewPosition { x = x - borderSize, y = y - borderSize }
                    |> (flip updateView) level
           )


addRock : Int -> Int -> Level -> Level
addRock x y level =
    addActor
        (Dict.fromList
            [ ( "transform", TransformComponent { position = { x = x, y = y }, movingState = NotMoving } )
            , ( "render", RenderComponent { colors = [ Color.darkGray ], ticksPerColor = 1 } )
            , ( "rigid", RigidComponent )
            , ( "physics"
              , PhysicsComponent
                    { strength = 20
                    , shape = Circle
                    , affectedByGravity = True
                    }
              )
            , ( "downsmash", DownSmashComponent { wasMovingDown = False } )
            ]
        )
        level


addExplosive : Int -> Int -> Level -> Level
addExplosive x y level =
    addActor
        (Dict.fromList
            [ ( "transform", TransformComponent { position = { x = x, y = y }, movingState = NotMoving } )
            , ( "render", RenderComponent { colors = [ Color.red ], ticksPerColor = 1 } )
            , ( "rigid", RigidComponent )
            , ( "explodable", ExplodableComponent )
            , ( "physics"
              , PhysicsComponent
                    { strength = 10
                    , shape = Circle
                    , affectedByGravity = True
                    }
              )
            ]
        )
        level


addExplosion : Int -> Int -> Level -> Level
addExplosion x y level =
    addActor
        (Dict.fromList
            [ ( "transform", TransformComponent { position = { x = x, y = y }, movingState = NotMoving } )
            , ( "render", RenderComponent { colors = [ Color.red, Color.darkOrange, Color.yellow ], ticksPerColor = 2 } )
            , ( "damage", DamageComponent { remainingTicks = 8 } )
            ]
        )
        level


addEnemy : Int -> Int -> Level -> Level
addEnemy x y level =
    addActor
        (Dict.fromList
            [ ( "transform", TransformComponent { position = { x = x, y = y }, movingState = NotMoving } )
            , ( "render", RenderComponent { colors = [ Color.darkOrange ], ticksPerColor = 1 } )
            , ( "rigid", RigidComponent )
            , ( "physics"
              , PhysicsComponent
                    { strength = 20
                    , shape = Circle
                    , affectedByGravity = False
                    }
              )
            , ( "ai"
              , AiComponent
                    { previousDirection = Data.Common.Right }
              )
            , ( "explodable", ExplodableComponent )
            ]
        )
        level


addDirt : Int -> Int -> Level -> Level
addDirt x y level =
    addActor
        (Dict.fromList
            [ ( "transform", TransformComponent { position = { x = x, y = y }, movingState = NotMoving } )
            , ( "render", RenderComponent { colors = [ Color.lightBrown ], ticksPerColor = 1 } )
            , ( "squashable", SquashableComponent )
            , ( "physics"
              , PhysicsComponent
                    { strength = 1
                    , shape = Square
                    , affectedByGravity = False
                    }
              )
            ]
        )
        level


addWall : Int -> Int -> Level -> Level
addWall x y level =
    addActor
        (Dict.fromList
            [ ( "transform", TransformComponent { position = { x = x, y = y }, movingState = NotMoving } )
            , ( "render", RenderComponent { colors = [ Color.rgb 98 100 87 ], ticksPerColor = 1 } )
            , ( "rigid", RigidComponent )
            , ( "physics"
              , PhysicsComponent
                    { strength = 100
                    , shape = Square
                    , affectedByGravity = False
                    }
              )
            ]
        )
        level


addStrongWall : Int -> Int -> Level -> Level
addStrongWall x y level =
    addActor
        (Dict.fromList
            [ ( "transform", TransformComponent { position = { x = x, y = y }, movingState = NotMoving } )
            , ( "render", RenderComponent { colors = [ Color.black ], ticksPerColor = 1 } )
            , ( "rigid", RigidComponent )
            , ( "physics"
              , PhysicsComponent
                    { strength = 100
                    , shape = Square
                    , affectedByGravity = False
                    }
              )
            ]
        )
        level


addDiamond : Int -> Int -> Level -> Level
addDiamond x y level =
    level
        |> addActor
            (Dict.fromList
                [ ( "transform", TransformComponent { position = { x = x, y = y }, movingState = NotMoving } )
                , ( "render", RenderComponent { colors = [ Color.blue, Color.lightBlue ], ticksPerColor = 12 } )
                , ( "diamond", DiamondComponent )
                , ( "physics"
                  , PhysicsComponent
                        { strength = 100
                        , shape = Circle
                        , affectedByGravity = True
                        }
                  )
                ]
            )
        |> (\level ->
                updateDiamonds
                    (incrementTotalDiamonds level.diamonds)
                    level
           )



{-

   Position Helpers

-}


getOffsetFromDirection : Direction -> Position
getOffsetFromDirection direction =
    case direction of
        Data.Common.Left ->
            { x = -1, y = 0 }

        Data.Common.Up ->
            { x = 0, y = -1 }

        Data.Common.Right ->
            { x = 1, y = 0 }

        Data.Common.Down ->
            { x = 0, y = 1 }


addPositions : List Position -> Position
addPositions =
    List.foldr
        (\position acc ->
            addPosition position acc
        )
        { x = 0, y = 0 }


addPosition : Position -> Position -> Position
addPosition pos1 pos2 =
    { x = pos1.x + pos2.x
    , y = pos1.y + pos2.y
    }



-- @todo we might be able to combine this with InputController.keyCodeToDirection


getDirectionFromID : Int -> Direction
getDirectionFromID id =
    case id % 4 of
        0 ->
            Data.Common.Left

        1 ->
            Data.Common.Up

        2 ->
            Data.Common.Right

        _ ->
            Data.Common.Down


getIDFromDirection : Direction -> Int
getIDFromDirection direction =
    case direction of
        Data.Common.Left ->
            0

        Data.Common.Up ->
            1

        Data.Common.Right ->
            2

        Data.Common.Down ->
            3
