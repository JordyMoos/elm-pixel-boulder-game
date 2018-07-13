module Actor.Actor
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
                                    MovingTowardsData towardsData ->
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


removeActor : Position -> ActorId -> Level -> Level
removeActor position actorId level =
    level
        |> removeActorFromIndex position actorId
        |> Dict.remove actorId level.actors
        |> updateActors level


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


getMovingTowardsData : TransformComponentData -> Maybe MovingTowardsData
getMovingTowardsData transformData =
    case transformData.movingState of
        MovingTowards transformData ->
            Just transformData

        NotMoving ->
            Nothing


isMoving : TransformComponentData -> Bool
isMoving transformData =
    getMovingTowardsData transformData
        |> Maybe.Extra.isJust


isNotMoving : TransformComponentData -> Bool
isNotMoving =
    not <| isMoving


getNewPosition : Direction -> TransformComponentData -> ( TransformComponentData, Position )
getNewPosition direction transformData =
    ( transformData, addPosition transformData.position (getOffsetFromDirection direction) )



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
        |> List.map (always <| getTransformComponent otherActor)
        |> Maybe.Extra.toList
        |> List.filter isNotMoving
        |> List.map (getNewPosition direction)
        |> List.map
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


startMovingTowards : Actor -> TransformComponentData -> Position -> Level -> Level
startMovingTowards actor transformData newPosition level =
    Dict.insert
        "transform"
        (TransformComponent
            { transformData
                | movingState =
                    MovingTowards
                        { position = newPosition
                        , tickTotal = movingTicks
                        , tickCounter = 0
                        , completionPercentage = 0.0
                        }
            }
        )
        actor.components
        |> updateComponents actor
        |> updateActor level.actors
        |> updateActors level



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
                getTransformComponent.actor
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
        |> List.map
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
                        |> removeActor position actor.id
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



{-

   CanSquashComponent

-}


updateCanSquashComponent : Actor -> Level -> Level
updateCanSquashComponent squashingActor level =
    getTransformComponent squashingActor
        |> Maybe.Extra.toList
        |> List.map .position
        |> List.map
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
                    removeActor position actor.id level
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
    level



{-

   CameraComponent

-}


type alias CameraComponentData =
    { borderSize : Int
    }



{-

   DamageComponent

-}


type alias DamageComponentData =
    { remainingTicks : Tick
    }



{-

   DownSmashComponent

-}


type alias DownSmashComponentData =
    { wasMovingDown : Bool
    }



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
