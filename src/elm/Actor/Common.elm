module Actor.Common
    exposing
        ( updateComponents
        , updateActor
        , updateActors
        , updateView
        , updateViewPosition
          -- Remove
        , removeActor
          -- Add
        , addActor
        , addActorToIndex
          -- Query
        , getActorById
        , getActorsByPosition
        , getActorIdsByPosition
        , getActorIdsByXY
        , getActorsThatAffectNeighborPosition
        , getActorsThatAffect
        , isEmpty
        , isDestinationEmpty
        , isDestinationEmptyByOffset
          -- TransformComponent
        , updateTransformComponent
        , getTransformComponent
        , getPosition
        , getMovingTowardsData
        , isActorMoving
        , isMoving
        , isNotMoving
        , isMovingAt
        , isNotMovingAt
        , isMovingDown
        , startMovingTowards
          -- TagComponent
        , getTagComponent
        )

import Data.Position exposing (Position)
import Actor.Actor as Actor
    exposing
        ( ActorId
        , Actor
        , Actors
        , Components
        , Level
        , View
        , PositionIndex
          -- For TransformComponent
        , Component(TransformComponent, TagComponent)
        , TransformComponentData
        , MovingState(..)
        , MovingTowardsData
          -- For TagComponent
        , TagComponentData
          -- For EventManager
        , Event
        , Events
        , Subscriber
        , Subscribers
        , EventManager
        )
import Dict
import List.Extra
import Data.Position as Position exposing (Position)
import Data.Direction as Direction exposing (Direction)
import Maybe.Extra


{-

   State update

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


updatePositionIndex : Level -> PositionIndex -> Level
updatePositionIndex level positionIndex =
    { level | positionIndex = positionIndex }


updateView : View -> Level -> Level
updateView view level =
    { level | view = view }


updateViewPosition : Position -> View -> View
updateViewPosition position view =
    { view | position = position }



{-

   Remove

-}


removeActor : Actor -> Level -> Level
removeActor actor level =
    level
        |> removeActorFromIndex actor
        |> removeActorFromDict actor.id


removeActorFromIndex : Actor -> Level -> Level
removeActorFromIndex actor level =
    getTransformComponent actor
        |> Maybe.map .position
        |> Maybe.map
            (\position ->
                removeActorFromIndexByPosition position actor.id level
            )
        |> Maybe.withDefault level


removeActorFromIndexByPosition : Position -> ActorId -> Level -> Level
removeActorFromIndexByPosition position actorId level =
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


removeActorFromDict : ActorId -> Level -> Level
removeActorFromDict actorId level =
    Dict.remove actorId level.actors
        |> updateActors level



{-

   Add

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
                getPosition actor
                    |> Maybe.andThen
                        (\position ->
                            Just <| ( addActorToIndex position actor.id level, actor )
                        )
                    |> Maybe.withDefault
                        ( level, actor )
           )
        -- Update view if needed
        |> (\( level, actor ) ->
                getCameraComponent actor
                    |> Maybe.andThen
                        (\camera ->
                            getTransformComponent actor
                        )
                    |> Maybe.andThen
                        (\transform ->
                            updateViewPosition
                                { x = transform.position.x - (round ((toFloat level.view.width) / 2))
                                , y = transform.position.y - (round ((toFloat level.view.height) / 2))
                                }
                                level.view
                                |> (flip updateView) level
                                |> Just
                        )
                    |> Maybe.andThen
                        (\level ->
                            Just ( level, actor )
                        )
                    |> Maybe.withDefault ( level, actor )
           )
        -- Create Event
        --        |> (\( level, actor ) ->
        --                ( { level
        --                    | eventManager =
        --                        addEvent
        --                            (Actor.ActorAdded actor)
        --                            level.manager
        --                  }
        --                , actor
        --                )
        --           )
        -- Add actor to the actors
        |> (\( level, actor ) ->
                updateActor level.actors actor
                    |> updateActors level
           )


incrementNextActorId : Level -> Level
incrementNextActorId level =
    { level | nextActorId = level.nextActorId + 1 }


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
                    Just [ actorId ]
        )
        level.positionIndex
        |> updatePositionIndex level


getCameraComponent : Actor -> Maybe Actor.CameraComponentData
getCameraComponent actor =
    Dict.get "camera" actor.components
        |> Maybe.andThen
            (\component ->
                case component of
                    Actor.CameraComponent data ->
                        Just data

                    _ ->
                        Nothing
            )



{-

   Query

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


getActorsThatAffectNeighborPosition : Actor -> Direction -> Level -> List Actor
getActorsThatAffectNeighborPosition actor direction level =
    getPosition actor
        |> Maybe.map
            (\position ->
                Position.addPosition position (Position.getOffsetFromDirection direction)
            )
        |> Maybe.map (flip getActorsThatAffect level)
        |> Maybe.withDefault []


getActorsThatAffect : Position -> Level -> List Actor
getActorsThatAffect position level =
    List.map
        (\position ->
            getActorIdsByPosition position level
        )
        [ position
        , Position.addPosition position <| Position.getOffsetFromDirection Direction.Left
        , Position.addPosition position <| Position.getOffsetFromDirection Direction.Up
        , Position.addPosition position <| Position.getOffsetFromDirection Direction.Right
        , Position.addPosition position <| Position.getOffsetFromDirection Direction.Down
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


isDestinationEmpty : Actor -> Direction -> Level -> Bool
isDestinationEmpty actor direction level =
    isDestinationEmptyByOffset actor (Position.getOffsetFromDirection direction) level


isDestinationEmptyByOffset : Actor -> Position -> Level -> Bool
isDestinationEmptyByOffset actor offset level =
    getPosition actor
        |> Maybe.map
            (\position ->
                Position.addPosition position offset
            )
        |> Maybe.map
            (\targetPosition ->
                isEmpty targetPosition level
            )
        |> Maybe.withDefault True



{-

   TransformComponent

-}


movingTicks : Int
movingTicks =
    4


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
                                            | tickCountLeft = towardsData.tickCountLeft - 1
                                            , completionPercentage = calculateCompletionPercentage towardsData.totalTickCount towardsData.tickCountLeft
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
                        |> removeActorFromIndexByPosition transformData.position actor.id
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


getPosition : Actor -> Maybe Position
getPosition actor =
    getTransformComponent actor
        |> Maybe.map .position


getMovingTowardsData : TransformComponentData -> Maybe MovingTowardsData
getMovingTowardsData transformData =
    case transformData.movingState of
        MovingTowards towardsData ->
            Just towardsData

        NotMoving ->
            Nothing


isActorMoving : Actor -> Bool
isActorMoving actor =
    getTransformComponent actor
        |> Maybe.map isMoving
        |> Maybe.withDefault False


isMoving : TransformComponentData -> Bool
isMoving transformData =
    getMovingTowardsData transformData
        |> Maybe.Extra.isJust


isNotMoving : TransformComponentData -> Bool
isNotMoving transformComponent =
    isMoving transformComponent |> not


isMovingAt : Position -> Level -> Bool
isMovingAt position level =
    getActorsByPosition position level
        |> List.map getTransformComponent
        |> Maybe.Extra.values
        |> List.filter isMoving
        |> List.isEmpty
        |> not


isNotMovingAt : Position -> Level -> Bool
isNotMovingAt position level =
    isMovingAt position level
        |> not


getNewPosition : Direction -> TransformComponentData -> ( TransformComponentData, Position )
getNewPosition direction transformData =
    ( transformData, Position.addPosition transformData.position (Position.getOffsetFromDirection direction) )


isMovingDown : TransformComponentData -> Bool
isMovingDown transformData =
    getMovingTowardsData transformData
        |> Maybe.Extra.filter
            (\towardsData ->
                Position.addPosition transformData.position (Position.getOffsetFromDirection Direction.Down) == towardsData.position
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
                        , tickCountLeft = movingTicks
                        , completionPercentage = 0.0
                        }
            }
        )
        actor.components
        |> updateComponents actor
        |> updateActor level.actors
        |> updateActors level



{-

   TagComponent

-}


getTagComponent : Actor -> Maybe TagComponentData
getTagComponent actor =
    Dict.get "tag" actor.components
        |> Maybe.andThen
            (\component ->
                case component of
                    TagComponent data ->
                        Just data

                    _ ->
                        Nothing
            )



{-

   EventManager

-}


addEvent : Event -> Level -> Level
addEvent event level =
    { level | events = event :: level.events }


handleEvents : EventManager -> Level -> Level
handleEvents manager level =
    List.foldr (handleEvent manager.subscribers) level level.events


handleEvent : Subscribers -> Event -> Level -> Level
handleEvent subscribers event level =
    level
