module Actor.Common exposing
    ( addActor
    , addActorToIndices
    , addEvent
    , getActorById
    , getActorIdsByPosition
    , getActorIdsByXY
    , getActorsByPosition
    , getActorsThatAffect
    , getActorsThatAffectNeighborPosition
    , getCameraComponent
    , getDynamicActorIdsByXY
    , getMovementComponent
    , getMovingTowardsData
    , getPosition
    , getTagComponent
    , getTransformComponent
    , isDestinationEmpty
    , isDestinationEmptyByOffset
    , isEmpty
    , isNotEmpty
    , removeActor
    , removeActorFromIndices
    , removeActorFromIndicesByPosition
    , setView
    , updateActor
    , updateActors
    , updateComponents
    , updateViewCoordinate
    )

import Actor.Actor as Actor
    exposing
        ( Actor
        , ActorId
        , ActorType(..)
        , Actors
        , Component(..)
        , Components
        , Event
        , EventManager
        , Events
        , Level
        , MovementComponentData
        , MovingState(..)
        , MovingTowardsData
        , PositionIndex
        , PositionIndices
        , Subscriber
        , Subscribers
        , TagComponentData
        , TransformComponentData
        , View
        )
import Data.Coordinate exposing (Coordinate)
import Data.Direction as Direction exposing (Direction)
import Data.Position as Position exposing (Position)
import Dict
import List.Extra
import Maybe.Extra
import Pilf exposing (flip)
import Util.Util as Util



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


updateStaticPositionIndex : PositionIndex -> PositionIndices -> PositionIndices
updateStaticPositionIndex static indices =
    { indices | static = static }


updateDynamicPositionIndex : PositionIndex -> PositionIndices -> PositionIndices
updateDynamicPositionIndex dynamic indices =
    { indices | dynamic = dynamic }


updatePositionIndices : Level -> PositionIndices -> Level
updatePositionIndices level positionIndices =
    { level | positionIndices = positionIndices }


setView : View -> Level -> Level
setView view level =
    { level | view = view }


updateViewCoordinate : Coordinate -> View -> View
updateViewCoordinate coordinate view =
    { view | coordinate = coordinate }



{-

   Remove

-}


removeActor : Actor -> Level -> Level
removeActor actor level =
    level
        |> removeActorFromIndices actor
        |> removeActorFromDict actor.id
        |> addEvent (Actor.ActorRemoved actor)


removeActorFromIndices : Actor -> Level -> Level
removeActorFromIndices actor level =
    getTransformComponent actor
        |> Maybe.map .position
        |> Maybe.map
            (\position ->
                removeActorFromIndicesByPosition position actor.id level
            )
        |> Maybe.withDefault level


removeActorFromIndicesByPosition : Position -> ActorId -> Level -> Level
removeActorFromIndicesByPosition position actorId level =
    let
        static =
            removeActorFromIndexByPosition position actorId level.positionIndices.static

        dynamic =
            removeActorFromIndexByPosition position actorId level.positionIndices.dynamic
    in
    level.positionIndices
        |> updateStaticPositionIndex static
        |> updateDynamicPositionIndex dynamic
        |> updatePositionIndices level


removeActorFromIndexByPosition : Position -> ActorId -> PositionIndex -> PositionIndex
removeActorFromIndexByPosition position actorId index =
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
        index


removeActorFromDict : ActorId -> Level -> Level
removeActorFromDict actorId level =
    Dict.remove actorId level.actors
        |> updateActors level



{-

   Add

-}


addActor : Components -> Level -> Level
addActor components givenLevel =
    givenLevel
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
                    |> Maybe.map
                        (\position ->
                            ( addActorToIndices position actor level, actor )
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
                    |> Maybe.map
                        (\transform ->
                            updateViewCoordinate
                                { x = (transform.position.x - round (toFloat level.view.width / 2)) * level.view.pixelSize
                                , y = (transform.position.y - round (toFloat level.view.height / 2)) * level.view.pixelSize
                                }
                                level.view
                                |> (\b a -> setView a b) level
                        )
                    |> Maybe.map
                        (\updatedLevel ->
                            ( updatedLevel, actor )
                        )
                    |> Maybe.withDefault ( level, actor )
           )
        -- Create Event
        |> (\( level, actor ) ->
                ( addEvent
                    (Actor.ActorAdded actor)
                    level
                , actor
                )
           )
        -- Add actor to the actors
        |> (\( level, actor ) ->
                updateActor level.actors actor
                    |> updateActors level
           )


incrementNextActorId : Level -> Level
incrementNextActorId level =
    { level | nextActorId = level.nextActorId + 1 }


addActorToIndices : Position -> Actor -> Level -> Level
addActorToIndices position actor level =
    case getActorType actor of
        StaticActor ->
            addActorToStaticIndex level position actor.id
                |> Pilf.flip updateStaticPositionIndex level.positionIndices
                |> updatePositionIndices level

        DynamicActor ->
            addActorToDynamicIndex level position actor.id
                |> Pilf.flip updateDynamicPositionIndex level.positionIndices
                |> updatePositionIndices level


addActorToIndex : PositionIndex -> Position -> ActorId -> PositionIndex
addActorToIndex index position actorId =
    Dict.update
        ( position.x, position.y )
        (\maybeActorIds ->
            case maybeActorIds of
                Just actorIds ->
                    (actorId :: actorIds)
                        |> List.Extra.unique
                        |> Just

                Nothing ->
                    Just [ actorId ]
        )
        index


addActorToStaticIndex : Level -> Position -> ActorId -> PositionIndex
addActorToStaticIndex level =
    addActorToIndex level.positionIndices.static


addActorToDynamicIndex : Level -> Position -> ActorId -> PositionIndex
addActorToDynamicIndex level =
    addActorToIndex level.positionIndices.dynamic


getActorType : Actor -> ActorType
getActorType actor =
    let
        hasUpdateableComponents =
            List.any
                (\component ->
                    case component of
                        Actor.AiComponent _ ->
                            True

                        Actor.CameraComponent _ ->
                            True

                        Actor.CollectorComponent _ ->
                            True

                        Actor.ControlComponent _ ->
                            True

                        Actor.CounterComponent _ ->
                            True

                        Actor.DamageComponent _ ->
                            True

                        Actor.DownSmashComponent _ ->
                            True

                        Actor.LifetimeComponent _ ->
                            True

                        Actor.SpawnComponent _ ->
                            True

                        Actor.MovementComponent _ ->
                            True

                        Actor.TriggerExplodableComponent _ ->
                            True

                        _ ->
                            False
                )
                (Dict.values actor.components)
    in
    case hasUpdateableComponents of
        True ->
            DynamicActor

        False ->
            StaticActor



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
    List.append
        (getStaticActorIdsByXY x y level)
        (getDynamicActorIdsByXY x y level)


getStaticActorIdsByXY : Int -> Int -> Level -> List ActorId
getStaticActorIdsByXY x y level =
    Util.dictGetWithDefault level.positionIndices.static ( x, y ) []


getDynamicActorIdsByXY : Int -> Int -> Level -> List ActorId
getDynamicActorIdsByXY x y level =
    Util.dictGetWithDefault level.positionIndices.dynamic ( x, y ) []


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
        (flip getActorIdsByPosition level)
        [ position
        , Position.addPosition position <| Position.getOffsetFromDirection Direction.Left
        , Position.addPosition position <| Position.getOffsetFromDirection Direction.Up
        , Position.addPosition position <| Position.getOffsetFromDirection Direction.Right
        , Position.addPosition position <| Position.getOffsetFromDirection Direction.Down
        ]
        |> Util.fastConcat
        |> List.map (\actorId -> getActorById actorId level)
        |> Maybe.Extra.values
        |> List.filter
            (\actor ->
                Util.lazyAny
                    [ \() ->
                        getTransformComponent actor
                            |> Maybe.Extra.filter (\transformData -> transformData.position == position)
                            |> Maybe.Extra.isJust
                    , \() ->
                        getMovementComponent actor
                            |> Maybe.andThen getMovingTowardsData
                            |> Maybe.Extra.filter (\movementData -> movementData.position == position)
                            |> Maybe.Extra.isJust
                    ]
            )


isEmpty : Position -> Level -> Bool
isEmpty position level =
    getActorsThatAffect position level
        |> List.isEmpty


isNotEmpty : Position -> Level -> Bool
isNotEmpty position level =
    isEmpty position level
        |> not


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



{-

   MovementComponent

-}


getMovementComponent : Actor -> Maybe MovementComponentData
getMovementComponent actor =
    Dict.get "movement" actor.components
        |> Maybe.andThen
            (\component ->
                case component of
                    MovementComponent data ->
                        Just data

                    _ ->
                        Nothing
            )


getMovingTowardsData : MovementComponentData -> Maybe MovingTowardsData
getMovingTowardsData movementData =
    case movementData.movingState of
        MovingTowards towardsData ->
            Just towardsData

        NotMoving ->
            Nothing



{-

   CameraComponent

-}


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
