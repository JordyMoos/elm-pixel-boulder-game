module Actor.Common exposing
    ( addActor
    , addActorToIndex
    , addEvent
    , getActorById
    , getActorIdsByPosition
    , getActorIdsByXY
    , getActorsByPosition
    , getActorsThatAffect
    , getActorsThatAffectNeighborPosition
    , getCameraComponent
    , getPosition
    , getTagComponent
    , getTransformComponent
    , isDestinationEmpty
    , isDestinationEmptyByOffset
    , isEmpty
    , isNotEmpty
    , removeActor
    , removeActorFromIndexByPosition
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
        , Actors
        , Component(..)
        , Components
        , Event
        , EventManager
        , Events
        , Level
        , MovingState(..)
        , MovingTowardsData
        , PositionIndex
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
        |> removeActorFromIndex actor
        |> removeActorFromDict actor.id
        |> addEvent (Actor.ActorRemoved actor)


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
                            updateViewCoordinate
                                { x = (transform.position.x - round (toFloat level.view.width / 2)) * level.view.pixelSize
                                , y = (transform.position.y - round (toFloat level.view.height / 2)) * level.view.pixelSize
                                }
                                level.view
                                |> (\b a -> setView a b) level
                                |> Just
                        )
                    |> Maybe.andThen
                        (\updatedLevel ->
                            Just ( updatedLevel, actor )
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
        (flip getActorIdsByPosition level)
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
