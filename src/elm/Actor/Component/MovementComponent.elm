module Actor.Component.MovementComponent exposing
    ( calculateCompletionPercentage
    , createMovingTowards
    , init
    , isActorMoving
    , isActorNotMoving
    , isMoving
    , isMovingAt
    , isMovingDown
    , isNotMoving
    , isNotMovingAt
    , setMovementData
    , startMovingTowards
    , updateMovementComponent
    )

import Actor.Actor as Actor
    exposing
        ( Actor
        , Component(..)
        , Level
        , MovementComponentData
        , MovingState(..)
        , MovingTowardsData
        )
import Actor.Common as Common
import Data.Direction as Direction exposing (Direction)
import Data.Position as Position exposing (Position)
import Dict
import Maybe.Extra
import Pilf


updateMovementComponent : MovementComponentData -> Actor -> Level -> Level
updateMovementComponent movementData actor level =
    Common.getMovingTowardsData movementData
        |> Maybe.map
            (\towardsData ->
                if towardsData.tickCountLeft > 0 then
                    let
                        newMovementData =
                            { movementData
                                | movingState =
                                    MovingTowards
                                        { towardsData
                                            | tickCountLeft = towardsData.tickCountLeft - 1
                                            , completionPercentage = calculateCompletionPercentage towardsData.totalTickCount towardsData.tickCountLeft
                                        }
                            }
                    in
                    setMovementData newMovementData ( actor, level )

                else
                    -- Finished moving
                    setTransformToPosition towardsData.position ( actor, level )
                        |> setMovementData (notMovingData movementData)
            )
        |> Maybe.map Tuple.second
        |> Maybe.withDefault level


init : Int -> MovementComponentData
init movingTicks =
    { movingTicks = movingTicks
    , movingState = NotMoving
    }


notMovingData : MovementComponentData -> MovementComponentData
notMovingData movementData =
    { movementData | movingState = NotMoving }


setMovementData : MovementComponentData -> ( Actor, Level ) -> ( Actor, Level )
setMovementData movementData ( actor, level ) =
    let
        updatedActor =
            Dict.insert
                "movement"
                (MovementComponent movementData)
                actor.components
                |> Common.updateComponents actor

        updatedLevel =
            updatedActor
                |> Common.updateActor level.actors
                |> Common.updateActors level
    in
    ( updatedActor, updatedLevel )


setTransformToPosition : Position -> ( Actor, Level ) -> ( Actor, Level )
setTransformToPosition position ( actor, level ) =
    let
        updatedActor =
            Dict.insert
                "transform"
                (TransformComponent
                    { position = position
                    }
                )
                actor.components
                |> Common.updateComponents actor

        updatedLevel =
            updatedActor
                |> Common.updateActor level.actors
                |> Common.updateActors level
                |> Common.removeActorFromIndices actor
                |> Common.addActorToIndices position actor
    in
    ( updatedActor, updatedLevel )


calculateCompletionPercentage : Int -> Int -> Float
calculateCompletionPercentage totalTickCount tickCountLeft =
    100 / toFloat totalTickCount * toFloat (totalTickCount - tickCountLeft)


isActorMoving : Actor -> Bool
isActorMoving actor =
    Common.getMovementComponent actor
        |> Maybe.map isMoving
        |> Maybe.withDefault False


isActorNotMoving : Actor -> Bool
isActorNotMoving =
    isActorMoving >> not


isMoving : MovementComponentData -> Bool
isMoving movementData =
    Common.getMovingTowardsData movementData
        |> Maybe.Extra.isJust


isNotMoving : MovementComponentData -> Bool
isNotMoving movementData =
    isMoving movementData
        |> not


isMovingAt : Position -> Level -> Bool
isMovingAt position level =
    Common.getActorsByPosition position level
        |> List.map Common.getMovementComponent
        |> Maybe.Extra.values
        |> List.filter isMoving
        |> List.isEmpty
        |> not


isNotMovingAt : Position -> Level -> Bool
isNotMovingAt position level =
    isMovingAt position level
        |> not


isMovingDown : Actor -> Bool
isMovingDown actor =
    Common.getMovementComponent actor
        |> Maybe.map Common.getMovingTowardsData
        |> Maybe.Extra.join
        |> Maybe.Extra.isJust


startMovingTowards : Actor -> Direction -> Level -> Level
startMovingTowards actor direction level =
    Maybe.map2
        Tuple.pair
        (Common.getTransformComponent actor)
        (Common.getMovementComponent actor)
        |> Maybe.map (\( transformData, movementData ) -> createMovingTowards transformData.position direction movementData)
        |> Maybe.map (Pilf.flip setMovementData ( actor, level ))
        |> Maybe.map Tuple.second
        |> Maybe.withDefault level


createMovingTowards : Position -> Direction -> MovementComponentData -> MovementComponentData
createMovingTowards oldPosition direction movementData =
    { movementData
        | movingState =
            MovingTowards
                { position = Position.addDirection oldPosition direction
                , totalTickCount = movementData.movingTicks
                , tickCountLeft = movementData.movingTicks
                , completionPercentage = 0.0
                , direction = direction
                }
    }
