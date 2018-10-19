module Actor.Component.DownSmashComponent exposing (updateDownSmashComponent)

import Actor.Actor as Actor
    exposing
        ( Actor
        , Component(..)
        , DownSmashComponentData
        , Level
        , MovingDownState(..)
        )
import Actor.Cheats as Cheats
import Actor.Common as Common
import Actor.Component.ExplodableComponent as Explodable
import Actor.Component.TransformComponent as Transform
import Data.Direction as Direction exposing (Direction)
import Data.Position as Position exposing (Position)
import Dict


updateDownSmashComponent : DownSmashComponentData -> Actor -> Level -> Level
updateDownSmashComponent downSmashData actor level =
    Common.getTransformComponent actor
        |> Maybe.andThen
            (\transformData ->
                Just ( transformData.position, Transform.isMovingDown transformData )
            )
        |> Maybe.andThen
            (\( position, movingDown ) ->
                (case ( movingDown, downSmashData.movingDownState ) of
                    ( False, IsMovingDown _ ) ->
                        Common.getActorsThatAffect
                            (Position.addPosition position <| Position.getOffsetFromDirection Direction.Down)
                            level
                            |> List.filter Explodable.hasExplodableComponent
                            |> List.foldr
                                (\downActor accLevel ->
                                    accLevel
                                        |> Cheats.addBigExplosion (Position.addPosition position <| Position.getOffsetFromDirection Direction.Down)
                                        |> Common.removeActor downActor
                                )
                                level

                    _ ->
                        level
                )
                    -- Update the WasMovingDown
                    |> (\accLevel ->
                            updateMovingDownState movingDown downSmashData
                                |> updateDownSmash
                                    actor
                                    accLevel
                       )
                    |> Just
            )
        |> Maybe.withDefault level


updateMovingDownState : Bool -> DownSmashComponentData -> DownSmashComponentData
updateMovingDownState movingDown downSmashData =
    case movingDown of
        True ->
            { downSmashData | movingDownState = IsMovingDown 3 }

        False ->
            { downSmashData | movingDownState = lowerMovingDownCount downSmashData.movingDownState }


lowerMovingDownCount : MovingDownState -> MovingDownState
lowerMovingDownCount movingDownState =
    case movingDownState of
        IsMovingDown 0 ->
            NotMovingDown

        IsMovingDown x ->
            IsMovingDown (x - 1)

        NotMovingDown ->
            NotMovingDown


updateDownSmash : Actor -> Level -> DownSmashComponentData -> Level
updateDownSmash actor level downSmashData =
    Dict.insert
        "downsmash"
        (DownSmashComponent downSmashData)
        actor.components
        |> Common.updateComponents actor
        |> Common.updateActor level.actors
        |> Common.updateActors level
