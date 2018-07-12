module UpdateLoop exposing (update)

import Data.Level exposing (Level, View)
import Actor.Actor as Actor
import Dict exposing (Dict)
import InputController exposing (Direction)


updateBorder : Int
updateBorder =
    5


update : Direction -> Level -> Level
update direction level =
    List.foldr
        (\y level ->
            List.foldr
                (\x level ->
                    Actor.getIdsByXY x y level
                        |> List.foldr
                            (\actorId level ->
                                Actor.getById actorId level
                                    |> Maybe.andThen
                                        (\actor ->
                                            Dict.foldr
                                                (\_ component level ->
                                                    Actor.getById actorId level
                                                        |> Maybe.andThen
                                                            (\actor ->
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
                                                                                        handleMovingTowards currentTick transformData movingData actor level

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

                                                                            DownSmashComponent downSmash ->
                                                                                tryDownSmash level actor downSmash

                                                                            DamageComponent damageData ->
                                                                                handleDamageComponent actor damageData level

                                                                            _ ->
                                                                                level
                                                                in
                                                                    Just updatedLevel
                                                            )
                                                        |> Maybe.withDefault level
                                                )
                                                level
                                                actor.components
                                                |> Just
                                        )
                                    |> Maybe.withDefault level
                            )
                            level
                )
                level
                (List.range (view.position.x - updateBorder) (view.position.x + view.width + updateBorder))
        )
        model.level
        (List.range (view.position.y - updateBorder) (view.position.y + view.height + updateBorder))
