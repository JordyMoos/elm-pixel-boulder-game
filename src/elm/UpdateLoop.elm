module UpdateLoop exposing (update)

import Actor.Actor as Actor exposing (Level)
import Dict exposing (Dict)
import InputController
import Data.Common exposing (Position, Direction)


updateBorder : Int
updateBorder =
    5


update : Maybe Direction -> Level -> Level
update maybeDirection level =
    List.foldr
        (\y level ->
            List.foldr
                (\x level ->
                    Actor.getActorIdsByXY x y level
                        |> List.foldr
                            (\actorId level ->
                                Actor.getActorById actorId level
                                    |> Maybe.andThen
                                        (\actor ->
                                            Dict.foldr
                                                (\_ component level ->
                                                    Actor.getActorById actorId level
                                                        |> Maybe.andThen
                                                            (\actor ->
                                                                let
                                                                    updatedLevel =
                                                                        case component of
                                                                            Actor.PlayerInputComponent ->
                                                                                Actor.updatePlayerInputComponent maybeDirection actor level

                                                                            Actor.TransformComponent transformData ->
                                                                                Actor.updateTransformComponent transformData actor level

                                                                            Actor.DiamondCollectorComponent ->
                                                                                Actor.updateDiamondCollectorComponent actor level

                                                                            Actor.CanSquashComponent ->
                                                                                Actor.updateCanSquashComponent actor level

                                                                            Actor.PhysicsComponent physics ->
                                                                                Actor.updatePhysicsComponent physics actor level

                                                                            Actor.AiComponent ai ->
                                                                                Actor.updateAiComponent ai actor level

                                                                            Actor.CameraComponent camera ->
                                                                                Actor.updateCameraComponent camera actor level

                                                                            Actor.DownSmashComponent downSmash ->
                                                                                Actor.updateDownSmashComponent downSmash actor level

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
                (List.range (level.view.position.x - updateBorder) (level.view.position.x + level.view.width + updateBorder))
        )
        level
        (List.range (level.view.position.y - updateBorder) (level.view.position.y + level.view.height + updateBorder))
