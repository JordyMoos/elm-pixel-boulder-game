module Actor.LevelUpdate exposing (update)

import Actor.Actor as Actor
import Actor.Common as Common
import Actor.Component.AiComponent as Ai
import Actor.Component.CameraComponent as Camera
import Actor.Component.CollectorComponent as Collector
import Actor.Component.ControlComponent as Control
import Actor.Component.DamageComponent as Damage
import Actor.Component.DownSmashComponent as DownSmash
import Actor.Component.LifetimeComponent as Lifetime
import Actor.Component.SpawnComponent as Spawn
import Actor.Component.TransformComponent as Transform
import Actor.Component.TriggerExplodableComponent as TriggerExplodable
import Data.Direction exposing (Direction)
import Dict


updateBorder : Int
updateBorder =
    5


update : Maybe Direction -> Actor.Level -> Actor.LevelConfig -> Actor.Level
update maybeDirection levelBeforeUpdate levelConfig =
    List.foldr
        (\y levelA ->
            List.foldr
                (\x levelB ->
                    Common.getActorIdsByXY x y levelB
                        |> List.foldr
                            (\actorId levelC ->
                                Common.getActorById actorId levelC
                                    |> Maybe.andThen
                                        (\actor ->
                                            Dict.foldr
                                                (\_ component levelD ->
                                                    Common.getActorById actorId levelD
                                                        |> Maybe.andThen
                                                            (\updatedActor ->
                                                                let
                                                                    updatedLevel =
                                                                        case component of
                                                                            Actor.TransformComponent transformData ->
                                                                                Transform.updateTransformComponent transformData updatedActor levelD

                                                                            Actor.CollectorComponent data ->
                                                                                Collector.updateCollectorComponent data updatedActor levelD

                                                                            Actor.ControlComponent control ->
                                                                                Control.updateControlComponent maybeDirection control updatedActor levelD

                                                                            Actor.CameraComponent camera ->
                                                                                Camera.updateCameraComponent camera updatedActor levelD

                                                                            Actor.DownSmashComponent downSmash ->
                                                                                DownSmash.updateDownSmashComponent downSmash updatedActor levelD

                                                                            Actor.LifetimeComponent lifetimeData ->
                                                                                Lifetime.updateLifetimeComponent lifetimeData updatedActor levelD

                                                                            Actor.DamageComponent damageData ->
                                                                                Damage.updateDamageComponent damageData updatedActor levelD

                                                                            Actor.TriggerExplodableComponent triggerData ->
                                                                                TriggerExplodable.updateTriggerExplodableComponent triggerData updatedActor levelD

                                                                            Actor.SpawnComponent spawnData ->
                                                                                Spawn.updateSpawnComponent levelConfig.entities spawnData updatedActor levelD

                                                                            Actor.AiComponent aiData ->
                                                                                Ai.updateAiComponent aiData updatedActor levelBeforeUpdate levelD

                                                                            _ ->
                                                                                levelD
                                                                in
                                                                Just updatedLevel
                                                            )
                                                        |> Maybe.withDefault levelD
                                                )
                                                levelC
                                                actor.components
                                                |> Just
                                        )
                                    |> Maybe.withDefault levelC
                            )
                            levelB
                )
                levelA
                (List.range (levelA.view.position.x - updateBorder) (levelA.view.position.x + levelA.view.width + updateBorder))
        )
        levelBeforeUpdate
        (List.range (levelBeforeUpdate.view.position.y - updateBorder) (levelBeforeUpdate.view.position.y + levelBeforeUpdate.view.height + updateBorder))
