module Actor.LevelUpdate exposing (update)

import Actor.Actor as Actor
import Actor.Common as Common
import Actor.Component.AiComponent as Ai
import Actor.Component.CameraComponent as Camera
import Actor.Component.CollectorComponent as Collector
import Actor.Component.ControlComponent as Control
import Actor.Component.CounterComponent as Counter
import Actor.Component.DamageComponent as Damage
import Actor.Component.DownSmashComponent as DownSmash
import Actor.Component.LifetimeComponent as Lifetime
import Actor.Component.SpawnComponent as Spawn
import Actor.Component.TransformComponent as Transform
import Actor.Component.TriggerExplodableComponent as TriggerExplodable
import Data.Coordinate as Coordinate
import Data.Direction exposing (Direction)
import Dict


update : Maybe Direction -> Actor.Level -> Actor.LevelConfig -> Actor.Level
update maybeDirection levelBeforeUpdate levelConfig =
    let
        view =
            levelBeforeUpdate.view

        xPosition =
            Coordinate.pixelToTile view.pixelSize view.coordinate.x

        yPosition =
            Coordinate.pixelToTile view.pixelSize view.coordinate.y
    in
    List.foldr
        (\y levelA ->
            List.foldr
                (\x levelB ->
                    Common.getActorIdsByXY x y levelBeforeUpdate
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

                                                                            Actor.CounterComponent counterData ->
                                                                                Counter.updateCounterComponent counterData updatedActor levelD

                                                                            Actor.DamageComponent damageData ->
                                                                                Damage.updateDamageComponent damageData updatedActor levelD

                                                                            Actor.TriggerExplodableComponent triggerData ->
                                                                                TriggerExplodable.updateTriggerExplodableComponent triggerData updatedActor levelD

                                                                            Actor.SpawnComponent spawnData ->
                                                                                Spawn.updateSpawnComponent levelConfig.entities spawnData updatedActor levelD

                                                                            Actor.AiComponent aiData ->
                                                                                Ai.updateAiComponent aiData updatedActor levelConfig.entities levelBeforeUpdate levelD

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
                (List.range (xPosition - levelConfig.updateBorder) (xPosition + view.width + levelConfig.updateBorder))
        )
        levelBeforeUpdate
        (List.range (yPosition - levelConfig.updateBorder) (yPosition + view.height + levelConfig.updateBorder))
