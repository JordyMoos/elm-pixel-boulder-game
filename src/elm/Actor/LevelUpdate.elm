module Actor.LevelUpdate exposing (update)

import Actor.Actor as Actor
import Actor.Common as Common
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
update maybeDirection level levelConfig =
    List.foldr
        (\y level ->
            List.foldr
                (\x level ->
                    Common.getActorIdsByXY x y level
                        |> List.foldr
                            (\actorId level ->
                                Common.getActorById actorId level
                                    |> Maybe.andThen
                                        (\actor ->
                                            Dict.foldr
                                                (\_ component level ->
                                                    Common.getActorById actorId level
                                                        |> Maybe.andThen
                                                            (\actor ->
                                                                let
                                                                    updatedLevel =
                                                                        case component of
                                                                            Actor.TransformComponent transformData ->
                                                                                Transform.updateTransformComponent transformData actor level

                                                                            Actor.CollectorComponent data ->
                                                                                Collector.updateCollectorComponent data actor level

                                                                            Actor.ControlComponent control ->
                                                                                Control.updateControlComponent maybeDirection control actor level

                                                                            Actor.CameraComponent camera ->
                                                                                Camera.updateCameraComponent camera actor level

                                                                            Actor.DownSmashComponent downSmash ->
                                                                                DownSmash.updateDownSmashComponent downSmash actor level

                                                                            Actor.LifetimeComponent lifetimeData ->
                                                                                Lifetime.updateLifetimeComponent lifetimeData actor level

                                                                            Actor.DamageComponent damageData ->
                                                                                Damage.updateDamageComponent damageData actor level

                                                                            Actor.TriggerExplodableComponent triggerData ->
                                                                                TriggerExplodable.updateTriggerExplodableComponent triggerData actor level

                                                                            Actor.SpawnComponent spawnData ->
                                                                                Spawn.updateSpawnComponent levelConfig.entities spawnData actor level

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
