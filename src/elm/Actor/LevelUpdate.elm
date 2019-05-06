module Actor.LevelUpdate exposing (update)

import Actor.Actor as Actor exposing (Actor, ActorId, Component, Level, LevelConfig)
import Actor.Common as Common
import Actor.Component.AiComponent as Ai
import Actor.Component.CameraComponent as Camera
import Actor.Component.CollectorComponent as Collector
import Actor.Component.ControlComponent as Control
import Actor.Component.CounterComponent as Counter
import Actor.Component.DamageComponent as Damage
import Actor.Component.DownSmashComponent as DownSmash
import Actor.Component.LifetimeComponent as Lifetime
import Actor.Component.MovementComponent as Movement
import Actor.Component.SpawnComponent as Spawn
import Actor.Component.TriggerExplodableComponent as TriggerExplodable
import Data.Coordinate as Coordinate
import Data.Direction exposing (Direction)
import Dict


update : Maybe Direction -> Level -> LevelConfig -> Level
update controllerInput levelBeforeUpdate levelConfig =
    let
        view =
            levelBeforeUpdate.view

        currentYPosition =
            Coordinate.pixelToTile view.pixelSize view.coordinate.y

        currentXPosition =
            Coordinate.pixelToTile view.pixelSize view.coordinate.x

        preparedUpdateActorsAtPosition =
            updateActorsAtPosition levelConfig levelBeforeUpdate controllerInput

        yPositions =
            List.range
                (currentYPosition - levelConfig.updateBorder)
                (currentYPosition + view.height + levelConfig.updateBorder)

        xPositions =
            List.range
                (currentXPosition - levelConfig.updateBorder)
                (currentXPosition + view.width + levelConfig.updateBorder)
    in
    List.foldl
        (\y levelA ->
            List.foldl
                (\x levelB -> preparedUpdateActorsAtPosition x y levelB)
                levelA
                xPositions
        )
        levelBeforeUpdate
        yPositions


updateActorsAtPosition : LevelConfig -> Level -> Maybe Direction -> Int -> Int -> Level -> Level
updateActorsAtPosition levelConfig levelBeforeUpdate controllerInput x y level =
    let
        preparedUpdateActorById =
            updateActorById levelConfig levelBeforeUpdate controllerInput
    in
    Common.getDynamicActorIdsByXY x y levelBeforeUpdate
        |> List.foldr
            (\actorId accLevel -> preparedUpdateActorById accLevel actorId)
            level


updateActorById : LevelConfig -> Level -> Maybe Direction -> Level -> ActorId -> Level
updateActorById levelConfig levelBeforeUpdate controllerInput level actorId =
    let
        preparedUpdateComponent =
            updateComponent levelConfig levelBeforeUpdate controllerInput
    in
    Common.getActorById actorId level
        |> Maybe.map
            (\actor ->
                Dict.foldr
                    (\_ component accLevel ->
                        Common.getActorById actorId accLevel
                            |> Maybe.map (preparedUpdateComponent component accLevel)
                            |> Maybe.withDefault accLevel
                    )
                    level
                    actor.components
            )
        |> Maybe.withDefault level


updateComponent : LevelConfig -> Level -> Maybe Direction -> Component -> Level -> Actor -> Level
updateComponent levelConfig levelBeforeUpdate controllerInput component level actor =
    case component of
        Actor.AiComponent aiData ->
            Ai.updateAiComponent aiData actor levelConfig.entities levelBeforeUpdate level

        Actor.CameraComponent camera ->
            Camera.updateCameraComponent camera actor level

        Actor.CollectorComponent data ->
            Collector.updateCollectorComponent data actor level

        Actor.ControlComponent control ->
            Control.updateControlComponent controllerInput control actor level

        Actor.CounterComponent counterData ->
            Counter.updateCounterComponent counterData actor level

        Actor.DamageComponent damageData ->
            Damage.updateDamageComponent damageData actor level

        Actor.DownSmashComponent downSmash ->
            DownSmash.updateDownSmashComponent downSmash actor level

        Actor.LifetimeComponent lifetimeData ->
            Lifetime.updateLifetimeComponent lifetimeData actor level

        Actor.SpawnComponent spawnData ->
            Spawn.updateSpawnComponent levelConfig.entities spawnData actor level

        Actor.MovementComponent movementData ->
            Movement.updateMovementComponent movementData actor level

        Actor.TriggerExplodableComponent triggerData ->
            TriggerExplodable.updateTriggerExplodableComponent triggerData actor level

        _ ->
            level
