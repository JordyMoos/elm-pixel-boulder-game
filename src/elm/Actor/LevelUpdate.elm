module Actor.LevelUpdate exposing (update)

import Actor.Actor as Actor exposing (Actor, ActorId, Component, Level, LevelConfig)
import Actor.Common as Common
import Actor.Component.AiComponent as Ai
import Actor.Component.AreaComponent as AreaComponent
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
import Dict
import InputController


update : Int -> InputController.Model -> Level -> LevelConfig -> Level
update currentTick controllerInput levelBeforeUpdate levelConfig =
    let
        view =
            levelBeforeUpdate.view

        currentYPosition =
            Coordinate.pixelToTile view.pixelSize view.coordinate.y

        currentXPosition =
            Coordinate.pixelToTile view.pixelSize view.coordinate.x

        preparedUpdateActorsAtPosition =
            updateActorsAtPosition currentTick levelConfig levelBeforeUpdate controllerInput

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


updateActorsAtPosition : Int -> LevelConfig -> Level -> InputController.Model -> Int -> Int -> Level -> Level
updateActorsAtPosition currentTick levelConfig levelBeforeUpdate controllerInput x y level =
    let
        preparedUpdateActorById =
            updateActorById currentTick levelConfig levelBeforeUpdate controllerInput
    in
    Common.getDynamicActorIdsByXY x y levelBeforeUpdate
        |> List.foldr
            (\actorId accLevel -> preparedUpdateActorById accLevel actorId)
            level


updateActorById : Int -> LevelConfig -> Level -> InputController.Model -> Level -> ActorId -> Level
updateActorById currentTick levelConfig levelBeforeUpdate controllerInput level actorId =
    let
        preparedUpdateComponent =
            updateComponent currentTick levelConfig levelBeforeUpdate controllerInput
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


updateComponent : Int -> LevelConfig -> Level -> InputController.Model -> Component -> Level -> Actor -> Level
updateComponent currentTick levelConfig levelBeforeUpdate controllerInput component level actor =
    case component of
        Actor.AiComponent aiData ->
            Ai.updateAiComponent aiData actor levelConfig.entities levelBeforeUpdate level

        Actor.AreaComponent area ->
            AreaComponent.updateAreaComponent currentTick area actor level

        Actor.CameraComponent camera ->
            Camera.updateCameraComponent camera actor level

        Actor.CollectorComponent data ->
            Collector.updateCollectorComponent data actor level

        Actor.ControlComponent control ->
            Control.updateControlComponent currentTick controllerInput control actor level

        Actor.CounterComponent counterData ->
            Counter.updateCounterComponent counterData actor level

        Actor.DamageComponent damageData ->
            Damage.updateDamageComponent damageData actor level

        Actor.DownSmashComponent downSmash ->
            DownSmash.updateDownSmashComponent downSmash actor level

        Actor.LifetimeComponent lifetimeData ->
            Lifetime.updateLifetimeComponent lifetimeData actor levelConfig.entities level

        Actor.SpawnComponent spawnData ->
            Spawn.updateSpawnComponent levelConfig.entities spawnData actor level

        Actor.MovementComponent movementData ->
            Movement.updateMovementComponent currentTick movementData actor level

        Actor.TriggerExplodableComponent triggerData ->
            TriggerExplodable.updateTriggerExplodableComponent triggerData actor level

        _ ->
            level
