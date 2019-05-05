module Actor.Component.SpawnComponent exposing (updateSpawnComponent)

import Actor.Actor as Actor
    exposing
        ( Actor
        , Component(..)
        , Entities
        , Level
        , SpawnComponentData
        , SpawnRepeat
        , SpawnRepeatTimes(..)
        )
import Actor.Common as Common
import Actor.Component.MovementComponent as MovementComponent
import Dict


updateSpawnComponent : Entities -> SpawnComponentData -> Actor -> Level -> Level
updateSpawnComponent entities data actor level =
    if data.delayTicks > 0 then
        spawnDecrementDelayTicks data
            |> setSpawnComponentData actor level
            |> Tuple.second

    else
        spawnActor entities data level
            |> updateSpawnComponentAfterSpawn data actor


setSpawnComponentData : Actor -> Level -> SpawnComponentData -> ( Actor, Level )
setSpawnComponentData actor level data =
    data
        |> SpawnComponent
        |> (\component ->
                Dict.insert "spawn" component actor.components
           )
        |> Common.updateComponents actor
        |> (\updatedActor ->
                ( updatedActor
                , Common.updateActor level.actors updatedActor
                    |> Common.updateActors level
                )
           )


spawnActor : Entities -> SpawnComponentData -> Level -> Level
spawnActor entities data level =
    if Common.getActorsThatAffect data.position level |> List.isEmpty then
        Dict.get
            data.entityName
            entities
            |> Maybe.map
                (\entity ->
                    Common.addActor
                        (entity
                            |> Dict.insert
                                "transform"
                                (Actor.TransformComponent { position = data.position })
                            -- @todo cheats. We should configure the speed in the spawn component
                            |> Dict.insert
                                "movement"
                                (Actor.MovementComponent <| MovementComponent.init 4)
                        )
                        level
                )
            |> Maybe.withDefault level

    else
        level


updateSpawnComponentAfterSpawn : SpawnComponentData -> Actor -> Level -> Level
updateSpawnComponentAfterSpawn data actor level =
    case data.repeat.times of
        RepeatNever ->
            removeSpawnComponent actor level

        RepeatForever ->
            spawnResetDelayTicks data
                |> setSpawnComponentData actor level
                |> Tuple.second

        RepeatTimes count ->
            if count > 0 then
                RepeatTimes (count - 1)
                    |> (\a -> spawnUpdateRepeatTimes a data.repeat)
                    |> (\a -> spawnUpdateRepeat a data)
                    |> spawnResetDelayTicks
                    |> setSpawnComponentData actor level
                    |> Tuple.second

            else
                removeSpawnComponent actor level


removeSpawnComponent : Actor -> Level -> Level
removeSpawnComponent actor level =
    Dict.remove "spawn" actor.components
        |> Common.updateComponents actor
        |> Common.updateActor level.actors
        |> Common.updateActors level


spawnUpdateRepeatTimes : SpawnRepeatTimes -> SpawnRepeat -> SpawnRepeat
spawnUpdateRepeatTimes times repeat =
    { repeat | times = times }


spawnUpdateRepeat : SpawnRepeat -> SpawnComponentData -> SpawnComponentData
spawnUpdateRepeat repeat data =
    { data | repeat = repeat }


spawnDecrementDelayTicks : SpawnComponentData -> SpawnComponentData
spawnDecrementDelayTicks data =
    { data | delayTicks = data.delayTicks - 1 }


spawnResetDelayTicks : SpawnComponentData -> SpawnComponentData
spawnResetDelayTicks data =
    { data | delayTicks = data.repeat.delayTicks }
