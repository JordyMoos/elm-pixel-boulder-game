module Actor.Component.SpawnComponent exposing (updateSpawnComponent)

import Actor.Actor as Actor
    exposing
        ( Actor
        , Level
        , Entities
        , Component(SpawnComponent)
        , SpawnComponentData
        , SpawnRepeat
        , SpawnRepeatTimes(..)
        )
import Actor.Common as Common
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
        |> (\actor ->
                ( actor
                , Common.updateActor level.actors actor
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
                        (Dict.insert
                            "transform"
                            (Actor.TransformComponent { position = data.position, movingState = Actor.NotMoving })
                            entity
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
                    |> flip spawnUpdateRepeatTimes data.repeat
                    |> flip spawnUpdateRepeat data
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
