module Actor.Component.CollectorComponent exposing (updateCollectorComponent)

import Actor.Actor as Actor
    exposing
        ( Actor
        , CollectibleComponentData
        , CollectorComponentData
        , Components
        , Level
        )
import Actor.Common as Common
import Actor.Component.CollectibleComponent as CollectibleComponent
import Dict
import Maybe.Extra


updateCollectorComponent : CollectorComponentData -> Actor -> Level -> Level
updateCollectorComponent collectorData collectorActor level =
    Common.getTransformComponent collectorActor
        |> Maybe.Extra.toList
        |> List.map .position
        |> List.concatMap
            (\position ->
                Common.getActorsByPosition position level
                    |> List.map
                        (\actor ->
                            ( position, actor )
                        )
            )
        |> List.filterMap
            (\( position, actor ) ->
                CollectibleComponent.getCollectibleComponent actor
                    |> Maybe.andThen
                        (\collectibleData ->
                            Just ( position, actor, collectibleData )
                        )
            )
        |> List.filter
            (\( position, actor, collectibleData ) ->
                canCollect collectibleData collectorData.interestedIn
            )
        |> List.foldr
            (\( position, actor, collectibleData ) level ->
                updateCollectorComponentData collectorData collectibleData
                    |> (\newCollectorComponentData ->
                            setCollectorComponent collectorActor.components newCollectorComponentData
                                |> Common.updateComponents collectorActor
                                |> Common.updateActor level.actors
                                |> Common.updateActors level
                                |> Common.removeActor actor
                                |> Common.addEvent (Actor.InventoryUpdated newCollectorComponentData.inventory)
                       )
            )
            level


updateCollectorComponentData : CollectorComponentData -> CollectibleComponentData -> CollectorComponentData
updateCollectorComponentData collector collectible =
    collector.inventory
        |> Dict.update
            collectible.name
            (\maybeCurrentQuantity ->
                Just <| Maybe.withDefault 0 maybeCurrentQuantity + collectible.quantity
            )
        |> updateCollectorInventory collector


updateCollectorInventory : CollectorComponentData -> Actor.Inventory -> CollectorComponentData
updateCollectorInventory collector newInventory =
    { collector | inventory = newInventory }


setCollectorComponent : Components -> CollectorComponentData -> Components
setCollectorComponent components collectorData =
    Dict.insert
        "collector"
        (Actor.CollectorComponent collectorData)
        components


getCollectibleDataIfCanCollect : Actor -> List String -> Maybe CollectibleComponentData
getCollectibleDataIfCanCollect targetActor interestedIn =
    CollectibleComponent.getCollectibleComponent targetActor
        |> Maybe.Extra.filter
            (\collectibleData ->
                List.member collectibleData.name interestedIn
            )


canCollect : CollectibleComponentData -> List String -> Bool
canCollect collectibleData =
    List.member collectibleData.name
