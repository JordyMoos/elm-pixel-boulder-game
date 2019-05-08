module Actor.Component.CollectorComponent exposing (tryCollectPosition, updateCollectorComponent)

import Actor.Actor as Actor
    exposing
        ( Actor
        , CollectibleComponentData
        , CollectorComponentData
        , Component(..)
        , Components
        , Level
        )
import Actor.Common as Common
import Actor.Component.CollectibleComponent as CollectibleComponent
import Actor.Component.MovementComponent as MovementComponent
import Data.Position exposing (Position)
import Dict
import Pilf
import Util.Util as Util


updateCollectorComponent : CollectorComponentData -> Actor -> Level -> Level
updateCollectorComponent data actor level =
    Common.getTransformComponent actor
        |> Maybe.map .position
        |> Maybe.map (\position -> collect data actor position level)
        |> Maybe.map Tuple.second
        |> Maybe.withDefault level


getCollectorComponent : Actor -> Maybe CollectorComponentData
getCollectorComponent actor =
    Dict.get "collector" actor.components
        |> Maybe.andThen
            (\component ->
                case component of
                    CollectorComponent data ->
                        Just data

                    _ ->
                        Nothing
            )


tryCollectPosition : Actor -> Position -> Level -> Level
tryCollectPosition actor position level =
    getCollectorComponent actor
        |> Maybe.map (\collectorData -> collect collectorData actor position level)
        |> Maybe.map Tuple.second
        |> Maybe.withDefault level


collect : CollectorComponentData -> Actor -> Position -> Level -> ( Actor, Level )
collect collectorData actor position level =
    let
        collectiblePredicates : ( Actor, CollectibleComponentData ) -> Bool
        collectiblePredicates ( collectibleActor, collectibleData ) =
            Util.lazyAll
                [ \() -> MovementComponent.isActorNotMoving collectibleActor
                , \() -> List.member collectibleData.name collectorData.interestedIn
                ]

        collectibleActors : List ( Actor, CollectibleComponentData )
        collectibleActors =
            Common.getActorsByPosition position level
                |> List.filter (\foundActor -> foundActor.id /= actor.id)
                |> List.filterMap withCollectibleData
                |> List.filter collectiblePredicates

        updatedCollectorData : CollectorComponentData
        updatedCollectorData =
            collectAll collectorData collectibleActors

        updatedCollectorActor : Actor
        updatedCollectorActor =
            actor.components
                |> Dict.insert "collector" (Actor.CollectorComponent updatedCollectorData)
                |> Common.updateComponents actor

        updatedLevel : Level
        updatedLevel =
            updatedCollectorActor
                |> Common.updateActor level.actors
                |> Common.updateActors level
                |> Pilf.flip removeCollectibleActorsFromLevel collectibleActors
                |> Common.addEvent (Actor.InventoryUpdated updatedCollectorData.inventory)
    in
    ( updatedCollectorActor, updatedLevel )


removeCollectibleActorsFromLevel : Level -> List ( Actor, CollectibleComponentData ) -> Level
removeCollectibleActorsFromLevel =
    List.foldl
        (\( actor, _ ) accLevel ->
            Common.removeActor actor accLevel
        )


collectAll : CollectorComponentData -> List ( Actor, CollectibleComponentData ) -> CollectorComponentData
collectAll =
    List.foldl
        (\( _, collectibleData ) collectorData ->
            collectorData.inventory
                |> Dict.update
                    collectibleData.name
                    (\maybeCurrentQuantity ->
                        Just <| Maybe.withDefault 0 maybeCurrentQuantity + collectibleData.quantity
                    )
                |> updateCollectorInventory collectorData
        )


updateCollectorInventory : CollectorComponentData -> Actor.Inventory -> CollectorComponentData
updateCollectorInventory collector newInventory =
    { collector | inventory = newInventory }


withCollectibleData : Actor -> Maybe ( Actor, CollectibleComponentData )
withCollectibleData actor =
    CollectibleComponent.getCollectibleComponent actor
        |> Maybe.map (\collectibleData -> ( actor, collectibleData ))
