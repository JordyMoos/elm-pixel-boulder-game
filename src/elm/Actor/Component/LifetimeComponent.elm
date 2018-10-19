module Actor.Component.LifetimeComponent exposing (updateLifetimeComponent)

import Actor.Actor as Actor
    exposing
        ( Actor
        , Component(..)
        , Level
        , LifetimeComponentData
        )
import Actor.Common as Common
import Dict


updateLifetimeComponent : LifetimeComponentData -> Actor -> Level -> Level
updateLifetimeComponent lifetimeData actor level =
    if lifetimeData.remainingTicks > 0 then
        Dict.insert
            "lifetime"
            (LifetimeComponent { lifetimeData | remainingTicks = lifetimeData.remainingTicks - 1 })
            actor.components
            |> Common.updateComponents actor
            |> Common.updateActor level.actors
            |> Common.updateActors level

    else
        Common.removeActor actor level
