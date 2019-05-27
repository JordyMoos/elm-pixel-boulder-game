module Actor.Component.LifetimeComponent exposing (updateLifetimeComponent)

import Actor.Actor as Actor
    exposing
        ( Actor
        , BecomeActorLifetimeActionData
        , Component(..)
        , Components
        , Entities
        , Level
        , LifetimeAction(..)
        , LifetimeComponentData
        , TransformComponentData
        )
import Actor.Common as Common
import Dict
import Pilf


updateLifetimeComponent : LifetimeComponentData -> Actor -> Entities -> Level -> Level
updateLifetimeComponent lifetimeData actor entities level =
    if lifetimeData.remainingTicks > 0 then
        Dict.insert
            "lifetime"
            (LifetimeComponent { lifetimeData | remainingTicks = lifetimeData.remainingTicks - 1 })
            actor.components
            |> Common.updateComponents actor
            |> Common.updateActor level.actors
            |> Common.updateActors level

    else
        handleAction lifetimeData.action actor entities level


handleAction : LifetimeAction -> Actor -> Entities -> Level -> Level
handleAction action actor entities level =
    case action of
        RemoveActorLifetimeAction ->
            handleRemove actor level

        BecomeActorLifetimeAction data ->
            level
                |> handleRemove actor
                |> handleBecome data actor entities


handleRemove : Actor -> Level -> Level
handleRemove =
    Common.removeActor


type alias RequiredBecomeData =
    { transform : TransformComponentData
    , components : Components
    }


handleBecome : BecomeActorLifetimeActionData -> Actor -> Entities -> Level -> Level
handleBecome data actor entities level =
    Maybe.map2
        RequiredBecomeData
        (Common.getTransformComponent actor)
        (Dict.get data.entityName entities)
        |> Maybe.map (\requiredData -> Dict.insert "transform" (TransformComponent requiredData.transform) requiredData.components)
        |> Maybe.map (Pilf.flip Common.addActor level)
        |> Maybe.withDefault level
