module Actor.Component.DamageComponent exposing (updateDamageComponent)

import Actor.Actor as Actor exposing (Actor, DamageComponentData, Level)
import Actor.Common as Common
import Actor.Component.PhysicsComponent as Physics
import Maybe.Extra
import Util.Util as Util


updateDamageComponent : DamageComponentData -> Actor -> Level -> Level
updateDamageComponent damageData damageDealingActor level =
    Common.getTransformComponent damageDealingActor
        |> Maybe.Extra.toList
        |> Util.fastConcatMap
            (\transformData ->
                Common.getActorsThatAffect transformData.position level
            )
        |> List.filter (\actor -> actor.id /= damageDealingActor.id)
        |> List.filter
            (\actor ->
                Physics.getPhysicsComponent actor
                    |> Maybe.map (\physics -> physics.strength < damageData.damageStrength)
                    |> Maybe.withDefault True
            )
        |> List.foldr
            (\actor accLevel ->
                Common.removeActor actor accLevel
            )
            level
