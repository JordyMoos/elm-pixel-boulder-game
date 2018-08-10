module Actor.DamageComponent exposing (updateDamageComponent)

import Actor.Actor as Actor exposing (Actor, Level, DamageComponentData)
import Actor.Common as Common
import Actor.PhysicsComponent as Physics
import Maybe.Extra


updateDamageComponent : DamageComponentData -> Actor -> Level -> Level
updateDamageComponent damageData damageDealingActor level =
    Common.getTransformComponent damageDealingActor
        |> Maybe.Extra.toList
        |> List.concatMap
            (\transformData ->
                Common.getActorsThatAffect transformData.position level
            )
        |> List.filter
            (\actor ->
                actor.id /= damageDealingActor.id
            )
        |> List.filter
            (\actor ->
                Physics.getPhysicsComponent actor
                    |> Maybe.andThen
                        (\physics ->
                            Just <| physics.strength < damageData.damageStrength
                        )
                    |> Maybe.withDefault True
            )
        |> List.foldr
            (\actor level ->
                Common.removeActor actor level
            )
            level
