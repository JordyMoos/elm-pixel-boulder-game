module Actor.Component.AttackComponet exposing (getAttackComponent)

import Actor.Actor as Actor exposing (Actor, AttackComponentData, Component(AttackComponent))
import Dict


getAttackComponent : Actor -> Maybe AttackComponentData
getAttackComponent actor =
    Dict.get "health" actor.components
        |> Maybe.andThen
            (\component ->
                case component of
                    AttackComponent data ->
                        Just data

                    _ ->
                        Nothing
            )
