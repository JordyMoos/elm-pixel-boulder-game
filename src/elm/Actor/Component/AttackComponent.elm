module Actor.Component.AttackComponent exposing (getAttackComponent)

import Actor.Actor as Actor
    exposing
        ( Actor
        , AttackComponentData
        , Component(..)
        )
import Dict


getAttackComponent : Actor -> Maybe AttackComponentData
getAttackComponent actor =
    Dict.get "attack" actor.components
        |> Maybe.andThen
            (\component ->
                case component of
                    AttackComponent data ->
                        Just data

                    _ ->
                        Nothing
            )
