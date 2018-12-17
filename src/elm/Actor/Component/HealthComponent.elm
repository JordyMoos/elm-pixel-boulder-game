module Actor.Component.HealthComponent exposing (getHealthComponent)

import Actor.Actor as Actor exposing (Actor, Component(HealthComponent), HealthComponentData)
import Dict


getHealthComponent : Actor -> Maybe HealthComponentData
getHealthComponent actor =
    Dict.get "health" actor.components
        |> Maybe.andThen
            (\component ->
                case component of
                    HealthComponent data ->
                        Just data

                    _ ->
                        Nothing
            )
