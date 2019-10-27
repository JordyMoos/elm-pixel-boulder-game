module Actor.Component.TriggerComponent exposing (getTriggerComponent)

import Actor.Actor as Actor exposing (Actor, Component(..), TriggerComponentData)
import Dict


getTriggerComponent : Actor -> Maybe TriggerComponentData
getTriggerComponent actor =
    Dict.get "trigger" actor.components
        |> Maybe.andThen
            (\component ->
                case component of
                    TriggerComponent data ->
                        Just data

                    _ ->
                        Nothing
            )
