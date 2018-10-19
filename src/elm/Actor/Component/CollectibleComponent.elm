module Actor.Component.CollectibleComponent exposing (getCollectibleComponent, hasCollectibleComponent)

import Actor.Actor as Actor
    exposing
        ( Actor
        , CollectibleComponentData
        , Component(..)
        )
import Dict


getCollectibleComponent : Actor -> Maybe CollectibleComponentData
getCollectibleComponent actor =
    Dict.get "collectible" actor.components
        |> Maybe.andThen
            (\component ->
                case component of
                    CollectibleComponent data ->
                        Just data

                    _ ->
                        Nothing
            )


hasCollectibleComponent : Actor -> Bool
hasCollectibleComponent actor =
    Dict.member "collectible" actor.components
