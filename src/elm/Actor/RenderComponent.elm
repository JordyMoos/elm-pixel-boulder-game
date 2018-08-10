module Actor.RenderComponent exposing (getRenderComponent)

import Actor.Actor as Actor
    exposing
        ( Actor
        , Component(RenderComponent)
        , RenderComponentData
        )
import Dict


getRenderComponent : Actor -> Maybe RenderComponentData
getRenderComponent actor =
    Dict.get "render" actor.components
        |> Maybe.andThen
            (\component ->
                case component of
                    RenderComponent data ->
                        Just data

                    _ ->
                        Nothing
            )
