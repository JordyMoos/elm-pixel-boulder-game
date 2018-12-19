module Actor.Component.TagComponent exposing (isTag)

import Actor.Actor as Actor
    exposing
        ( Actor
        , Component(..)
        , TagComponentData
        )
import Actor.Common as Common
import Dict
import Maybe.Extra


isTag : String -> Actor -> Bool
isTag tag actor =
    Common.getTagComponent actor
        |> Maybe.map .name
        |> Maybe.Extra.filter ((==) tag)
        |> Maybe.Extra.isJust
