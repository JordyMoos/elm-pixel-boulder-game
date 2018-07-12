module Actor.Component.RenderComponent exposing (RenderComponentData)

import Color exposing (Color)


type alias RenderComponentData =
    { colors : List Color
    , ticksPerColor : Int
    }
