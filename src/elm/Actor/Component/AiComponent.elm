module Actor.Component.AiComponent exposing (AiComponentData)

import Data.Coordinate exposing (Direction)


type alias AiComponentData =
    { previousDirection : Direction
    }
