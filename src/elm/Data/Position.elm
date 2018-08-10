module Data.Position
    exposing
        ( Position
        , addPosition
        , addPositions
        , getOffsetFromDirection
        )

import Data.Direction as Direction exposing (Direction)


type alias Position =
    { x : Int
    , y : Int
    }


addPositions : List Position -> Position
addPositions =
    List.foldr
        (\position acc ->
            addPosition position acc
        )
        { x = 0, y = 0 }


addPosition : Position -> Position -> Position
addPosition pos1 pos2 =
    { x = pos1.x + pos2.x
    , y = pos1.y + pos2.y
    }


getOffsetFromDirection : Direction -> Position
getOffsetFromDirection direction =
    case direction of
        Direction.Left ->
            { x = -1, y = 0 }

        Direction.Up ->
            { x = 0, y = -1 }

        Direction.Right ->
            { x = 1, y = 0 }

        Direction.Down ->
            { x = 0, y = 1 }
