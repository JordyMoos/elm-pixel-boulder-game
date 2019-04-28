module Data.Position exposing
    ( Position
    , X
    , Y
    , addDirection
    , addPosition
    , addPositions
    , aroundNeighborOffsets
    , directNeighborOffsets
    , fromTuple
    , getOffsetFromDirection
    )

import Data.Direction as Direction exposing (Direction)


type alias X =
    Int


type alias Y =
    Int


type alias Position =
    { x : X
    , y : Y
    }


fromTuple : ( X, Y ) -> Position
fromTuple ( x, y ) =
    { x = x
    , y = y
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


addDirection : Position -> Direction -> Position
addDirection position direction =
    addPosition position (getOffsetFromDirection direction)


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


directNeighborOffsets : List Position
directNeighborOffsets =
    [ getOffsetFromDirection Direction.Left
    , getOffsetFromDirection Direction.Up
    , getOffsetFromDirection Direction.Right
    , getOffsetFromDirection Direction.Down
    ]


aroundNeighborOffsets : List Position
aroundNeighborOffsets =
    [ getOffsetFromDirection Direction.Left
    , getOffsetFromDirection Direction.Up
    , getOffsetFromDirection Direction.Right
    , getOffsetFromDirection Direction.Down
    , addPosition (getOffsetFromDirection Direction.Left) (getOffsetFromDirection Direction.Up)
    , addPosition (getOffsetFromDirection Direction.Up) (getOffsetFromDirection Direction.Right)
    , addPosition (getOffsetFromDirection Direction.Right) (getOffsetFromDirection Direction.Down)
    , addPosition (getOffsetFromDirection Direction.Down) (getOffsetFromDirection Direction.Left)
    ]
