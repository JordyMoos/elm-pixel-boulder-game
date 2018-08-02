module Data.Common
    exposing
        ( Tick
        , Position
        , Direction(..)
        , addPosition
        , addPositions
        )


type Direction
    = Left
    | Up
    | Right
    | Down


type alias Tick =
    Int


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
