module Data.Coordinate exposing
    ( Coordinate
    , addCoordinate
    , addCoordinates
    , asPosition
    , pixelToTile
    )

import Data.Position exposing (Position)


type alias Coordinate =
    { x : Int
    , y : Int
    }


addCoordinates : List Coordinate -> Coordinate
addCoordinates =
    List.foldr
        (\coordinate acc ->
            addCoordinate coordinate acc
        )
        { x = 0, y = 0 }


addCoordinate : Coordinate -> Coordinate -> Coordinate
addCoordinate pos1 pos2 =
    { x = pos1.x + pos2.x
    , y = pos1.y + pos2.y
    }


pixelToTile : Int -> Int -> Int
pixelToTile pixelSize pixel =
    pixel // pixelSize


asPosition : Int -> Coordinate -> Position
asPosition pixelSize coordinate =
    { x = pixelToTile pixelSize coordinate.x
    , y = pixelToTile pixelSize coordinate.y
    }
