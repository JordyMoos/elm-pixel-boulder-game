module Data.Direction
    exposing
        ( Direction(..)
        , getDirectionFromID
        , getIDFromDirection
        )


type Direction
    = Left
    | Up
    | Right
    | Down


getDirectionFromID : Int -> Direction
getDirectionFromID id =
    case id % 4 of
        0 ->
            Left

        1 ->
            Up

        2 ->
            Right

        _ ->
            Down


getIDFromDirection : Direction -> Int
getIDFromDirection direction =
    case direction of
        Left ->
            0

        Up ->
            1

        Right ->
            2

        Down ->
            3
