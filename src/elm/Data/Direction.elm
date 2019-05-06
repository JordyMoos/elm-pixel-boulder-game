module Data.Direction exposing
    ( Direction(..)
    , getDirectionFromID
    , getIDFromDirection
    , getIDFromKey
    , invert
    )


type Direction
    = Left
    | Up
    | Right
    | Down


getDirectionFromID : Int -> Direction
getDirectionFromID id =
    case modBy 4 id of
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


invert : Direction -> Direction
invert direction =
    case direction of
        Left ->
            Right

        Up ->
            Down

        Right ->
            Left

        Down ->
            Up


getIDFromKey : String -> Maybe Int
getIDFromKey key =
    case key of
        "left" ->
            Just 0

        "up" ->
            Just 1

        "right" ->
            Just 2

        "down" ->
            Just 3

        _ ->
            Nothing
