module Data.Common
    exposing
        ( Tick
        , Position
        , Direction(..)
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
