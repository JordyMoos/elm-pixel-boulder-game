module Data.Rotation exposing
    ( Rotation
    , getPitch
    , getRoll
    , getYaw
    )


type alias Rotation =
    { x : Float
    , y : Float
    , z : Float
    }


getPitch : Rotation -> Float
getPitch rotation =
    rotation.x


getYaw : Rotation -> Float
getYaw rotation =
    rotation.y


getRoll : Rotation -> Float
getRoll rotation =
    rotation.z
