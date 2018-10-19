module Actor.Component.PhysicsComponent exposing
    ( getPhysicsComponent
    , getStrength
    , isActorCircle
    , isCircle
    , isCircleAt
    )

import Actor.Actor as Actor
    exposing
        ( Actor
        , Component(..)
        , Level
        , PhysicsComponentData
        , Shape(..)
        )
import Actor.Common as Common
import Data.Position exposing (Position)
import Dict
import Maybe.Extra


getPhysicsComponent : Actor -> Maybe PhysicsComponentData
getPhysicsComponent actor =
    Dict.get "physics" actor.components
        |> Maybe.andThen
            (\component ->
                case component of
                    PhysicsComponent data ->
                        Just data

                    _ ->
                        Nothing
            )


getStrength : Actor -> Int
getStrength actor =
    getPhysicsComponent actor
        |> Maybe.map .strength
        |> Maybe.withDefault 0


isCircle : PhysicsComponentData -> Bool
isCircle physicsData =
    case physicsData.shape of
        Circle ->
            True

        _ ->
            False


isCircleAt : Position -> Level -> Bool
isCircleAt position level =
    Common.getActorsByPosition position level
        |> List.map getPhysicsComponent
        |> Maybe.Extra.values
        |> List.filter isCircle
        |> List.isEmpty
        |> not


isActorCircle : Actor -> Bool
isActorCircle actor =
    getPhysicsComponent actor
        |> Maybe.map isCircle
        |> Maybe.withDefault False
