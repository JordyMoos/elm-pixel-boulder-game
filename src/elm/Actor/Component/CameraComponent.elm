module Actor.Component.CameraComponent exposing (updateCameraComponent, getCameraComponent)

import Actor.Actor as Actor
    exposing
        ( Actor
        , Level
        , View
        , Component(CameraComponent)
        , CameraComponentData
        )
import Actor.Common as Common
import Data.Position as Position exposing (Position)
import Dict


updateCameraComponent : CameraComponentData -> Actor -> Level -> Level
updateCameraComponent camera actor level =
    Common.getTransformComponent actor
        |> Maybe.andThen
            (\transformData ->
                let
                    view =
                        level.view

                    viewPosition =
                        view.position

                    position =
                        transformData.position

                    x =
                        if position.x - camera.borderSize <= viewPosition.x then
                            position.x - camera.borderSize
                        else if position.x - view.width + camera.borderSize > viewPosition.x - 1 then
                            position.x - view.width + camera.borderSize + 1
                        else
                            viewPosition.x

                    y =
                        if position.y - camera.borderSize <= viewPosition.y then
                            position.y - camera.borderSize
                        else if position.y - view.height + camera.borderSize >= viewPosition.y - 1 then
                            position.y - view.height + camera.borderSize + 1
                        else
                            viewPosition.y

                    newViewPosition =
                        { viewPosition
                            | x = x
                            , y = y
                        }
                in
                    Just { level | view = Common.updateViewPosition newViewPosition view }
            )
        |> Maybe.withDefault level


getCameraComponent : Actor -> Maybe CameraComponentData
getCameraComponent actor =
    Dict.get "camera" actor.components
        |> Maybe.andThen
            (\component ->
                case component of
                    CameraComponent data ->
                        Just data

                    _ ->
                        Nothing
            )
