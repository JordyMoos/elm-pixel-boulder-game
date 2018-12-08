module Actor.Component.CameraComponent exposing (updateCameraComponent)

import Actor.Actor as Actor
    exposing
        ( Actor
        , CameraComponentData
        , Component(..)
        , Level
        , View
        )
import Actor.Common as Common
import Data.Coordinate as Coordinate exposing (Coordinate)
import Dict


updateCameraComponent : CameraComponentData -> Actor -> Level -> Level
updateCameraComponent camera actor level =
    Common.getTransformComponent actor
        |> Maybe.andThen
            (\transformData ->
                let
                    --
                    --                    view =
                    --                        level.view
                    --
                    --                    pixelSize =
                    --                        view.pixelSize
                    --
                    --                    width =
                    --                        view.width * pixelSize
                    --
                    --                    height =
                    --                        view.height * pixelSize
                    --
                    --                    position =
                    --                        transformData.position
                    --
                    --                    movementOffset =
                    --                        movementToPixels pixelSize transformData
                    --
                    --                    givenX =
                    --                        position.x * pixelSize
                    --
                    --                    givenY =
                    --                        position.y * pixelSize
                    --
                    --                    viewCoordinate =
                    --                        view.coordinate
                    --
                    --                    borderSize =
                    --                        camera.borderSize * pixelSize
                    --
                    --                    x =
                    --                        if givenX - borderSize <= viewCoordinate.x then
                    --                            givenX - borderSize + movementOffset.x
                    --
                    --                        else if givenX - width + borderSize > viewCoordinate.x - pixelSize then
                    --                            givenX - width + borderSize + pixelSize - movementOffset.x
                    --
                    --                        else
                    --                            viewCoordinate.x
                    --
                    --                    y =
                    --                        if givenY - borderSize <= viewCoordinate.y then
                    --                            givenY - borderSize + movementOffset.y
                    --
                    --                        else if givenY - height + borderSize >= viewCoordinate.y - pixelSize then
                    --                            givenY - height + borderSize + pixelSize + movementOffset.y
                    --
                    --                        else
                    --                            viewCoordinate.y
                    view =
                        level.view

                    viewCoordinate =
                        view.coordinate

                    pixelSize =
                        view.pixelSize

                    movementOffset =
                        movementToPixels pixelSize transformData

                    borderSize =
                        camera.borderSize * pixelSize

                    width =
                        view.width * pixelSize

                    height =
                        view.height * pixelSize

                    xMin =
                        view.coordinate.x + borderSize

                    xMax =
                        view.coordinate.x + width - borderSize

                    entityX =
                        transformData.position.x * pixelSize + movementOffset.x

                    yMin =
                        view.coordinate.y + borderSize

                    yMax =
                        view.coordinate.y + width - borderSize

                    entityY =
                        transformData.position.y * pixelSize + movementOffset.y

                    clampXResult =
                        clamp xMin xMax entityX

                    clampYResult =
                        clamp yMin yMax entityY

                    -- If the entity is within the limits then we do not move the screen
                    newX =
                        if clampXResult == xMin then
                            entityX - borderSize

                        else if clampXResult == xMax then
                            entityX + borderSize - width

                        else
                            viewCoordinate.x

                    newY =
                        if clampYResult == yMin then
                            entityY - borderSize

                        else if clampYResult == yMax then
                            entityY + borderSize - width

                        else
                            viewCoordinate.y

                    newViewCoordinate =
                        { viewCoordinate
                            | x = newX
                            , y = newY
                        }
                in
                Just { level | view = Common.updateViewCoordinate newViewCoordinate view }
            )
        |> Maybe.withDefault level


movementToPixels : Int -> Actor.TransformComponentData -> Coordinate
movementToPixels pixelSize transformData =
    case transformData.movingState of
        Actor.NotMoving ->
            { x = 0, y = 0 }

        Actor.MovingTowards towardsData ->
            let
                calculateWithCompletion : Int -> Int -> Int
                calculateWithCompletion a b =
                    let
                        aFloat =
                            toFloat (a * pixelSize)

                        bFloat =
                            toFloat (b * pixelSize)

                        diffFloat =
                            bFloat - aFloat

                        offset =
                            diffFloat * (towardsData.completionPercentage / 100)

                        result =
                            round <| offset
                    in
                    result
            in
            { x = calculateWithCompletion transformData.position.x towardsData.position.x
            , y = calculateWithCompletion transformData.position.y towardsData.position.y
            }
