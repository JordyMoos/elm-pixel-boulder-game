module CanvasRenderer exposing (view)

import Canvas
import Actor exposing (Level)
import Html exposing (Html)
import Data.Common exposing (Tick, Position)
import Color exposing (Color)
import Canvas.Point
import Maybe.Extra
import List.Extra


pixelSize : Int
pixelSize =
    30


view : Tick -> Level -> Html msg
view currentTick level =
    Canvas.initialize (Canvas.Size (level.view.width * pixelSize) (level.view.height * pixelSize))
        |> Canvas.batch
            (List.range level.view.position.y (level.view.position.y + level.view.height - 1)
                |> List.map
                    (\y ->
                        List.range level.view.position.x (level.view.position.x + level.view.height - 1)
                            |> List.map
                                (\x ->
                                    getPixel currentTick level.view.position { x = x, y = y } level
                                        |> Maybe.withDefault []
                                )
                            |> List.concat
                    )
                |> List.concat
            )
        |> Canvas.toHtml []


getPixel : Tick -> Position -> Position -> Level -> Maybe (List Canvas.DrawOp)
getPixel tick viewPosition position level =
    Actor.getActorsThatAffect position level
        |> List.foldr
            (\actor acc ->
                (Actor.getRenderComponent actor
                    |> Maybe.andThen
                        (\renderData ->
                            Actor.getTransformComponent actor
                                |> Maybe.andThen
                                    (\transformData ->
                                        if transformData.position == position then
                                            case transformData.movingState of
                                                Actor.MovingTowards towardsData ->
                                                    Just <| calculateColor (getColor tick renderData) (100.0 - towardsData.completionPercentage)

                                                _ ->
                                                    Just (getColor tick renderData)
                                        else
                                            case transformData.movingState of
                                                Actor.MovingTowards towardsData ->
                                                    if towardsData.position == position then
                                                        Just <| calculateColor (getColor tick renderData) towardsData.completionPercentage
                                                    else
                                                        Nothing

                                                _ ->
                                                    Nothing
                                    )
                        )
                )
                    :: acc
            )
            []
        |> Maybe.Extra.values
        |> List.foldr
            (\color acc ->
                case acc of
                    Nothing ->
                        Just color

                    Just accColor ->
                        Just <| combineColors color accColor
            )
            Nothing
        |> Maybe.andThen
            (\color ->
                Just <| asPixel viewPosition position color
            )


getColor : Tick -> Actor.RenderComponentData -> Color
getColor tick renderData =
    round ((toFloat tick) / (toFloat (max renderData.ticksPerColor 1)))
        % (max 1 <| List.length renderData.colors)
        |> (flip List.Extra.getAt) renderData.colors
        |> Maybe.withDefault noColor


noColor : Color
noColor =
    Color.white


combineColors : Color -> Color -> Color
combineColors color1 color2 =
    let
        rgba1 =
            Color.toRgb color1

        rgba2 =
            Color.toRgb color2

        intAlpha1 =
            round (rgba1.alpha * 100.0)

        intAlpha2 =
            round (rgba2.alpha * 100.0)

        combinedRgba =
            { red = round <| (toFloat ((rgba1.red * intAlpha1) + (rgba2.red * intAlpha2))) / (toFloat (max 1 (intAlpha1 + intAlpha2)))
            , green = round <| (toFloat ((rgba1.green * intAlpha1) + (rgba2.green * intAlpha2))) / (toFloat (max 1 (intAlpha1 + intAlpha2)))
            , blue = round <| (toFloat ((rgba1.blue * intAlpha1) + (rgba2.blue * intAlpha2))) / (toFloat (max 1 (intAlpha1 + intAlpha2)))
            , alpha = (max rgba1.alpha rgba2.alpha)
            }
    in
        Color.rgba combinedRgba.red combinedRgba.green combinedRgba.blue combinedRgba.alpha


calculateColor : Color -> Float -> Color
calculateColor color percentage =
    let
        rgba =
            Color.toRgb color

        newRgba =
            { rgba | alpha = (percentage / 100) }
    in
        Color.rgba newRgba.red newRgba.green newRgba.blue newRgba.alpha


asPixel : Position -> Position -> Color -> List Canvas.DrawOp
asPixel viewPosition position color =
    [ Canvas.FillStyle color
    , Canvas.FillRect
        (Canvas.Point.fromInts ( (position.x - viewPosition.x) * pixelSize, (position.y - viewPosition.y) * pixelSize ))
        (Canvas.Size pixelSize pixelSize)
    ]
