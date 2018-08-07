module Renderer.Canvas.TextRenderer exposing (renderText)

import Canvas
import Canvas.Point
import Html exposing (Html)
import Color exposing (Color)
import Data.Common exposing (Tick, Position, addPosition, addPositions)
import Text
import Renderer.Canvas.Common exposing (pixelSize)


renderText : Int -> Int -> List Text.Letters -> Html msg
renderText width height lines =
    Canvas.initialize (Canvas.Size (width * pixelSize) (height * pixelSize))
        |> Canvas.batch (renderLines width height lines)
        |> Canvas.toHtml []


renderLines : Int -> Int -> List Text.Letters -> List Canvas.DrawOp
renderLines width height lines =
    lines
        |> List.indexedMap
            (\index line ->
                renderLine width height (index * (Text.letterHeight + 1)) line
            )
        |> List.concat


renderLine : Int -> Int -> Int -> Text.Letters -> List Canvas.DrawOp
renderLine width height yOffset letters =
    letters
        |> List.foldr
            (\letter ( xOffset, ops ) ->
                ( xOffset + letter.width + 1
                , letter.positions
                    |> List.map (addPosition { x = xOffset, y = yOffset })
                    |> List.concatMap
                        (\position ->
                            [ Canvas.FillStyle Color.red
                            , Canvas.FillRect
                                (Canvas.Point.fromInts ( position.x * pixelSize, position.y * pixelSize ))
                                (Canvas.Size pixelSize pixelSize)
                            ]
                        )
                    |> List.append ops
                )
            )
            ( 0, [] )
        |> Tuple.second
