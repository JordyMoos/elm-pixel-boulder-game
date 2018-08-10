module Renderer.Canvas.TextRenderer exposing (renderText)

import Canvas
import Canvas.Point
import Html exposing (Html)
import Color exposing (Color)
import Data.Position as Position exposing (Position)
import Text
import Renderer.Canvas.Common exposing (pixelSize)


type alias XOffset =
    Int


type alias YOffset =
    Int


renderText : Int -> Int -> List ( XOffset, YOffset, Color, Text.Letters ) -> Html msg
renderText width height lines =
    Canvas.initialize (Canvas.Size (width * pixelSize) (height * pixelSize))
        |> Canvas.batch (renderLines width height lines)
        |> Canvas.toHtml []


renderLines : Int -> Int -> List ( XOffset, YOffset, Color, Text.Letters ) -> List Canvas.DrawOp
renderLines width height lines =
    lines
        |> List.indexedMap
            (\index ( xOffset, yOffset, color, line ) ->
                renderLine width height xOffset yOffset color line
            )
        |> List.concat


renderLine : Int -> Int -> XOffset -> YOffset -> Color -> Text.Letters -> List Canvas.DrawOp
renderLine width height xOffset yOffset color letters =
    letters
        |> List.foldr
            (\letter ( xOffset, ops ) ->
                ( xOffset + letter.width + 1
                , letter.positions
                    |> List.map (Position.addPosition { x = xOffset, y = yOffset })
                    |> List.concatMap
                        (\position ->
                            [ Canvas.FillStyle color
                            , Canvas.FillRect
                                (Canvas.Point.fromInts ( position.x * pixelSize, position.y * pixelSize ))
                                (Canvas.Size pixelSize pixelSize)
                            ]
                        )
                    |> List.append ops
                )
            )
            ( xOffset, [] )
        |> Tuple.second
