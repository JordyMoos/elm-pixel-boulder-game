module Renderer.Canvas.TextRenderer exposing (renderText)

import Canvas
import Canvas.Point
import Html exposing (Html)
import Color exposing (Color)
import Data.Common exposing (Tick, Position, addPosition, addPositions)
import Text
import Renderer.Canvas.Common exposing (pixelSize)


type alias XOffset =
    Int


renderText : Int -> Int -> List ( XOffset, Text.Letters ) -> Html msg
renderText width height lines =
    Canvas.initialize (Canvas.Size (width * pixelSize) (height * pixelSize))
        |> Canvas.batch (renderLines width height lines)
        |> Canvas.toHtml []


renderLines : Int -> Int -> List ( XOffset, Text.Letters ) -> List Canvas.DrawOp
renderLines width height lines =
    lines
        |> List.indexedMap
            (\index ( xOffset, line ) ->
                renderLine width height (index * (Text.letterHeight + 1)) xOffset line
            )
        |> List.concat


renderLine : Int -> Int -> Int -> XOffset -> Text.Letters -> List Canvas.DrawOp
renderLine width height yOffset xOffset letters =
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
            ( xOffset, [] )
        |> Tuple.second
