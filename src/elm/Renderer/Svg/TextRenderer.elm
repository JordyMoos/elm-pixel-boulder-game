module Renderer.Svg.TextRenderer exposing (renderText)

import Color exposing (Color)
import Data.Config exposing (Config)
import Data.Position as Position exposing (Position)
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Text
import String


type alias XOffset =
    Int


type alias YOffset =
    Int


renderText : Config -> List ( XOffset, YOffset, Color, Text.Letters ) -> Html msg
renderText config lines =
    Svg.svg
        [ Attributes.width <| String.fromInt <| (config.width * config.pixelSize)
        , Attributes.height <| String.fromInt <| (config.height * config.pixelSize)
        , Attributes.x "0"
        , Attributes.y "0"
        , Attributes.version "1.1"
        ]
        (renderLines config lines)



renderLines : Config -> List ( XOffset, YOffset, Color, Text.Letters ) -> List (Svg msg)
renderLines config lines =
    lines
        |> List.indexedMap
            (\index ( xOffset, yOffset, color, line ) ->
                renderLine config xOffset yOffset color line
            )
        |> List.concat



renderLine : Config -> XOffset -> YOffset -> Color -> Text.Letters -> List (Svg msg)
renderLine config givenXOffset yOffset color letters =
    letters
        |> List.foldr
            (\letter ( xOffset, ops ) ->
                ( xOffset + letter.width + 1
                , letter.positions
                    |> List.map (Position.addPosition { x = xOffset, y = yOffset })
                    |> List.concatMap
                        (\position ->
                            [ Svg.rect
                                [ Attributes.width <| String.fromInt config.pixelSize
                                , Attributes.height <| String.fromInt config.pixelSize
                                , Attributes.x <| String.fromInt <| position.x * config.pixelSize
                                , Attributes.y <| String.fromInt <| position.y * config.pixelSize
                                , Attributes.fill <| Color.Convert.colorToHex color
                                ]
                                []
                            ]
                        )
                    |> List.append ops
                )
            )
            ( givenXOffset, [] )
        |> Tuple.second


