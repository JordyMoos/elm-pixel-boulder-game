module Renderer.Svg.TextRenderer exposing (renderText)

import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Html exposing (Html)
import Color exposing (Color)
import Data.Config exposing (Config)
import Data.Position as Position exposing (Position)
import Text
import Color.Convert


type alias XOffset =
    Int


type alias YOffset =
    Int


renderText : Config -> List ( XOffset, YOffset, Color, Text.Letters ) -> Html msg
renderText config lines =
    Svg.svg
        [ Attributes.width <| toString <| (config.width * config.pixelSize)
        , Attributes.height <| toString <| (config.height * config.pixelSize)
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
renderLine config xOffset yOffset color letters =
    letters
        |> List.foldr
            (\letter ( xOffset, ops ) ->
                ( xOffset + letter.width + 1
                , letter.positions
                    |> List.map (Position.addPosition { x = xOffset, y = yOffset })
                    |> List.concatMap
                        (\position ->
                            [ Svg.rect
                                [ Attributes.width <| toString config.pixelSize
                                , Attributes.height <| toString config.pixelSize
                                , Attributes.x <| toString <| position.x * config.pixelSize
                                , Attributes.y <| toString <| position.y * config.pixelSize
                                , Attributes.fill <| Color.Convert.colorToHex color
                                ]
                                []
                            ]
                        )
                    |> List.append ops
                )
            )
            ( xOffset, [] )
        |> Tuple.second
