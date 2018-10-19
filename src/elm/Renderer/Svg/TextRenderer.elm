module Renderer.Svg.TextRenderer exposing (Line, renderText)

import Color exposing (Color)
import Data.Config exposing (Config)
import Data.Position as Position exposing (Position)
import Html exposing (Html)
import String
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Text


type alias XOffset =
    Int


type alias YOffset =
    Int


type alias Line =
    { xOffset : XOffset
    , yOffset : Int
    , color : Color
    , letters : Text.Letters
    }


renderText : Config -> List Line -> Html msg
renderText config lines =
    Svg.svg
        [ Attributes.width <| String.fromInt <| (config.width * config.pixelSize)
        , Attributes.height <| String.fromInt <| (config.height * config.pixelSize)
        , Attributes.x "0"
        , Attributes.y "0"
        , Attributes.version "1.1"
        ]
        (renderLines config lines)


renderLines : Config -> List Line -> List (Svg msg)
renderLines config lines =
    lines
        |> List.indexedMap
            (\index line ->
                renderLine config line
            )
        |> List.concat


renderLine : Config -> Line -> List (Svg msg)
renderLine config line =
    line.letters
        |> List.foldr
            (\letter ( xOffset, ops ) ->
                ( xOffset + letter.width + 1
                , letter.positions
                    |> List.map (Position.addPosition { x = xOffset, y = line.yOffset })
                    |> List.concatMap
                        (\position ->
                            [ Svg.rect
                                [ Attributes.width <| String.fromInt config.pixelSize
                                , Attributes.height <| String.fromInt config.pixelSize
                                , Attributes.x <| String.fromInt <| position.x * config.pixelSize
                                , Attributes.y <| String.fromInt <| position.y * config.pixelSize
                                , Attributes.fill <| Color.toCssString line.color
                                ]
                                []
                            ]
                        )
                    |> List.append ops
                )
            )
            ( line.xOffset, [] )
        |> Tuple.second
