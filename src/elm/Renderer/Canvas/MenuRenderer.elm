module Renderer.Canvas.MenuRenderer exposing (..)

import Html exposing (Html)
import Data.Config exposing (Config)
import Data.Menu as Menu exposing (Menu)
import Renderer.Canvas.TextRenderer as TextRenderer
import Maybe.Extra
import List.Extra
import Color
import Text


type alias WithText a =
    { a | text : Text.Letters }


render : Config -> Int -> Menu (WithText a) -> Html msg
render config tick menu =
    TextRenderer.renderText
        config.width
        config.height
        (Maybe.Extra.values
            [ List.Extra.last menu.items.before |> Maybe.map (\item -> ( 0, -3, Color.red, item.text ))
            , Just ( getXOffset config tick menu.items.selected.text, 3, Color.blue, menu.items.selected.text )
            , List.head menu.items.after |> Maybe.map (\item -> ( 0, 9, Color.red, item.text ))
            ]
        )


getXOffset : Config -> Int -> Text.Letters -> Int
getXOffset config tick letters =
    let
        lineLength =
            getLineLength letters

        beforeLength =
            2

        afterLength =
            0

        totalLength =
            beforeLength + lineLength + afterLength

        minOffset =
            0

        maxOffset =
            max 0 (lineLength - config.width)

        tickSpeedCorrection =
            tick // 4

        offset =
            (tickSpeedCorrection % totalLength) - beforeLength
    in
        clamp minOffset maxOffset offset
            |> negate


getLineLength : Text.Letters -> Int
getLineLength letters =
    letters
        |> List.map .width
        |> List.sum
        |> (+) (List.length letters)
