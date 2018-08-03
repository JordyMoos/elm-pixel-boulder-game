module CanvasRenderer exposing (renderLevel, renderText)

import Canvas
import Actor exposing (Level)
import Html exposing (Html)
import Data.Common exposing (Tick, Position, addPosition, addPositions)
import Color exposing (Color)
import Canvas.Point
import Maybe.Extra
import List.Extra
import Dict exposing (Dict)
import Text


pixelSize : Int
pixelSize =
    32


renderLevel : Tick -> Level -> Html msg
renderLevel currentTick level =
    Canvas.initialize (Canvas.Size (level.view.width * pixelSize) (level.view.height * pixelSize))
        |> Canvas.batch [ Canvas.ClearRect (Canvas.Point.fromInts ( 0, 0 )) (Canvas.Size (level.view.width * pixelSize) (level.view.height * pixelSize)) ]
        |> Canvas.batch (drawBackground currentTick level.images level.background level.view.width level.view.height)
        |> (\canvas ->
                List.foldr
                    (\y acc ->
                        List.range level.view.position.x (level.view.position.x + level.view.height - 1)
                            |> List.foldr
                                (\x acc ->
                                    getDrawOps currentTick level.view.position { x = x, y = y } level acc
                                )
                                acc
                    )
                    ( [], [] )
                    (List.range level.view.position.y (level.view.position.y + level.view.height - 1))
                    |> (\( back, front ) ->
                            [ back, front ]
                       )
                    |> List.foldl
                        (\ops canvas ->
                            Canvas.batch ops canvas
                        )
                        canvas
           )
        |> Canvas.toHtml []


drawBackground : Tick -> Actor.CanvasImages -> Actor.RenderComponentData -> Int -> Int -> List Canvas.DrawOp
drawBackground tick images backgroundData width height =
    case backgroundData of
        Actor.PixelRenderComponent data ->
            [ Canvas.FillStyle <| getColor tick data
            , Canvas.FillRect
                (Canvas.Point.fromInts ( 0, 0 ))
                (Canvas.Size (width * pixelSize) (height * pixelSize))
            ]

        Actor.ImageRenderComponent data ->
            Dict.get
                data.name
                images
                |> Maybe.andThen
                    (\image ->
                        Just [ Canvas.DrawImage image <| Canvas.At (Canvas.Point.fromInts ( 0, 0 )) ]
                    )
                |> Maybe.withDefault []


getImage : Position -> Position -> Level -> ( List Canvas.DrawOp, List Canvas.DrawOp ) -> ( List Canvas.DrawOp, List Canvas.DrawOp )
getImage viewPosition position level acc =
    Actor.getActorsThatAffect position level
        |> List.foldr
            (\actor ( backOps, frontOps ) ->
                Actor.getRenderComponent actor
                    |> Maybe.andThen
                        (\renderData ->
                            case renderData of
                                Actor.ImageRenderComponent data ->
                                    Just data

                                _ ->
                                    Nothing
                        )
                    |> Maybe.andThen
                        (\imageData ->
                            Dict.get
                                imageData.name
                                level.images
                        )
                    |> Maybe.andThen
                        (\image ->
                            Actor.getTransformComponent actor
                                |> Maybe.andThen
                                    (\transformData ->
                                        if transformData.position == position then
                                            Just transformData
                                        else
                                            Nothing
                                    )
                                |> Maybe.andThen
                                    (\transformData ->
                                        Just <| getImageOp image transformData viewPosition acc
                                    )
                        )
                    |> Maybe.withDefault acc
            )
            acc


getImageOp : Canvas.Canvas -> Actor.TransformComponentData -> Position -> ( List Canvas.DrawOp, List Canvas.DrawOp ) -> ( List Canvas.DrawOp, List Canvas.DrawOp )
getImageOp image transformData viewPosition ( backOps, frontOps ) =
    case transformData.movingState of
        Actor.NotMoving ->
            ( List.append backOps
                [ Canvas.DrawImage image <|
                    Canvas.At <|
                        Canvas.Point.fromInts
                            ( (transformData.position.x - viewPosition.x) * pixelSize
                            , (transformData.position.y - viewPosition.y) * pixelSize
                            )
                ]
            , frontOps
            )

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
                            round <| aFloat + offset
                    in
                        result
            in
                ( backOps
                , List.append frontOps
                    [ Canvas.DrawImage image <|
                        Canvas.At <|
                            Canvas.Point.fromInts
                                ( calculateWithCompletion (transformData.position.x - viewPosition.x) (towardsData.position.x - viewPosition.x)
                                , calculateWithCompletion (transformData.position.y - viewPosition.y) (towardsData.position.y - viewPosition.y)
                                )
                    ]
                )


getDrawOps : Tick -> Position -> Position -> Level -> ( List Canvas.DrawOp, List Canvas.DrawOp ) -> ( List Canvas.DrawOp, List Canvas.DrawOp )
getDrawOps tick viewPosition position level acc =
    acc
        |> (getPixel tick viewPosition position level)
        |> (getImage viewPosition position level)


getPixel : Tick -> Position -> Position -> Level -> ( List Canvas.DrawOp, List Canvas.DrawOp ) -> ( List Canvas.DrawOp, List Canvas.DrawOp )
getPixel tick viewPosition position level ( backOps, frontOps ) =
    Actor.getActorsThatAffect position level
        |> List.foldr
            (\actor acc ->
                (Actor.getRenderComponent actor
                    |> Maybe.andThen
                        (\renderData ->
                            case renderData of
                                Actor.PixelRenderComponent data ->
                                    Just data

                                _ ->
                                    Nothing
                        )
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
                Just <| ( List.append backOps (asPixel viewPosition position color), frontOps )
            )
        |> Maybe.withDefault ( backOps, frontOps )


getColor : Tick -> Actor.PixelRenderComponentData -> Color
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



{-

   menu:
       items:
           [ { text, msg, [length] } ]
       selected: Int (determines the y offset and color of the selected + msg to spawn on input)
                   Not sure if we spawn messages btw. We just do something in update if A is pressed




-}


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
