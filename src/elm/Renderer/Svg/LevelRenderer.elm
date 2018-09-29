module Renderer.Svg.LevelRenderer exposing (renderLevel)

import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Actor.Actor as Actor exposing (Level)
import Actor.Common as Common
import Actor.Component.RenderComponent as Render
import Html exposing (Html)
import Data.Config exposing (Config)
import Data.Direction as Direction exposing (Direction)
import Data.Position as Position exposing (Position)
import Color exposing (Color)
import Util.Util as Util
import Maybe.Extra
import List.Extra
import Dict exposing (Dict)
import Text


renderLevel : Int -> Config -> Level -> Actor.Images -> Html msg
renderLevel currentTick config level images =
    List.concat
        [ drawBackground currentTick config images level.background
        , drawLevel currentTick config level images
        ]
        |> Svg.svg
            [ Attributes.width <| toString <| (config.width * config.pixelSize)
            , Attributes.height <| toString <| (config.height * config.pixelSize)
            , Attributes.x "0"
            , Attributes.y "0"
            , Attributes.version "1.1"
            ]


drawBackground : Int -> Config -> Actor.Images -> Actor.RenderComponentData -> List (Svg msg)
drawBackground tick config images backgroundData =
    case backgroundData of
        Actor.PixelRenderComponent data ->
            [ Svg.rect
                [ Attributes.width <| toString <| (config.width * config.pixelSize)
                , Attributes.height <| toString <| (config.height * config.pixelSize)
                , Attributes.x "0"
                , Attributes.y "0"
                , Attributes.fill <| Util.colorToHex <| getColor tick data
                ]
                []
            ]

        Actor.ImageRenderComponent data ->
            getImageName tick data.default
                |> Maybe.andThen (flip Dict.get images)
                |> Maybe.map
                    (\image ->
                        [ Svg.image
                            [ Attributes.width <| toString <| (config.width * config.pixelSize)
                            , Attributes.height <| toString <| (config.height * config.pixelSize)
                            , Attributes.xlinkHref image
                            , Attributes.x "0"
                            , Attributes.y "0"
                            ]
                            []
                        ]
                    )
                |> Maybe.withDefault []


drawLevel : Int -> Config -> Level -> Actor.Images -> List (Svg msg)
drawLevel tick config level images =
    List.foldr
        (\y acc ->
            List.range level.view.position.x (level.view.position.x + level.view.height - 1)
                |> List.foldr
                    (\x acc ->
                        getDrawOps tick config level.view.position { x = x, y = y } level images acc
                    )
                    acc
        )
        ( [], [] )
        (List.range level.view.position.y (level.view.position.y + level.view.height - 1))
        |> (\( back, front ) ->
                List.append
                    back
                    front
           )


getImage : Int -> Config -> Position -> Position -> Level -> Actor.Images -> ( List (Svg msg), List (Svg msg) ) -> ( List (Svg msg), List (Svg msg) )
getImage tick config viewPosition position level images acc =
    Common.getActorsThatAffect position level
        |> List.foldr
            (\actor ( backOps, frontOps ) ->
                Render.getRenderComponent actor
                    |> Maybe.andThen
                        (\renderData ->
                            case renderData of
                                Actor.ImageRenderComponent data ->
                                    Just data

                                _ ->
                                    Nothing
                        )
                    |> Maybe.andThen
                        (\imageRenderData ->
                            Common.getTransformComponent actor
                                |> Maybe.andThen
                                    (\transformData ->
                                        if transformData.position == position then
                                            Just transformData
                                        else
                                            Nothing
                                    )
                                |> Maybe.andThen
                                    (\transformData ->
                                        Just <| getImageOp tick config imageRenderData transformData viewPosition images actor.id acc
                                    )
                        )
                    |> Maybe.withDefault acc
            )
            acc


getImageOp :
    Int
    -> Config
    -> Actor.ImageRenderComponentData
    -> Actor.TransformComponentData
    -> Position
    -> Actor.Images
    -> Actor.ActorId
    -> ( List (Svg msg), List (Svg msg) )
    -> ( List (Svg msg), List (Svg msg) )
getImageOp tick config imageRenderData transformData viewPosition images actorId ( backOps, frontOps ) =
    case transformData.movingState of
        Actor.NotMoving ->
            (getImageName tick imageRenderData.default)
                |> Maybe.andThen (flip Dict.get images)
                |> Maybe.map
                    (\image ->
                        ( List.append backOps
                            [ Svg.image
                                [ Attributes.width (toString config.pixelSize)
                                , Attributes.height (toString config.pixelSize)
                                , Attributes.xlinkHref image
                                , Attributes.x <| toString <| (transformData.position.x - viewPosition.x) * config.pixelSize
                                , Attributes.y <| toString <| (transformData.position.y - viewPosition.y) * config.pixelSize
                                , Attributes.id <| "actor-" ++ (toString actorId)
                                ]
                                []
                            ]
                        , frontOps
                        )
                    )
                |> Maybe.withDefault ( backOps, frontOps )

        Actor.MovingTowards towardsData ->
            let
                calculateWithCompletion : Int -> Int -> Int
                calculateWithCompletion a b =
                    let
                        aFloat =
                            toFloat (a * config.pixelSize)

                        bFloat =
                            toFloat (b * config.pixelSize)

                        diffFloat =
                            bFloat - aFloat

                        offset =
                            diffFloat * (towardsData.completionPercentage / 100)

                        result =
                            round <| aFloat + offset
                    in
                        result
            in
                getImageNamesDataByDirection towardsData.direction imageRenderData
                    |> getImageName tick
                    |> Maybe.andThen (flip Dict.get images)
                    |> Maybe.map
                        (\image ->
                            ( backOps
                            , List.append frontOps
                                [ Svg.image
                                    [ Attributes.width (toString config.pixelSize)
                                    , Attributes.height (toString config.pixelSize)
                                    , Attributes.xlinkHref image
                                    , Attributes.x <| toString <| calculateWithCompletion (transformData.position.x - viewPosition.x) (towardsData.position.x - viewPosition.x)
                                    , Attributes.y <| toString <| calculateWithCompletion (transformData.position.y - viewPosition.y) (towardsData.position.y - viewPosition.y)
                                    , Attributes.id <| "actor-" ++ (toString actorId)
                                    ]
                                    []
                                ]
                            )
                        )
                    |> Maybe.withDefault ( backOps, frontOps )


getImageNamesDataByDirection : Direction -> Actor.ImageRenderComponentData -> Actor.ImagesData
getImageNamesDataByDirection direction imageRenderData =
    Direction.getIDFromDirection direction
        |> flip Dict.get imageRenderData.direction
        |> Maybe.withDefault imageRenderData.default


getDrawOps : Int -> Config -> Position -> Position -> Level -> Actor.Images -> ( List (Svg msg), List (Svg msg) ) -> ( List (Svg msg), List (Svg msg) )
getDrawOps tick config viewPosition position level images acc =
    acc
        |> (getPixel tick config viewPosition position level)
        |> (getImage tick config viewPosition position level images)


getPixel : Int -> Config -> Position -> Position -> Level -> ( List (Svg msg), List (Svg msg) ) -> ( List (Svg msg), List (Svg msg) )
getPixel tick config viewPosition position level ( backOps, frontOps ) =
    Common.getActorsThatAffect position level
        |> List.foldr
            (\actor acc ->
                (Render.getRenderComponent actor
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
                            Common.getTransformComponent actor
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
                Just <| ( List.append backOps [ (asPixel config viewPosition position color) ], frontOps )
            )
        |> Maybe.withDefault ( backOps, frontOps )


getColor : Int -> Actor.PixelRenderComponentData -> Color
getColor tick renderData =
    round ((toFloat tick) / (toFloat (max renderData.ticksPerColor 1)))
        % (max 1 <| List.length renderData.colors)
        |> (flip List.Extra.getAt) renderData.colors
        |> Maybe.withDefault noColor


getImageName : Int -> Actor.ImagesData -> Maybe String
getImageName tick imagesData =
    round ((toFloat tick) / (toFloat (max imagesData.ticksPerImage 1)))
        % (max 1 <| List.length imagesData.names)
        |> (flip List.Extra.getAt) imagesData.names


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


asPixel : Config -> Position -> Position -> Color -> Svg msg
asPixel config viewPosition position color =
    Svg.rect
        [ Attributes.width <| toString <| config.pixelSize
        , Attributes.height <| toString <| config.pixelSize
        , Attributes.x <| toString <| (position.x - viewPosition.x) * config.pixelSize
        , Attributes.y <| toString <| (position.y - viewPosition.y) * config.pixelSize
        , Attributes.fill <| Util.colorToHex color
        ]
        []
