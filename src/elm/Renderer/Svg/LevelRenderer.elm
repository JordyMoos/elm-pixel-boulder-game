module Renderer.Svg.LevelRenderer exposing (renderLevel)

import Actor.Actor as Actor exposing (Level)
import Actor.Common as Common
import Actor.Component.RenderComponent as Render
import Color exposing (Color)
import Data.Config exposing (Config)
import Data.Coordinate as Coordinate exposing (Coordinate)
import Data.Direction as Direction exposing (Direction)
import Data.Position as Position exposing (Position)
import Dict exposing (Dict)
import Html exposing (Html)
import List.Extra
import Maybe.Extra
import String
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Util.Util as Util


renderLevel : Int -> Config -> Level -> Actor.Images -> Html msg
renderLevel currentTick config level images =
    Util.fastConcat
        [ [ drawLoadImages config images ]
        , drawBackground currentTick config images level.background
        , drawLevel currentTick config level images
        ]
        |> Svg.svg
            [ Attributes.width <| String.fromInt <| config.width * config.pixelSize
            , Attributes.height <| String.fromInt <| config.height * config.pixelSize
            , Attributes.x "0"
            , Attributes.y "0"
            , Attributes.version "1.1"
            ]


drawLoadImages : Config -> Actor.Images -> Svg msg
drawLoadImages config images =
    Dict.toList images
        |> List.map (drawLoadImage config)
        |> Svg.defs []


drawLoadImage : Config -> ( String, String ) -> Svg msg
drawLoadImage config ( name, path ) =
    Svg.image
        [ Attributes.width <| String.fromInt <| config.pixelSize
        , Attributes.height <| String.fromInt <| config.pixelSize
        , Attributes.id <| "image-" ++ name
        , Attributes.xlinkHref path
        ]
        []


drawBackground : Int -> Config -> Actor.Images -> Actor.RenderComponentData -> List (Svg msg)
drawBackground tick config images backgroundData =
    case backgroundData of
        Actor.PixelRenderComponent data ->
            [ Svg.rect
                [ Attributes.width <| String.fromInt <| config.width * config.pixelSize
                , Attributes.height <| String.fromInt <| config.height * config.pixelSize
                , Attributes.x "0"
                , Attributes.y "0"
                , Attributes.fill <| Color.toCssString <| getColor tick data
                ]
                []
            ]

        Actor.ImageRenderComponent data ->
            getImageName tick data.default
                |> Maybe.andThen (\a -> Dict.get a images)
                |> Maybe.map
                    (\image ->
                        [ Svg.image
                            [ Attributes.width <| String.fromInt <| config.width * config.pixelSize
                            , Attributes.height <| String.fromInt <| config.height * config.pixelSize
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
    let
        view =
            level.view

        xPixelOffset =
            modBy view.pixelSize view.coordinate.x

        yPixelOffset =
            modBy view.pixelSize view.coordinate.y

        viewPixelOffset =
            { x = xPixelOffset * -1
            , y = yPixelOffset * -1
            }

        xBasePosition =
            Coordinate.pixelToTile view.pixelSize view.coordinate.x

        yBasePosition =
            Coordinate.pixelToTile view.pixelSize view.coordinate.y

        viewPosition =
            { x =
                if view.coordinate.x < 0 && viewPixelOffset.x /= 0 then
                    xBasePosition - 1

                else
                    xBasePosition
            , y =
                if view.coordinate.y < 0 && viewPixelOffset.y /= 0 then
                    yBasePosition - 1

                else
                    yBasePosition
            }

        xEndPosition =
            xBasePosition + level.view.width

        yEndPosition =
            yBasePosition + level.view.height
    in
    List.foldr
        (\y acc ->
            List.range xBasePosition xEndPosition
                |> List.foldr
                    (\x innerAcc ->
                        getDrawOps tick config viewPosition { x = x, y = y } viewPixelOffset level images innerAcc
                    )
                    acc
        )
        ( [], [] )
        (List.range yBasePosition yEndPosition)
        |> (\( back, front ) ->
                List.append
                    back
                    front
           )


getImage : Int -> Config -> Position -> Position -> Coordinate -> Level -> Actor.Images -> ( List (Svg msg), List (Svg msg) ) -> ( List (Svg msg), List (Svg msg) )
getImage tick config viewPosition position pixelOffset level images acc =
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
                                        Just <| getImageOp tick config imageRenderData transformData viewPosition pixelOffset images actor acc
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
    -> Coordinate
    -> Actor.Images
    -> Actor.Actor
    -> ( List (Svg msg), List (Svg msg) )
    -> ( List (Svg msg), List (Svg msg) )
getImageOp tick config imageRenderData transformData viewPosition pixelOffset images actor ( backOps, frontOps ) =
    let
        notMovingOp _ =
            getImageName tick imageRenderData.default
                |> Maybe.map
                    (\imageName ->
                        ( List.append backOps
                            [ Svg.use
                                [ Attributes.xlinkHref <| "#image-" ++ imageName
                                , Attributes.x <| String.fromInt <| (transformData.position.x - viewPosition.x) * config.pixelSize + pixelOffset.x
                                , Attributes.y <| String.fromInt <| (transformData.position.y - viewPosition.y) * config.pixelSize + pixelOffset.y
                                ]
                                []
                            ]
                        , frontOps
                        )
                    )
                |> Maybe.withDefault ( backOps, frontOps )

        movingOp towardsData =
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
                |> Maybe.map
                    (\imageName ->
                        ( backOps
                        , List.append frontOps
                            [ Svg.use
                                [ Attributes.xlinkHref <| "#image-" ++ imageName
                                , Attributes.x <| String.fromInt <| calculateWithCompletion (transformData.position.x - viewPosition.x) (towardsData.position.x - viewPosition.x) + pixelOffset.x
                                , Attributes.y <| String.fromInt <| calculateWithCompletion (transformData.position.y - viewPosition.y) (towardsData.position.y - viewPosition.y) + pixelOffset.y
                                ]
                                []
                            ]
                        )
                    )
                |> Maybe.withDefault ( backOps, frontOps )
    in
    Common.getMovementComponent actor
        |> Maybe.andThen Common.getMovingTowardsData
        |> Maybe.map movingOp
        |> Maybe.withDefault (notMovingOp ())


getImageNamesDataByDirection : Direction -> Actor.ImageRenderComponentData -> Actor.ImagesData
getImageNamesDataByDirection direction imageRenderData =
    Direction.getIDFromDirection direction
        |> (\a -> Dict.get a imageRenderData.direction)
        |> Maybe.withDefault imageRenderData.default


getDrawOps : Int -> Config -> Position -> Position -> Coordinate -> Level -> Actor.Images -> ( List (Svg msg), List (Svg msg) ) -> ( List (Svg msg), List (Svg msg) )
getDrawOps tick config viewPosition position pixelOffset level images acc =
    acc
        |> getPixel tick config viewPosition position pixelOffset level
        |> getImage tick config viewPosition position pixelOffset level images


getPixel : Int -> Config -> Position -> Position -> Coordinate -> Level -> ( List (Svg msg), List (Svg msg) ) -> ( List (Svg msg), List (Svg msg) )
getPixel tick config viewPosition position pixelOffset level ( backOps, frontOps ) =
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
                                            Common.getMovementComponent actor
                                                |> Maybe.andThen Common.getMovingTowardsData
                                                |> Maybe.map (\towardsData -> calculateColor (getColor tick renderData) (100.0 - towardsData.completionPercentage))
                                                |> Maybe.withDefault (getColor tick renderData)
                                                |> Maybe.Just

                                        else
                                            Common.getMovementComponent actor
                                                |> Maybe.andThen Common.getMovingTowardsData
                                                |> Maybe.map (\towardsData -> calculateColor (getColor tick renderData) towardsData.completionPercentage)
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
                Just <| ( List.append backOps [ asPixel config viewPosition position pixelOffset color ], frontOps )
            )
        |> Maybe.withDefault ( backOps, frontOps )


getColor : Int -> Actor.PixelRenderComponentData -> Color
getColor tick renderData =
    modBy (max 1 <| List.length renderData.colors) (round (toFloat tick / toFloat (max renderData.ticksPerColor 1)))
        |> (\b a -> List.Extra.getAt a b) renderData.colors
        |> Maybe.withDefault noColor


getImageName : Int -> Actor.ImagesData -> Maybe String
getImageName tick imagesData =
    modBy (max 1 <| List.length imagesData.names) (round (toFloat tick / toFloat (max imagesData.ticksPerImage 1)))
        |> (\b a -> List.Extra.getAt a b) imagesData.names


noColor : Color
noColor =
    Color.white


combineColors : Color -> Color -> Color
combineColors color1 color2 =
    let
        rgba1 =
            Color.toRgba color1

        rgba2 =
            Color.toRgba color2

        intAlpha1 =
            rgba1.alpha * 100.0

        intAlpha2 =
            rgba2.alpha * 100.0
    in
    Color.fromRgba
        { red = ((rgba1.red * intAlpha1) + (rgba2.red * intAlpha2)) / max 1 (intAlpha1 + intAlpha2)
        , green = ((rgba1.green * intAlpha1) + (rgba2.green * intAlpha2)) / max 1 (intAlpha1 + intAlpha2)
        , blue = ((rgba1.blue * intAlpha1) + (rgba2.blue * intAlpha2)) / max 1 (intAlpha1 + intAlpha2)
        , alpha = max rgba1.alpha rgba2.alpha
        }


calculateColor : Color -> Float -> Color
calculateColor color percentage =
    let
        rgba =
            Color.toRgba color

        newRgba =
            { rgba | alpha = percentage / 100 }
    in
    Color.rgba newRgba.red newRgba.green newRgba.blue newRgba.alpha


asPixel : Config -> Position -> Position -> Coordinate -> Color -> Svg msg
asPixel config viewPosition position pixelOffset color =
    Svg.rect
        [ Attributes.width <| String.fromInt config.pixelSize
        , Attributes.height <| String.fromInt config.pixelSize
        , Attributes.x <| String.fromInt <| (position.x - viewPosition.x) * config.pixelSize + pixelOffset.x
        , Attributes.y <| String.fromInt <| (position.y - viewPosition.y) * config.pixelSize + pixelOffset.y
        , Attributes.fill <| Color.toCssString color
        ]
        []
