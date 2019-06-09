module Renderer.Aframe.LevelRenderer exposing (renderLevel)

import Actor.Actor as Actor exposing (Level)
import Actor.Common as Common
import Actor.Component.RenderComponent as Render
import Color exposing (Color)
import Data.Config exposing (Config)
import Data.Coordinate as Coordinate exposing (Coordinate)
import Data.Direction as Direction exposing (Direction)
import Data.Position as Position exposing (Position)
import Dict exposing (Dict)
import Html exposing (Html, node)
import Html.Attributes as Attributes
import List.Extra
import Maybe.Extra
import String
import Util.Util as Util


zDistance : String
zDistance =
    "-20"


zDistanceBG : String
zDistanceBG =
    "-22"


type alias Offset =
    { x : Float
    , y : Float
    }


renderLevel : Int -> Config -> Level -> Actor.Images -> Html msg
renderLevel currentTick config level images =
    Util.fastConcat
        [ [ drawLoadImages config images ]
        , drawBackground currentTick config images level.background

        --        , [ node "a-entity"
        --                [ Attributes.attribute "camera" "userHeight: 1.6"
        --                , Attributes.attribute "position" "6 -6 0"
        --                ]
        --                []
        --          ]
        , [ node "a-camera"
                [ Attributes.attribute "position" "3 -8 -5"
                ]
                []
          ]
        , drawLevel currentTick config level images
        ]
        |> node "a-scene" []


drawLoadImages : Config -> Actor.Images -> Html msg
drawLoadImages config images =
    Dict.toList images
        |> List.map (drawLoadImage config)
        |> node "a-assets" []


drawLoadImage : Config -> ( String, String ) -> Html msg
drawLoadImage config ( name, path ) =
    Html.img
        [ Attributes.id <| "image-" ++ name
        , Attributes.src path
        ]
        []


drawBackground : Int -> Config -> Actor.Images -> Actor.RenderComponentData -> List (Html msg)
drawBackground tick config images backgroundData =
    case backgroundData of
        Actor.PixelRenderComponent data ->
            [ node "a-box"
                [ Attributes.width config.width
                , Attributes.height config.height
                , Attributes.attribute "material" "color: red"
                , Attributes.attribute "position" <| "0 0 " ++ zDistanceBG
                ]
                []
            ]

        Actor.ImageRenderComponent data ->
            getImageName tick data.default
                |> Maybe.map
                    (\image ->
                        --                        [ node "a-image"
                        --                            [ Attributes.width (config.width + 1)
                        --                            , Attributes.height (config.height + 1)
                        --                            , Attributes.attribute "src" <| "#image-" ++ image
                        --                            , Attributes.attribute "position" <| "6 -6 " ++ zDistanceBG
                        --                            ]
                        --                            []
                        --                        ]
                        [ node "a-sky"
                            [ Attributes.attribute "src" <| "#image-" ++ image ]
                            []
                        ]
                    )
                |> Maybe.withDefault []


drawLevel : Int -> Config -> Level -> Actor.Images -> List (Html msg)
drawLevel tick config level images =
    let
        view =
            level.view

        xPixelOffset =
            (100 / toFloat view.pixelSize * (toFloat <| modBy view.pixelSize view.coordinate.x)) * 0.01

        yPixelOffset =
            (100 / toFloat view.pixelSize * (toFloat <| modBy view.pixelSize view.coordinate.y)) * 0.01

        viewPixelOffset =
            { x = xPixelOffset * -1
            , y = yPixelOffset * -1
            }

        xBasePosition =
            Coordinate.pixelToTile view.pixelSize view.coordinate.x - config.additionalViewBorder

        yBasePosition =
            Coordinate.pixelToTile view.pixelSize view.coordinate.y - config.additionalViewBorder

        viewPosition =
            { x =
                if view.coordinate.x < 0 && viewPixelOffset.x /= 0.0 then
                    xBasePosition - 1

                else
                    xBasePosition
            , y =
                if view.coordinate.y < 0 && viewPixelOffset.y /= 0.0 then
                    yBasePosition - 1

                else
                    yBasePosition
            }

        xEndPosition =
            xBasePosition + level.view.width + (config.additionalViewBorder * 2)

        yEndPosition =
            yBasePosition + level.view.height + (config.additionalViewBorder * 2)
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
        Dict.empty
        (List.range yBasePosition yEndPosition)
        |> toSortedList


getImage : Int -> Config -> Position -> Position -> Offset -> Level -> Actor.Images -> LayeredSvg msg -> LayeredSvg msg
getImage tick config viewPosition position pixelOffset level images acc =
    Common.getActorsThatAffect position level
        |> List.foldr
            (\actor innerAcc ->
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
                                        Just <| getImageOp tick config imageRenderData transformData viewPosition pixelOffset images actor innerAcc
                                    )
                        )
                    |> Maybe.withDefault innerAcc
            )
            acc


getImageOp :
    Int
    -> Config
    -> Actor.ImageRenderComponentData
    -> Actor.TransformComponentData
    -> Position
    -> Offset
    -> Actor.Images
    -> Actor.Actor
    -> LayeredSvg msg
    -> LayeredSvg msg
getImageOp tick config imageRenderData transformData viewPosition pixelOffset images actor acc =
    let
        notMovingOp _ =
            getImageName tick imageRenderData.default
                |> Maybe.map
                    (\imageName ->
                        let
                            x =
                                String.fromFloat <| toFloat (transformData.position.x - viewPosition.x) + pixelOffset.x

                            y =
                                String.fromFloat <| (toFloat (transformData.position.y - viewPosition.y) + pixelOffset.y) * -1.0

                            position =
                                x ++ " " ++ y ++ " " ++ " " ++ zDistance
                        in
                        addToLayeredSvg
                            imageRenderData.layer
                            (Html.node "a-box"
                                [ Attributes.attribute "src" <| "#image-" ++ imageName
                                , Attributes.attribute "position" position
                                ]
                                []
                            )
                            acc
                    )
                |> Maybe.withDefault acc

        movingOp towardsData =
            let
                calculateWithCompletion : Int -> Int -> Float
                calculateWithCompletion a b =
                    let
                        aFloat =
                            toFloat a

                        bFloat =
                            toFloat b

                        diffFloat =
                            bFloat - aFloat

                        offset =
                            diffFloat * (towardsData.completionPercentage / 100)

                        result =
                            aFloat + offset
                    in
                    result
            in
            getImageNamesDataByDirection towardsData.direction imageRenderData
                |> getImageName tick
                |> Maybe.map
                    (\imageName ->
                        let
                            x =
                                String.fromFloat <| calculateWithCompletion (transformData.position.x - viewPosition.x) (towardsData.position.x - viewPosition.x) + pixelOffset.x

                            y =
                                String.fromFloat <| (calculateWithCompletion (transformData.position.y - viewPosition.y) (towardsData.position.y - viewPosition.y) + pixelOffset.y) * -1.0

                            position =
                                x ++ " " ++ y ++ " " ++ " " ++ zDistance
                        in
                        addToLayeredSvg
                            imageRenderData.layer
                            (node "a-image"
                                [ Attributes.attribute "src" <| "#image-" ++ imageName
                                , Attributes.attribute "position" position
                                ]
                                []
                            )
                            acc
                    )
                |> Maybe.withDefault acc
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


getDrawOps : Int -> Config -> Position -> Position -> Offset -> Level -> Actor.Images -> LayeredSvg msg -> LayeredSvg msg
getDrawOps tick config viewPosition position pixelOffset level images acc =
    acc
        |> getPixel tick config viewPosition position pixelOffset level
        |> getImage tick config viewPosition position pixelOffset level images


getPixel : Int -> Config -> Position -> Position -> Offset -> Level -> LayeredSvg msg -> LayeredSvg msg
getPixel tick config viewPosition position pixelOffset level givenAcc =
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
                                                |> Maybe.map (\color -> ( renderData.layer, color ))
                                                |> Maybe.withDefault ( renderData.layer, getColor tick renderData )
                                                |> Maybe.Just

                                        else
                                            Common.getMovementComponent actor
                                                |> Maybe.andThen Common.getMovingTowardsData
                                                |> Maybe.map (\towardsData -> calculateColor (getColor tick renderData) towardsData.completionPercentage)
                                                |> Maybe.map (\color -> ( renderData.layer, color ))
                                    )
                        )
                )
                    :: acc
            )
            []
        |> Maybe.Extra.values
        |> List.foldr
            (\layerAndColor acc ->
                case acc of
                    Nothing ->
                        Just layerAndColor

                    Just accColor ->
                        Just <| ( Tuple.first layerAndColor, combineColors (Tuple.second layerAndColor) (Tuple.second accColor) )
            )
            Nothing
        |> Maybe.map (\layerAndColor -> addToLayeredSvg (Tuple.first layerAndColor) (asPixel config viewPosition position pixelOffset (Tuple.second layerAndColor)) givenAcc)
        |> Maybe.withDefault givenAcc


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


asPixel : Config -> Position -> Position -> Offset -> Color -> Html msg
asPixel config viewPosition position pixelOffset color =
    let
        x =
            String.fromFloat <| (toFloat <| (position.x - viewPosition.x) * config.pixelSize) + pixelOffset.x

        y =
            String.fromFloat <| (toFloat <| (position.y - viewPosition.y) * config.pixelSize) + pixelOffset.y

        boxPosition =
            x ++ " " ++ y ++ " " ++ " " ++ zDistance
    in
    node "a-box"
        [ Attributes.attribute "position" boxPosition
        , Attributes.attribute "material" <| "color: " ++ Color.toCssString color
        ]
        []


type alias LayeredSvg msg =
    Dict Int (List (Html msg))


addToLayeredSvg : Int -> Html msg -> LayeredSvg msg -> LayeredSvg msg
addToLayeredSvg layer svg =
    Dict.update layer
        (\maybeList ->
            case maybeList of
                Nothing ->
                    Just [ svg ]

                Just list ->
                    Just <| svg :: list
        )


toSortedList : LayeredSvg msg -> List (Html msg)
toSortedList =
    Dict.values >> Util.fastConcat
