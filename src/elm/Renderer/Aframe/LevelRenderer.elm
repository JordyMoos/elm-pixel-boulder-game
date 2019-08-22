module Renderer.Aframe.LevelRenderer exposing (renderLevel)

import Actor.Actor as Actor exposing (Level, LevelConfig)
import Actor.Common as Common
import Actor.Component.RenderComponent as Render
import Color exposing (Color)
import Data.Config exposing (Config)
import Data.Coordinate as Coordinate exposing (Coordinate)
import Data.Direction as Direction exposing (Direction)
import Data.Position exposing (Position)
import Dict exposing (Dict)
import Html exposing (Html, node)
import Html.Attributes exposing (attribute)
import List.Extra
import String
import Util.Util as Util


type alias Vec3 =
    { x : Float
    , y : Float
    , z : Float
    }


type alias DrawAcc msg =
    Dict Int (List (Html msg))


renderLevel : Int -> Level -> LevelConfig -> Html msg
renderLevel currentTick level levelConfig =
    let
        elements : () -> List (Html msg)
        elements _ =
            Dict.values (drawLevel currentTick level levelConfig)
                |> List.map (node "a-entity" [])
    in
    node "a-scene" [] <|
        drawAssets levelConfig
            :: elements ()


drawAssets : LevelConfig -> Html msg
drawAssets levelConfig =
    let
        drawLoadImage : ( String, Actor.Image ) -> Html msg
        drawLoadImage ( name, image ) =
            case image.imageType of
                Actor.RegularImage ->
                    node "img"
                        [ attribute "id" <| "image-" ++ name
                        , attribute "src" image.path
                        ]
                        []

                Actor.PatternImage patternImageData ->
                    node "img"
                        [ attribute "id" <| "image-" ++ name
                        , attribute "src" image.path
                        ]
                        []

                Actor.LinkImage linkData ->
                    node "img"
                        [ attribute "id" <| "image-" ++ name
                        , attribute "src" image.path
                        ]
                        []

        drawLoadObjectAsset : ( String, String ) -> Html msg
        drawLoadObjectAsset ( name, path ) =
            node "a-asset-item"
                [ attribute "id" <| "asset-" ++ name
                , attribute "src" path
                ]
                []
    in
    Util.fastConcat
        [ Dict.toList levelConfig.images |> List.map drawLoadImage
        , Dict.toList levelConfig.objects.assets |> List.map drawLoadObjectAsset
        ]
        |> node "a-assets" []


drawCamera : Level -> Position -> Position -> DrawAcc msg -> DrawAcc msg
drawCamera level viewPositionCoordinate viewPixelOffset acc =
    let
        xOffset =
            toFloat viewPixelOffset.x / toFloat level.config.pixelSize

        yOffset =
            toFloat viewPixelOffset.y / toFloat level.config.pixelSize

        x =
            toFloat viewPositionCoordinate.x + (toFloat level.config.width / 2.0) - xOffset

        y =
            toFloat viewPositionCoordinate.y + (toFloat level.config.height / 2.0) - yOffset

        cameraNode =
            node "a-camera"
                [ attribute "position" <|
                    String.join " "
                        [ String.fromFloat x
                        , String.fromFloat (y * -1)
                        , "7"
                        ]
                , attribute "wasd-controls" "enabled: false;"
                ]
                []
    in
    addToDrawAcc 0 cameraNode acc


addToDrawAcc : Int -> Html msg -> DrawAcc msg -> DrawAcc msg
addToDrawAcc key node =
    Dict.update
        key
        (\maybeCurrentData ->
            case maybeCurrentData of
                Just currentData ->
                    Just <| node :: currentData

                Nothing ->
                    Just [ node ]
        )


drawLevel : Int -> Level -> LevelConfig -> DrawAcc msg
drawLevel tick level levelConfig =
    let
        view =
            level.view

        xPixelOffset =
            modBy level.config.pixelSize view.coordinate.x

        yPixelOffset =
            modBy level.config.pixelSize view.coordinate.y

        viewPixelOffset =
            { x = xPixelOffset * -1
            , y = yPixelOffset * -1
            }

        xBasePosition =
            Coordinate.pixelToTile level.config.pixelSize view.coordinate.x - level.config.additionalViewBorder

        yBasePosition =
            Coordinate.pixelToTile level.config.pixelSize view.coordinate.y - level.config.additionalViewBorder

        viewPositionCoordinate =
            { x =
                if view.coordinate.x < 0 && viewPixelOffset.x /= 0 then
                    xBasePosition - 1 + level.config.additionalViewBorder

                else
                    xBasePosition + level.config.additionalViewBorder
            , y =
                if view.coordinate.y < 0 && viewPixelOffset.y /= 0 then
                    yBasePosition - 1 + level.config.additionalViewBorder

                else
                    yBasePosition + level.config.additionalViewBorder
            }

        xEndPosition =
            xBasePosition + level.config.width + (level.config.additionalViewBorder * 2)

        yEndPosition =
            yBasePosition + level.config.height + (level.config.additionalViewBorder * 2)

        drawBackgrounds : DrawAcc msg -> DrawAcc msg
        drawBackgrounds givenAcc =
            let
                position =
                    { x = (xEndPosition - xBasePosition) // 2 + xBasePosition
                    , y = (yEndPosition - yBasePosition) // 2 + yBasePosition
                    }

                xOffset =
                    toFloat viewPixelOffset.x / toFloat level.config.pixelSize

                yOffset =
                    toFloat viewPixelOffset.y / toFloat level.config.pixelSize
            in
            List.foldr
                (\backgroundRenderComponentData innerAcc ->
                    drawRenderRequirements
                        (RenderRequirements
                            tick
                            backgroundRenderComponentData
                            (Actor.TransformComponentData position)
                            Nothing
                        )
                        levelConfig
                        level
                        innerAcc
                )
                givenAcc
                levelConfig.backgrounds

        drawEnvironment : DrawAcc msg -> DrawAcc msg
        drawEnvironment givenAcc =
            List.foldr
                (\y acc ->
                    List.range (xBasePosition - level.config.additionalEnvironment) xEndPosition
                        |> List.foldr
                            (\x innerAcc ->
                                drawActors
                                    tick
                                    level
                                    levelConfig
                                    (Common.getEnvironmentActorsByPosition { x = x, y = y } level)
                                    innerAcc
                            )
                            acc
                )
                givenAcc
                (List.range (yBasePosition - level.config.additionalEnvironment) yEndPosition)

        drawOtherActors : DrawAcc msg -> DrawAcc msg
        drawOtherActors givenAcc =
            List.foldr
                (\y acc ->
                    List.range xBasePosition xEndPosition
                        |> List.foldr
                            (\x innerAcc ->
                                drawActors
                                    tick
                                    level
                                    levelConfig
                                    (Common.getActorsByPosition { x = x, y = y } level)
                                    innerAcc
                            )
                            acc
                )
                givenAcc
                (List.range yBasePosition yEndPosition)
    in
    Dict.fromList []
        |> drawCamera level viewPositionCoordinate viewPixelOffset
        |> drawBackgrounds
        |> drawEnvironment
        |> drawOtherActors


type alias RenderRequirements =
    { tick : Int
    , render : Actor.RenderComponentData
    , transform : Actor.TransformComponentData
    , maybeTowards : Maybe Actor.MovingTowardsData
    }


drawActors : Int -> Level -> LevelConfig -> List Actor.Actor -> DrawAcc msg -> DrawAcc msg
drawActors tick level levelConfig actors acc =
    let
        asRenderRequirements : Actor.Actor -> Maybe RenderRequirements
        asRenderRequirements actor =
            Maybe.map3
                (RenderRequirements tick)
                (Render.getRenderComponent actor)
                (Common.getTransformComponent actor)
                (Common.getMovementComponent actor
                    |> Maybe.map Common.getMovingTowardsData
                    |> Maybe.withDefault Nothing
                    |> Just
                )
    in
    actors
        |> List.filterMap asRenderRequirements
        |> List.foldr
            (\renderRequirements innerAcc -> drawRenderRequirements renderRequirements levelConfig level innerAcc)
            acc


drawRenderRequirements : RenderRequirements -> LevelConfig -> Level -> DrawAcc msg -> DrawAcc msg
drawRenderRequirements renderRequirements levelConfig level acc =
    let
        pixelSize : Float
        pixelSize =
            toFloat level.config.pixelSize

        xPoint =
            toFloat renderRequirements.transform.position.x

        yPoint =
            toFloat renderRequirements.transform.position.y

        zPoint =
            0.0

        imageNotMovingOp : Actor.ImageTypeData -> DrawAcc msg
        imageNotMovingOp imageData =
            getImageName renderRequirements.tick imageData.default
                |> Maybe.map
                    (\imageName ->
                        renderImage renderRequirements pixelSize xPoint yPoint zPoint levelConfig.images imageName acc
                    )
                |> Maybe.withDefault acc

        imageMovingOp : Actor.ImageTypeData -> Actor.MovingTowardsData -> DrawAcc msg
        imageMovingOp imageData towardsData =
            let
                xDestPoint =
                    toFloat towardsData.position.x

                yDestPoint =
                    toFloat towardsData.position.y

                asMovementLocation : Float -> Float -> Float -> Float
                asMovementLocation xCurrent xDest completion =
                    (xDest - xCurrent) / 100.0 * completion + xCurrent

                xFinal =
                    asMovementLocation xPoint xDestPoint towardsData.completionPercentage

                yFinal =
                    asMovementLocation yPoint yDestPoint towardsData.completionPercentage
            in
            getImageNamesDataByDirection towardsData.direction imageData
                |> getImageName renderRequirements.tick
                |> Maybe.map
                    (\imageName ->
                        renderImage renderRequirements pixelSize xFinal yFinal zPoint levelConfig.images imageName acc
                    )
                |> Maybe.withDefault acc

        pixelNotMovingOp : Actor.PixelTypeData -> DrawAcc msg
        pixelNotMovingOp pixelData =
            asPixel
                level.config
                renderRequirements
                xPoint
                yPoint
                (getColor renderRequirements.tick pixelData)
                acc

        pixelMovingOp : Actor.PixelTypeData -> Actor.MovingTowardsData -> DrawAcc msg
        pixelMovingOp pixelData towardsData =
            let
                xDestPoint =
                    toFloat towardsData.position.x

                yDestPoint =
                    toFloat towardsData.position.y

                originElement : DrawAcc msg -> DrawAcc msg
                originElement givenAcc =
                    asPixel
                        level.config
                        renderRequirements
                        xPoint
                        yPoint
                        (getColor renderRequirements.tick pixelData |> withCompletionPercentage (100 - towardsData.completionPercentage))
                        givenAcc

                destinationElement : DrawAcc msg -> DrawAcc msg
                destinationElement givenAcc =
                    asPixel
                        level.config
                        renderRequirements
                        xDestPoint
                        yDestPoint
                        (getColor renderRequirements.tick pixelData |> withCompletionPercentage towardsData.completionPercentage)
                        givenAcc
            in
            acc
                |> originElement
                |> destinationElement

        objectNotMovingOp : Actor.ObjectTypeData -> DrawAcc msg
        objectNotMovingOp objectData =
            drawObject renderRequirements { x = xPoint, y = yPoint, z = zPoint } levelConfig.objects.presets objectData.default acc

        objectMovingOp : Actor.ObjectTypeData -> Actor.MovingTowardsData -> DrawAcc msg
        objectMovingOp objectData towardsData =
            let
                xDestPoint =
                    toFloat towardsData.position.x

                yDestPoint =
                    toFloat towardsData.position.y

                asMovementLocation : Float -> Float -> Float -> Float
                asMovementLocation xCurrent xDest completion =
                    (xDest - xCurrent) / 100.0 * completion + xCurrent

                xFinal =
                    asMovementLocation xPoint xDestPoint towardsData.completionPercentage

                yFinal =
                    asMovementLocation yPoint yDestPoint towardsData.completionPercentage
            in
            getPresetNameByDirection towardsData.direction objectData
                |> (\presetName -> drawObject renderRequirements { x = xFinal, y = yFinal, z = zPoint } levelConfig.objects.presets presetName acc)
    in
    case ( renderRequirements.render.renderType, renderRequirements.maybeTowards ) of
        ( Actor.PixelRenderType pixelData, Nothing ) ->
            pixelNotMovingOp pixelData

        ( Actor.PixelRenderType pixelData, Just towardsData ) ->
            pixelMovingOp pixelData towardsData

        ( Actor.ImageRenderType imageData, Nothing ) ->
            imageNotMovingOp imageData

        ( Actor.ImageRenderType imageData, Just towardsData ) ->
            imageMovingOp imageData towardsData

        ( Actor.ObjectRenderType objectData, Nothing ) ->
            objectNotMovingOp objectData

        ( Actor.ObjectRenderType objectData, Just towardsData ) ->
            objectMovingOp objectData towardsData


drawObject : RenderRequirements -> Vec3 -> Actor.ObjectPresets -> Actor.ObjectPresetName -> DrawAcc msg -> DrawAcc msg
drawObject renderRequirements position presets presetName acc =
    Dict.get presetName presets
        |> Maybe.map
            (\preset ->
                drawObjectPreset renderRequirements position preset acc
            )
        |> Maybe.withDefault acc


drawObjectPreset : RenderRequirements -> Vec3 -> Actor.ObjectPresetData -> DrawAcc msg -> DrawAcc msg
drawObjectPreset renderRequirements position preset =
    let
        element =
            node "a-entity"
                (List.append
                    [ attribute "position" <|
                        String.join " "
                            [ String.fromFloat (position.x + preset.xOffset)
                            , String.fromFloat ((position.y + preset.yOffset) * -1)
                            , String.fromFloat (position.z + preset.zOffset)
                            ]
                    ]
                    (preset.settings
                        |> Dict.toList
                        |> List.map
                            (\( settingKey, settingData ) ->
                                attribute settingKey settingData
                            )
                    )
                )
                []
    in
    addToDrawAcc renderRequirements.render.layer element


renderImage : RenderRequirements -> Float -> Float -> Float -> Float -> Actor.Images -> String -> DrawAcc msg -> DrawAcc msg
renderImage renderRequirements pixelSize x y z images imageName acc =
    let
        asImage : Actor.Image -> List (Html.Attribute msg) -> DrawAcc msg
        asImage image additionalAttributes =
            let
                element =
                    node "a-image"
                        (List.append
                            [ attribute "material" <|
                                String.join ""
                                    [ "src: #image-"
                                    , imageName
                                    , "; transparent: true;"
                                    ]
                            , attribute "position" <|
                                String.join " "
                                    [ String.fromFloat <| x + (toFloat image.xOffset / pixelSize) + (toFloat image.width / pixelSize / 2.0)
                                    , String.fromFloat <| (y + (toFloat image.yOffset / pixelSize) + (toFloat image.height / pixelSize / 2.0)) * -1.0
                                    , String.fromFloat z
                                    ]
                            , attribute "geometry" <|
                                String.join ""
                                    [ "width: "
                                    , String.fromFloat <| (toFloat image.width / pixelSize)
                                    , "; height: "
                                    , String.fromFloat <| (toFloat image.height / pixelSize)
                                    , ";"
                                    ]
                            ]
                            additionalAttributes
                        )
                        []
            in
            addToDrawAcc renderRequirements.render.layer element acc
    in
    Dict.get imageName images
        |> Maybe.map
            (\image ->
                case image.imageType of
                    Actor.RegularImage ->
                        asImage image []

                    Actor.PatternImage _ ->
                        asImage image []

                    Actor.LinkImage linkData ->
                        asImage image
                            [ attribute "link" <|
                                String.join ""
                                    [ "href: "
                                    , linkData.href
                                    , ";"
                                    ]
                            ]
            )
        |> Maybe.withDefault acc


withCompletionPercentage : Float -> Color -> Color
withCompletionPercentage completionPercentage color =
    let
        rgba =
            Color.toRgba color

        updatedAlpha =
            { rgba | alpha = rgba.alpha / 100.0 * completionPercentage }
    in
    Color.fromRgba updatedAlpha


getPresetNameByDirection : Direction -> Actor.ObjectTypeData -> Actor.ObjectPresetName
getPresetNameByDirection direction objectData =
    Direction.getIDFromDirection direction
        |> (\a -> Dict.get a objectData.direction)
        |> Maybe.withDefault objectData.default


getImageNamesDataByDirection : Direction -> Actor.ImageTypeData -> Actor.ImagesData
getImageNamesDataByDirection direction imageRenderData =
    Direction.getIDFromDirection direction
        |> (\a -> Dict.get a imageRenderData.direction)
        |> Maybe.withDefault imageRenderData.default


getColor : Int -> Actor.PixelTypeData -> Color
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


asPixel : Config -> RenderRequirements -> Float -> Float -> Color -> DrawAcc msg -> DrawAcc msg
asPixel config renderRequirements xPoint yPoint color acc =
    let
        rgba =
            Color.toRgba color

        asCssString : String
        asCssString =
            let
                pct x =
                    ((x * 10000) |> round |> toFloat)
                        / 100
                        |> round
            in
            String.concat
                [ "rgb("
                , String.fromInt (pct rgba.red)
                , "%,"
                , String.fromInt (pct rgba.green)
                , "%,"
                , String.fromInt (pct rgba.blue)
                , "%)"
                ]

        element =
            node "a-box"
                [ attribute "material" <|
                    String.join ""
                        [ "color: "
                        , asCssString
                        , "; transparent: true;"
                        , "opacity: "
                        , String.fromFloat rgba.alpha
                        , ";"
                        ]
                , attribute "position" <|
                    String.join " "
                        [ String.fromFloat xPoint
                        , String.fromFloat (yPoint * -1)
                        , "0"
                        ]
                ]
                []
    in
    addToDrawAcc renderRequirements.render.layer element acc
