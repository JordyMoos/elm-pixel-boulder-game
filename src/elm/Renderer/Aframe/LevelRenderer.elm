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
import Html.Attributes as Attributes
import List.Extra
import Maybe.Extra
import String
import Util.Util as Util


renderLevel : Int -> Level -> LevelConfig -> Html msg
renderLevel currentTick level levelConfig =
    Util.fastConcat
        [ [ drawAssets levelConfig ]
        , drawLevel currentTick level levelConfig
        ]
        |> node "a-scene" []


drawAssets : LevelConfig -> Html msg
drawAssets levelConfig =
    Dict.toList levelConfig.images
        |> List.map drawLoadImage
        |> node "a-assets" []


drawLoadImage : ( String, Actor.Image ) -> Html msg
drawLoadImage ( name, image ) =
    case image.imageType of
        Actor.RegularImage ->
            node "img"
                [ Attributes.id <| "image-" ++ name
                , Attributes.src image.path
                ]
                []

        Actor.PatternImage patternImageData ->
            node "img"
                [ Attributes.id <| "image-" ++ name
                , Attributes.src image.path
                ]
                []

        Actor.LinkImage linkData ->
            node "img"
                [ Attributes.id <| "image-" ++ name
                , Attributes.src image.path
                ]
                []


drawCamera : Level -> LevelConfig -> Position -> Position -> Html msg
drawCamera level levelConfig viewPositionCoordinate viewPixelOffset =
    let
        x =
            (toFloat level.view.coordinate.x / toFloat level.config.pixelSize) + (toFloat level.config.width / 2.0)

        y =
            (toFloat level.view.coordinate.y / toFloat level.config.pixelSize) + (toFloat level.config.height / 2.0)

        xOffset =
            toFloat viewPixelOffset.x / toFloat level.config.pixelSize

        yOffset =
            toFloat viewPixelOffset.y / toFloat level.config.pixelSize

        x2 =
            toFloat viewPositionCoordinate.x + (toFloat level.config.width / 2.0) - xOffset

        y2 =
            toFloat viewPositionCoordinate.y + (toFloat level.config.height / 2.0) - yOffset
    in
    node "a-camera"
        [ Attributes.attribute "position" <|
            String.join " "
                [ String.fromFloat x2
                , String.fromFloat (y2 * -1)
                , "7"
                ]
        , Attributes.attribute "wasd-controls" "enabled: false;"
        ]
        []



--    node "a-camera"
--        [ Attributes.attribute "position" <|
--            String.join " "
--                [ String.fromFloat x
--                , String.fromFloat (y * -1)
--                , "20"
--                ]
--        , Attributes.attribute "wasd-controls" "enabled: false;"
--        ]
--        []


drawLevel : Int -> Level -> LevelConfig -> List (Html msg)
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
            xBasePosition + level.config.width + (level.config.additionalViewBorder * 2)

        yEndPosition =
            yBasePosition + level.config.height + (level.config.additionalViewBorder * 2)

        drawEnvironment givenAcc =
            List.foldr
                (\y acc ->
                    List.range (xBasePosition - level.config.additionalEnvironment) xEndPosition
                        |> List.foldr
                            (\x innerAcc ->
                                drawActors
                                    tick
                                    viewPositionCoordinate
                                    { x = x, y = y }
                                    viewPixelOffset
                                    level
                                    levelConfig
                                    (Common.getEnvironmentActorsByPosition { x = x, y = y } level)
                                    innerAcc
                            )
                            acc
                )
                givenAcc
                (List.range (yBasePosition - level.config.additionalEnvironment) yEndPosition)

        drawOtherActors givenAcc =
            List.foldr
                (\y acc ->
                    List.range xBasePosition xEndPosition
                        |> List.foldr
                            (\x innerAcc ->
                                drawActors
                                    tick
                                    viewPositionCoordinate
                                    { x = x, y = y }
                                    viewPixelOffset
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
    Util.fastConcat
        [ drawEnvironment []
        , drawOtherActors []
        , [ drawCamera level levelConfig viewPositionCoordinate viewPixelOffset ]
        ]


type alias RenderRequirements =
    { tick : Int
    , viewPositionCoordinate : Position
    , position : Position
    , pixelOffset : Coordinate
    , render : Actor.RenderComponentData
    , transform : Actor.TransformComponentData
    , maybeTowards : Maybe Actor.MovingTowardsData
    }


drawActors : Int -> Position -> Position -> Coordinate -> Level -> LevelConfig -> List Actor.Actor -> List (Html msg) -> List (Html msg)
drawActors tick viewPositionCoordinate position pixelOffset level levelConfig actors acc =
    let
        asRenderRequirements : Actor.Actor -> Maybe RenderRequirements
        asRenderRequirements actor =
            Maybe.map3
                (RenderRequirements tick viewPositionCoordinate position pixelOffset)
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


drawRenderRequirements : RenderRequirements -> LevelConfig -> Level -> List (Html msg) -> List (Html msg)
drawRenderRequirements renderRequirements levelConfig level acc =
    let
        pixelSize : Float
        pixelSize =
            toFloat level.config.pixelSize

        asXPoint : Int -> Float
        asXPoint givenX =
            toFloat givenX

        -- toFloat renderRequirements.viewPositionCoordinate.x + (toFloat renderRequirements.pixelOffset.x / pixelSize)
        asYPoint : Int -> Float
        asYPoint givenY =
            toFloat givenY

        -- toFloat renderRequirements.viewPositionCoordinate.y + (toFloat renderRequirements.pixelOffset.y / pixelSize)
        xPoint =
            asXPoint renderRequirements.transform.position.x

        yPoint =
            asYPoint renderRequirements.transform.position.y

        zPoint =
            toFloat renderRequirements.render.layer / pixelSize

        imageNotMovingOp : Actor.ImageObjectData -> List (Html msg)
        imageNotMovingOp imageData =
            getImageName renderRequirements.tick imageData.default
                |> Maybe.map (renderImage pixelSize xPoint yPoint zPoint levelConfig.images)
                |> Maybe.map (List.append acc)
                |> Maybe.withDefault acc

        imageMovingOp : Actor.ImageObjectData -> Actor.MovingTowardsData -> List (Html msg)
        imageMovingOp imageData towardsData =
            let
                xDestPoint =
                    asXPoint towardsData.position.x

                yDestPoint =
                    asYPoint towardsData.position.y

                asMovementLocation : Float -> Float -> Float -> Float
                asMovementLocation xCurrent xDest completion =
                    (xDest - xCurrent) / 100.0 * completion + xCurrent

                xFinal =
                    asMovementLocation xPoint xDestPoint towardsData.completionPercentage

                xFinal2 =
                    xFinal - (toFloat renderRequirements.pixelOffset.x / pixelSize)

                yFinal =
                    asMovementLocation yPoint yDestPoint towardsData.completionPercentage

                yFinal2 =
                    yFinal - (toFloat renderRequirements.pixelOffset.y / pixelSize)
            in
            getImageNamesDataByDirection towardsData.direction imageData
                |> getImageName renderRequirements.tick
                |> Maybe.map (renderImage pixelSize xFinal yFinal zPoint levelConfig.images)
                |> Maybe.map (List.append acc)
                |> Maybe.withDefault acc

        pixelNotMovingOp : Actor.PixelObjectData -> List (Html msg)
        pixelNotMovingOp pixelData =
            let
                pixelElement : Html msg
                pixelElement =
                    asPixel
                        level.config
                        xPoint
                        yPoint
                        (getColor renderRequirements.tick pixelData)
            in
            pixelElement :: acc

        pixelMovingOp : Actor.PixelObjectData -> Actor.MovingTowardsData -> List (Html msg)
        pixelMovingOp pixelData towardsData =
            let
                originElement : Html msg
                originElement =
                    asPixel
                        level.config
                        xPoint
                        yPoint
                        (getColor renderRequirements.tick pixelData |> withCompletionPercentage (100 - towardsData.completionPercentage))

                destinationElement : Html msg
                destinationElement =
                    asPixel
                        level.config
                        xPoint
                        yPoint
                        (getColor renderRequirements.tick pixelData |> withCompletionPercentage towardsData.completionPercentage)
            in
            originElement :: destinationElement :: acc
    in
    case ( renderRequirements.render.object, renderRequirements.maybeTowards ) of
        ( Actor.PixelRenderObject pixelData, Nothing ) ->
            pixelNotMovingOp pixelData

        ( Actor.PixelRenderObject pixelData, Just towardsData ) ->
            pixelMovingOp pixelData towardsData

        ( Actor.ImageRenderObject imageData, Nothing ) ->
            imageNotMovingOp imageData

        ( Actor.ImageRenderObject imageData, Just towardsData ) ->
            imageMovingOp imageData towardsData


renderImage : Float -> Float -> Float -> Float -> Actor.Images -> String -> List (Html msg)
renderImage pixelSize x y z images imageName =
    let
        asImage : Actor.Image -> List (Html.Attribute msg) -> Html msg
        asImage image additionalAttributes =
            node "a-image"
                (List.append
                    [ Attributes.attribute "material" <|
                        String.join ""
                            [ "src: #image-"
                            , imageName
                            , "; transparent: true;"
                            ]
                    , Attributes.attribute "position" <|
                        String.join " "
                            [ String.fromFloat <| x + (toFloat image.xOffset / pixelSize) + (toFloat image.width / pixelSize / 2.0)
                            , String.fromFloat <| (y + (toFloat image.yOffset / pixelSize) + (toFloat image.height / pixelSize / 2.0)) * -1.0
                            , String.fromFloat z
                            ]
                    , Attributes.attribute "geometry" <|
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
                            [ Attributes.attribute "link" <|
                                String.join ""
                                    [ "href: "
                                    , linkData.href
                                    , ";"
                                    ]
                            ]
            )
        |> Maybe.Extra.toList


withCompletionPercentage : Float -> Color -> Color
withCompletionPercentage completionPercentage color =
    let
        rgba =
            Color.toRgba color

        updatedAlpha =
            { rgba | alpha = rgba.alpha / 100.0 * completionPercentage }
    in
    Color.fromRgba updatedAlpha


getImageNamesDataByDirection : Direction -> Actor.ImageObjectData -> Actor.ImagesData
getImageNamesDataByDirection direction imageRenderData =
    Direction.getIDFromDirection direction
        |> (\a -> Dict.get a imageRenderData.direction)
        |> Maybe.withDefault imageRenderData.default


getColor : Int -> Actor.PixelObjectData -> Color
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


asPixel : Config -> Float -> Float -> Color -> Html msg
asPixel config xPoint yPoint color =
    node "a-box"
        [ Attributes.attribute "material" <|
            String.join ""
                [ "color: "
                , toCssString color
                , "; transparent: true;"
                ]
        , Attributes.attribute "position" <|
            String.join " "
                [ String.fromFloat xPoint
                , String.fromFloat (yPoint * -1)
                , "0"
                ]
        ]
        []


toCssString : Color -> String
toCssString color =
    let
        rgba =
            Color.toRgba color

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
