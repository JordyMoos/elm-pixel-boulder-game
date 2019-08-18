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
        , [ drawCamera level levelConfig ]
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


drawCamera : Level -> LevelConfig -> Html msg
drawCamera level levelConfig =
    let
        _ =
            Debug.log "camera" <| Debug.toString level.view.coordinate

        _ =
            Debug.log "config" <| Debug.toString level.config

        x =
            (level.view.coordinate.x // level.config.pixelSize) + (level.config.width // 2)

        y =
            (level.view.coordinate.y // level.config.pixelSize) + level.config.height
    in
    node "a-camera"
        [ Attributes.attribute "position" <|
            String.join " "
                [ String.fromInt x
                , String.fromInt (y * -1)
                , "10"
                ]
        , Attributes.attribute "wasd-controls" "enabled: false;"
        ]
        []


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
                                    viewPosition
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
                                    viewPosition
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
        ]


type alias RenderRequirements =
    { tick : Int
    , viewPosition : Position
    , position : Position
    , pixelOffset : Coordinate
    , render : Actor.RenderComponentData
    , transform : Actor.TransformComponentData
    , maybeTowards : Maybe Actor.MovingTowardsData
    }


drawActors : Int -> Position -> Position -> Coordinate -> Level -> LevelConfig -> List Actor.Actor -> List (Html msg) -> List (Html msg)
drawActors tick viewPosition position pixelOffset level levelConfig actors acc =
    let
        asRenderRequirements : Actor.Actor -> Maybe RenderRequirements
        asRenderRequirements actor =
            Maybe.map3
                (RenderRequirements tick viewPosition position pixelOffset)
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
        z =
            toFloat renderRequirements.render.layer / 64.0

        imageNotMovingOp : Actor.ImageObjectData -> List (Html msg)
        imageNotMovingOp imageData =
            let
                {-
                   Must handle the offset.
                   The offset is in pixels while this format understands tiles.
                   So the pixels offset must be divided by the tilesize (for example 32)
                -}
                x =
                    toFloat renderRequirements.transform.position.x - (toFloat renderRequirements.viewPosition.x / 64.0) + (toFloat renderRequirements.pixelOffset.x / 64.0)

                y =
                    toFloat renderRequirements.transform.position.y - (toFloat renderRequirements.viewPosition.y / 64.0) + (toFloat renderRequirements.pixelOffset.y / 64.0)
            in
            getImageName renderRequirements.tick imageData.default
                |> Maybe.map (renderImage x y z levelConfig.images)
                |> Maybe.map (List.append acc)
                |> Maybe.withDefault acc

        imageMovingOp : Actor.ImageObjectData -> Actor.MovingTowardsData -> List (Html msg)
        imageMovingOp imageData towardsData =
            let
                calculateWithCompletion : Int -> Int -> Int
                calculateWithCompletion a b =
                    let
                        aFloat =
                            toFloat (a * level.config.pixelSize)

                        bFloat =
                            toFloat (b * level.config.pixelSize)

                        diffFloat =
                            bFloat - aFloat

                        offset =
                            diffFloat * (towardsData.completionPercentage / 100)

                        result =
                            round <| aFloat + offset
                    in
                    result

                {-
                   Must handle the offset.
                   The offset is in pixels while this format understands tiles.
                   So the pixels offset must be divided by the tilesize (for example 32)
                -}
                x =
                    calculateWithCompletion (renderRequirements.transform.position.x - renderRequirements.viewPosition.x) (towardsData.position.x - renderRequirements.viewPosition.x) + renderRequirements.pixelOffset.x

                y =
                    calculateWithCompletion (renderRequirements.transform.position.y - renderRequirements.viewPosition.y) (towardsData.position.y - renderRequirements.viewPosition.y) + renderRequirements.pixelOffset.y

                xAsTile =
                    toFloat x / toFloat level.config.pixelSize

                yAsTile =
                    toFloat y / toFloat level.config.pixelSize
            in
            getImageNamesDataByDirection towardsData.direction imageData
                |> getImageName renderRequirements.tick
                |> Maybe.map (renderImage xAsTile yAsTile z levelConfig.images)
                |> Maybe.map (List.append acc)
                |> Maybe.withDefault acc

        pixelNotMovingOp : Actor.PixelObjectData -> List (Html msg)
        pixelNotMovingOp pixelData =
            let
                pixelElement : Html msg
                pixelElement =
                    asPixel
                        level.config
                        renderRequirements.viewPosition
                        renderRequirements.position
                        renderRequirements.pixelOffset
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
                        renderRequirements.viewPosition
                        renderRequirements.position
                        renderRequirements.pixelOffset
                        (getColor renderRequirements.tick pixelData |> withCompletionPercentage (100 - towardsData.completionPercentage))

                destinationElement : Html msg
                destinationElement =
                    asPixel
                        level.config
                        renderRequirements.viewPosition
                        towardsData.position
                        renderRequirements.pixelOffset
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


renderImage : Float -> Float -> Float -> Actor.Images -> String -> List (Html msg)
renderImage x y z images imageName =
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
                            [ String.fromFloat <| x + (toFloat image.xOffset / 64.0) - (toFloat image.width / 64.0 / 2.0)
                            , String.fromFloat <| (y + (toFloat image.yOffset / 64.0) + (toFloat image.height / 64.0 / 2.0)) * -1.0 -- 64 should be config.pixelSize
                            , String.fromFloat z
                            ]
                    , Attributes.attribute "geometry" <|
                        String.join ""
                            [ "width: "
                            , String.fromFloat <| (toFloat image.width / 64)
                            , "; height: "
                            , String.fromFloat <| (toFloat image.height / 64) -- 64 should be config.pixelSize
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


asPixel : Config -> Position -> Position -> Coordinate -> Color -> Html msg
asPixel config viewPosition position pixelOffset color =
    node "a-box"
        [ Attributes.attribute "material" <|
            String.join ""
                [ "color: "
                , Color.toCssString color
                , "; transparent: true;"
                ]
        , Attributes.attribute "position" <|
            String.join " "
                [ String.fromInt <| (position.x - viewPosition.x) * config.pixelSize + pixelOffset.x
                , String.fromInt <| ((position.y - viewPosition.y) * config.pixelSize + pixelOffset.y) * -1
                , "0"
                ]
        ]
        []
