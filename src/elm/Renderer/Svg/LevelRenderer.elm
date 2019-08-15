module Renderer.Svg.LevelRenderer exposing (renderLevel)

import Actor.Actor as Actor exposing (Level)
import Actor.Common as Common
import Actor.Component.RenderComponent as Render
import Color exposing (Color)
import Data.Config exposing (Config)
import Data.Coordinate as Coordinate exposing (Coordinate)
import Data.Direction as Direction exposing (Direction)
import Data.Position exposing (Position)
import Dict exposing (Dict)
import Html exposing (Html)
import List.Extra
import Maybe.Extra
import String
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Util.Util as Util


type alias LayeredSvg msg =
    Dict Int (List (Svg msg))


renderLevel : Int -> Level -> Actor.Images -> Html msg
renderLevel currentTick level images =
    Util.fastConcat
        [ [ drawLoadImages level.view images ]
        , drawBackgrounds currentTick level.config images level.backgrounds
        , drawLevel currentTick level images
        ]
        |> Svg.svg
            [ Attributes.width <| String.fromInt <| (level.config.width + (level.config.additionalViewBorder * 2)) * level.config.pixelSize
            , Attributes.height <| String.fromInt <| (level.config.height + (level.config.additionalViewBorder * 2)) * level.config.pixelSize
            , Attributes.x "0"
            , Attributes.y "0"
            , Attributes.version "2.0"
            ]


drawLoadImages : Actor.View -> Actor.Images -> Svg msg
drawLoadImages view images =
    Dict.toList images
        |> List.map (drawLoadImage view)
        |> Svg.defs []


drawLoadImage : Actor.View -> ( String, Actor.Image ) -> Svg msg
drawLoadImage view ( name, image ) =
    case image.imageType of
        Actor.RegularImage ->
            Svg.image
                [ Attributes.width <| String.fromInt image.width
                , Attributes.height <| String.fromInt image.height
                , Attributes.id <| "image-" ++ name
                , Attributes.xlinkHref image.path
                ]
                []

        Actor.PatternImage patternImageData ->
            Svg.pattern
                [ Attributes.width <| String.fromInt image.width
                , Attributes.height <| String.fromInt image.height
                , Attributes.id <| "pattern-" ++ name
                , Attributes.x <| getOffset view patternImageData.xOffset
                , Attributes.y <| getOffset view patternImageData.yOffset
                , Attributes.patternUnits "userSpaceOnUse"
                ]
                [ Svg.image
                    [ Attributes.width <| String.fromInt image.width
                    , Attributes.height <| String.fromInt image.height
                    , Attributes.id <| "image-" ++ name
                    , Attributes.xlinkHref image.path
                    ]
                    []
                ]


getOffset : Actor.View -> Actor.ImagePositionOffset -> String
getOffset view imagePositionOffset =
    case imagePositionOffset of
        Actor.FixedOffset intOffset ->
            String.fromInt intOffset

        Actor.MultipliedByViewX multiplier ->
            String.fromFloat <| Basics.toFloat view.coordinate.x * multiplier

        Actor.MultipliedByViewY multiplier ->
            String.fromFloat <| Basics.toFloat view.coordinate.y * multiplier


drawBackgrounds : Int -> Config -> Actor.Images -> List Actor.RenderComponentData -> List (Svg msg)
drawBackgrounds tick config images backgrounds =
    List.map
        (drawBackground tick config images)
        backgrounds
        |> Maybe.Extra.values


drawBackground : Int -> Config -> Actor.Images -> Actor.RenderComponentData -> Maybe (Svg msg)
drawBackground tick config images backgroundData =
    let
        xy =
            config.additionalViewBorder * config.pixelSize
    in
    case backgroundData.object of
        Actor.PixelRenderObject data ->
            Svg.rect
                [ Attributes.width <| String.fromInt <| config.width * config.pixelSize
                , Attributes.height <| String.fromInt <| config.height * config.pixelSize
                , Attributes.x <| String.fromInt <| xy
                , Attributes.y <| String.fromInt <| xy
                , Attributes.fill <| Color.toCssString <| getColor tick data
                ]
                []
                |> Just

        Actor.ImageRenderObject data ->
            getImageName tick data.default
                |> Maybe.andThen (imageNameToSvg xy xy images)


imageNameToSvg : Int -> Int -> Actor.Images -> String -> Maybe (Svg msg)
imageNameToSvg x y images imageName =
    Dict.get imageName images
        |> Maybe.map
            (\image ->
                case image.imageType of
                    Actor.RegularImage ->
                        Svg.use
                            [ Attributes.xlinkHref <| "#image-" ++ imageName
                            , Attributes.x <| String.fromInt <| x + image.xOffset
                            , Attributes.y <| String.fromInt <| y + image.yOffset
                            ]
                            []

                    Actor.PatternImage _ ->
                        Svg.rect
                            [ Attributes.fill <| "url(#pattern-" ++ imageName ++ ")"
                            , Attributes.width <| String.fromInt image.width
                            , Attributes.height <| String.fromInt image.height
                            , Attributes.x <| String.fromInt <| x + image.xOffset
                            , Attributes.y <| String.fromInt <| y + image.yOffset
                            ]
                            []
            )


drawLevel : Int -> Level -> Actor.Images -> List (Svg msg)
drawLevel tick level images =
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
                                    images
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
                                    images
                                    (Common.getActorsByPosition { x = x, y = y } level)
                                    innerAcc
                            )
                            acc
                )
                givenAcc
                (List.range yBasePosition yEndPosition)
    in
    Dict.empty
        |> drawEnvironment
        |> drawOtherActors
        |> toSortedList


type alias RenderRequirements =
    { tick : Int
    , viewPosition : Position
    , position : Position
    , pixelOffset : Coordinate
    , render : Actor.RenderComponentData
    , transform : Actor.TransformComponentData
    , maybeTowards : Maybe Actor.MovingTowardsData
    }


drawActors : Int -> Position -> Position -> Coordinate -> Level -> Actor.Images -> List Actor.Actor -> LayeredSvg msg -> LayeredSvg msg
drawActors tick viewPosition position pixelOffset level images actors acc =
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
            (\renderRequirements innerAcc -> drawRenderRequirements renderRequirements images level innerAcc)
            acc


drawRenderRequirements : RenderRequirements -> Actor.Images -> Level -> LayeredSvg msg -> LayeredSvg msg
drawRenderRequirements renderRequirements images level acc =
    let
        imageNotMovingOp : Actor.ImageObjectData -> LayeredSvg msg
        imageNotMovingOp imageData =
            let
                x =
                    (renderRequirements.transform.position.x - renderRequirements.viewPosition.x) * level.config.pixelSize + renderRequirements.pixelOffset.x

                y =
                    (renderRequirements.transform.position.y - renderRequirements.viewPosition.y) * level.config.pixelSize + renderRequirements.pixelOffset.y
            in
            getImageName renderRequirements.tick imageData.default
                |> Maybe.andThen (imageNameToSvg x y images)
                |> Maybe.map (addToLayeredSvgFlipped renderRequirements.render.layer acc)
                |> Maybe.withDefault acc

        imageMovingOp : Actor.ImageObjectData -> Actor.MovingTowardsData -> LayeredSvg msg
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

                x =
                    calculateWithCompletion (renderRequirements.transform.position.x - renderRequirements.viewPosition.x) (towardsData.position.x - renderRequirements.viewPosition.x) + renderRequirements.pixelOffset.x

                y =
                    calculateWithCompletion (renderRequirements.transform.position.y - renderRequirements.viewPosition.y) (towardsData.position.y - renderRequirements.viewPosition.y) + renderRequirements.pixelOffset.y
            in
            getImageNamesDataByDirection towardsData.direction imageData
                |> getImageName renderRequirements.tick
                |> Maybe.andThen (imageNameToSvg x y images)
                |> Maybe.map (addToLayeredSvgFlipped renderRequirements.render.layer acc)
                |> Maybe.withDefault acc

        pixelNotMovingOp : Actor.PixelObjectData -> LayeredSvg msg
        pixelNotMovingOp pixelData =
            let
                pixelElement : Svg msg
                pixelElement =
                    asPixel
                        level.config
                        renderRequirements.viewPosition
                        renderRequirements.position
                        renderRequirements.pixelOffset
                        (getColor renderRequirements.tick pixelData)
            in
            addToLayeredSvg renderRequirements.render.layer pixelElement acc

        pixelMovingOp : Actor.PixelObjectData -> Actor.MovingTowardsData -> LayeredSvg msg
        pixelMovingOp pixelData towardsData =
            let
                originElement : Svg msg
                originElement =
                    asPixel
                        level.config
                        renderRequirements.viewPosition
                        renderRequirements.position
                        renderRequirements.pixelOffset
                        (getColor renderRequirements.tick pixelData |> withCompletionPercentage (100 - towardsData.completionPercentage))

                destinationElement : Svg msg
                destinationElement =
                    asPixel
                        level.config
                        renderRequirements.viewPosition
                        towardsData.position
                        renderRequirements.pixelOffset
                        (getColor renderRequirements.tick pixelData |> withCompletionPercentage towardsData.completionPercentage)
            in
            acc
                |> addToLayeredSvg renderRequirements.render.layer originElement
                |> addToLayeredSvg renderRequirements.render.layer destinationElement
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


addToLayeredSvgFlipped : Int -> LayeredSvg msg -> Svg msg -> LayeredSvg msg
addToLayeredSvgFlipped layer acc svg =
    addToLayeredSvg layer svg acc


addToLayeredSvg : Int -> Svg msg -> LayeredSvg msg -> LayeredSvg msg
addToLayeredSvg layer svg =
    Dict.update layer
        (\maybeList ->
            case maybeList of
                Nothing ->
                    Just [ svg ]

                Just list ->
                    Just <| svg :: list
        )


toSortedList : LayeredSvg msg -> List (Svg msg)
toSortedList =
    Dict.values >> Util.fastConcat
