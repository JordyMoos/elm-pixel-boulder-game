module Actor.Decoder exposing (levelConfigDecoder, defaultBackground)

import Actor.Actor as Actor
    exposing
        ( LevelConfig
        , Entities
        , Signs
        , Scene
        , Images
        , Component(..)
        , Components
        , RenderComponentData(..)
        , KeyedComponent
        , MovingDownState(..)
        , PixelRenderComponentData
        , ImageRenderComponentData
        , ImagesData
        , SpawnComponentData
        , SpawnRepeat
        , SpawnRepeatTimes(..)
        , CameraComponentData
        , PhysicsComponentData
        , LifetimeComponentData
        , DamageComponentData
        , TriggerExplodableComponentData
        , CollectibleComponentData
        , CollectorComponentData
        , Inventory
        , Shape(..)
        , ControlComponentData
        , ControlSettings
        , ControlType(..)
        , WalkAroundAiControlData
        , TagComponentData
        , Subscriber
        , EventAction(..)
        , LevelFailedData
        , LevelCompletedData
        , AnimationSetup
        )
import Data.Position as Position exposing (Position)
import Data.Direction as Direction exposing (Direction)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Color exposing (Color)
import Util.Util as Util
import Dict exposing (Dict)
import Actor.EventManager as EventManager
import Util.PrimeSearch as PrimeSearch
import GameState.PlayingLevel.Animation.CurrentTick as CurrentTickAnimation
import GameState.PlayingLevel.Animation.PseudoRandomTraversal as PseudoRandomTraversalAnimation
import GameState.PlayingLevel.Animation.ReadingDirection as ReadingDirectionAnimation
import Maybe.Extra


defaultCameraBorderSize : Int
defaultCameraBorderSize =
    3


levelConfigDecoder : Decoder LevelConfig
levelConfigDecoder =
    JDP.decode LevelConfig
        |> JDP.required "entities" entitiesDecoder
        |> JDP.required "signs" signsDecoder
        |> JDP.required "scene" sceneDecoder
        |> JDP.optional "images" imagesDecoder Dict.empty
        |> JDP.optional "background" renderDataDecoder defaultBackground
        |> JDP.optional "subscribers" (Decode.list subscriberDecoder) []


defaultBackground : RenderComponentData
defaultBackground =
    PixelRenderComponent
        { colors = [ Color.white ]
        , ticksPerColor = 1
        }


entitiesDecoder : Decoder Entities
entitiesDecoder =
    Decode.dict componentsDecoder


componentsDecoder : Decoder Components
componentsDecoder =
    Decode.list componentDecoder
        |> Decode.andThen
            (\keyedComponents ->
                Decode.succeed <| Dict.fromList keyedComponents
            )


componentDecoder : Decoder KeyedComponent
componentDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\theType ->
                (case theType of
                    "control" ->
                        Decode.map ControlComponent <| Decode.field "data" controlDataDecoder

                    "camera" ->
                        Decode.map CameraComponent <| Decode.field "data" cameraDataDecoder

                    "lifetime" ->
                        Decode.map LifetimeComponent <| Decode.field "data" lifetimeDataDecoder

                    "damage" ->
                        Decode.map DamageComponent <| Decode.field "data" damageDataDecoder

                    "collectible" ->
                        Decode.map CollectibleComponent <| Decode.field "data" collectibleDecoder

                    "collector" ->
                        Decode.map CollectorComponent <| Decode.field "data" collectorDecoder

                    "explodable" ->
                        Decode.succeed ExplodableComponent

                    "physics" ->
                        Decode.map PhysicsComponent <| Decode.field "data" physicsDataDecoder

                    "render" ->
                        Decode.map RenderComponent <| Decode.field "data" renderDataDecoder

                    "rigid" ->
                        Decode.succeed RigidComponent

                    "trigger-explodable" ->
                        Decode.map TriggerExplodableComponent <| Decode.field "data" triggerExplodableDataDecoder

                    "smash-down" ->
                        Decode.succeed <| DownSmashComponent { movingDownState = NotMovingDown }

                    "spawn" ->
                        Decode.map SpawnComponent <| Decode.field "data" spawnDataDecoder

                    "tag" ->
                        Decode.map TagComponent <| Decode.field "data" tagDataDecoder

                    _ ->
                        Decode.fail <|
                            "Trying to decode component, but type "
                                ++ theType
                                ++ " is not supported"
                )
                    |> Decode.andThen
                        (\component ->
                            Decode.succeed ( theType, component )
                        )
            )


renderDataDecoder : Decoder RenderComponentData
renderDataDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\theType ->
                case theType of
                    "pixel" ->
                        Decode.map PixelRenderComponent <| Decode.field "data" renderPixelDataDecoder

                    "image" ->
                        Decode.map ImageRenderComponent <| Decode.field "data" renderImageDataDecoder

                    _ ->
                        Decode.fail <|
                            "Trying to decode render, but the type "
                                ++ theType
                                ++ " is not supported."
            )


renderPixelDataDecoder : Decoder PixelRenderComponentData
renderPixelDataDecoder =
    JDP.decode PixelRenderComponentData
        |> JDP.required "colors" (Decode.list colorDecoder)
        |> JDP.optional "ticksPerColor" Decode.int 1


renderImageDataDecoder : Decoder ImageRenderComponentData
renderImageDataDecoder =
    JDP.decode ImageRenderComponentData
        |> JDP.required "default" imagesDataDecoder
        |> JDP.optional "direction" decodeDirectionImagesData Dict.empty


type alias DirectionNames =
    { directionId : Int
    , entityNames : List String
    }


imagesDataDecoder : Decoder ImagesData
imagesDataDecoder =
    JDP.decode ImagesData
        |> JDP.required "names" (Decode.list Decode.string)
        |> JDP.optional "ticksPerImage" Decode.int 1


decodeDirectionImagesData : Decoder (Dict Int ImagesData)
decodeDirectionImagesData =
    Decode.dict imagesDataDecoder
        |> Decode.andThen
            (\dict ->
                Dict.toList dict
                    |> List.map
                        (\( directionName, imagesData ) ->
                            Direction.getIDFromKey directionName
                                |> Maybe.map
                                    (\directionId ->
                                        ( directionId, imagesData )
                                    )
                        )
                    |> Maybe.Extra.values
                    |> Dict.fromList
                    |> (\newDict ->
                            if Dict.size dict == Dict.size newDict then
                                Decode.succeed newDict
                            else
                                Decode.fail "There are invalid directions in the image data"
                       )
            )


decodeDirectionNames : Decoder DirectionNames
decodeDirectionNames =
    JDP.decode DirectionNames
        |> JDP.required "direction" directionIdDecoder
        |> JDP.required "names" (Decode.list Decode.string)


tagDataDecoder : Decoder TagComponentData
tagDataDecoder =
    JDP.decode TagComponentData
        |> JDP.required "name" Decode.string


spawnDataDecoder : Decoder SpawnComponentData
spawnDataDecoder =
    JDP.decode SpawnComponentData
        |> JDP.required "entityName" Decode.string
        |> JDP.required "position" positionDecoder
        |> JDP.optional "delayTicks" Decode.int 0
        |> JDP.optional "repeat" spawnRepeatDecoder spawnNeverRepeat


spawnRepeatDecoder : Decoder SpawnRepeat
spawnRepeatDecoder =
    JDP.decode SpawnRepeat
        |> JDP.required "times" spawnRepeatTimesDecoder
        |> JDP.required "delayTicks" Decode.int


spawnRepeatTimesDecoder : Decoder SpawnRepeatTimes
spawnRepeatTimesDecoder =
    Decode.oneOf
        [ Decode.string
            |> Decode.andThen
                (\times ->
                    case times of
                        "forever" ->
                            Decode.succeed RepeatForever

                        "never" ->
                            Decode.succeed RepeatNever

                        other ->
                            case String.toInt other of
                                Ok timesInt ->
                                    Decode.succeed <| RepeatTimes timesInt

                                Err error ->
                                    Decode.fail <|
                                        "Trying to decode spawn repeat times, but the times "
                                            ++ other
                                            ++ " should be something that can be parsed to an int."
                )
        , Decode.int
            |> Decode.andThen
                (\times ->
                    Decode.succeed <| RepeatTimes times
                )
        ]


positionDecoder : Decoder Position
positionDecoder =
    JDP.decode Position
        |> JDP.required "x" Decode.int
        |> JDP.required "y" Decode.int


cameraDataDecoder : Decoder CameraComponentData
cameraDataDecoder =
    JDP.decode CameraComponentData
        |> JDP.optional "borderSize" Decode.int defaultCameraBorderSize


physicsDataDecoder : Decoder PhysicsComponentData
physicsDataDecoder =
    JDP.decode PhysicsComponentData
        |> JDP.required "strength" Decode.int
        |> JDP.required "shape" physicsShapeDecoder


lifetimeDataDecoder : Decoder LifetimeComponentData
lifetimeDataDecoder =
    JDP.decode LifetimeComponentData
        |> JDP.required "remainingTicks" Decode.int


damageDataDecoder : Decoder DamageComponentData
damageDataDecoder =
    JDP.decode DamageComponentData
        |> JDP.required "damageStrength" Decode.int


triggerExplodableDataDecoder : Decoder TriggerExplodableComponentData
triggerExplodableDataDecoder =
    JDP.decode TriggerExplodableComponentData
        |> JDP.required "triggerStrength" Decode.int


collectibleDecoder : Decoder CollectibleComponentData
collectibleDecoder =
    JDP.decode CollectibleComponentData
        |> JDP.required "name" Decode.string
        |> JDP.optional "quantity" Decode.int 1


collectorDecoder : Decoder CollectorComponentData
collectorDecoder =
    JDP.decode CollectorComponentData
        |> JDP.required "interestedIn" (Decode.list Decode.string)
        |> JDP.optional "inventory" inventoryDecoder Dict.empty


inventoryDecoder : Decoder Inventory
inventoryDecoder =
    Decode.dict Decode.int


physicsShapeDecoder : Decoder Shape
physicsShapeDecoder =
    Decode.string
        |> Decode.andThen
            (\shape ->
                case shape of
                    "circle" ->
                        Decode.succeed Circle

                    "square" ->
                        Decode.succeed Square

                    _ ->
                        Decode.fail <|
                            "Trying to decode a physics shape, but the shape "
                                ++ shape
                                ++ " is not supported."
            )


controlDataDecoder : Decoder ControlComponentData
controlDataDecoder =
    JDP.decode ControlComponentData
        |> JDP.optional "settings" controlSettingsDecoder emptyControlSettings
        |> JDP.required "control" controlTypeDecoder


controlSettingsDecoder : Decoder ControlSettings
controlSettingsDecoder =
    JDP.decode ControlSettings
        |> JDP.optional "pushStrength" Decode.int emptyControlSettings.pushStrength
        |> JDP.optional "walkOverStrength" Decode.int emptyControlSettings.walkOverStrength


emptyControlSettings : ControlSettings
emptyControlSettings =
    { pushStrength = 0
    , walkOverStrength = 0
    }


controlTypeDecoder : Decoder ControlType
controlTypeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\theType ->
                case theType of
                    "input" ->
                        Decode.succeed InputControl

                    "walkAroundAi" ->
                        Decode.map WalkAroundAiControl <| Decode.field "data" walkAroundAiDataDecoder

                    "gravityAi" ->
                        Decode.succeed GravityAiControl

                    _ ->
                        Decode.fail <|
                            "Trying to decode control components control type, but the type "
                                ++ theType
                                ++ " is not supported."
            )


walkAroundAiDataDecoder : Decoder WalkAroundAiControlData
walkAroundAiDataDecoder =
    JDP.decode WalkAroundAiControlData
        |> JDP.optional "previousDirection" directionDecoder Direction.Left
        |> JDP.required "nextDirectionOffsets" (Decode.list Decode.int)


directionDecoder : Decoder Direction
directionDecoder =
    Decode.string
        |> Decode.andThen
            (\direction ->
                case direction of
                    "left" ->
                        Decode.succeed Direction.Left

                    "up" ->
                        Decode.succeed Direction.Up

                    "right" ->
                        Decode.succeed Direction.Right

                    "down" ->
                        Decode.succeed Direction.Down

                    _ ->
                        Decode.fail <|
                            "Trying to decode direction, but the direction "
                                ++ direction
                                ++ " is not supported. Supported directions are: left, up, right, down."
            )


directionIdDecoder : Decoder Int
directionIdDecoder =
    directionDecoder
        |> Decode.andThen
            (\direction ->
                Decode.succeed <| Direction.getIDFromDirection direction
            )


colorDecoder : Decoder Color
colorDecoder =
    Decode.string
        |> Decode.andThen
            (\stringColor ->
                case Util.hexToColor stringColor of
                    Ok color ->
                        Decode.succeed color

                    Err _ ->
                        Decode.fail <| "Failed to decode color: " ++ stringColor
            )


signsDecoder : Decoder Signs
signsDecoder =
    Decode.dict Decode.string


sceneDecoder : Decoder Scene
sceneDecoder =
    Decode.list Decode.string


imagesDecoder : Decoder Images
imagesDecoder =
    Decode.dict Decode.string


subscriberDecoder : Decoder Subscriber
subscriberDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\theType ->
                case theType of
                    "onTagDied" ->
                        Decode.field "data" onTagDiedSubscriberDecoder

                    "onInventoryUpdated" ->
                        Decode.field "data" onInventoryUpdatedSubscriberDecoder

                    _ ->
                        Decode.fail <|
                            "Trying to decode subscriber, but the type "
                                ++ theType
                                ++ " is not supported."
            )


onTagDiedSubscriberDecoder : Decoder Subscriber
onTagDiedSubscriberDecoder =
    JDP.decode EventManager.onTagDiedSubscriber
        |> JDP.required "tagName" Decode.string
        |> JDP.required "action" eventActionDecoder


onInventoryUpdatedSubscriberDecoder : Decoder Subscriber
onInventoryUpdatedSubscriberDecoder =
    JDP.decode EventManager.onInventoryUpdatedSubscriber
        |> JDP.required "interestedIn" Decode.string
        |> JDP.required "minimumQuantity" Decode.int
        |> JDP.required "action" eventActionDecoder


eventActionDecoder : Decoder EventAction
eventActionDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\theType ->
                case theType of
                    "failed" ->
                        Decode.map LevelFailed <| Decode.field "data" eventActionFailedDataDecoder

                    "completed" ->
                        Decode.map LevelCompleted <| Decode.field "data" eventActionCompletedDataDecoder

                    _ ->
                        Decode.fail <|
                            "Trying to decode subscriber action, but the type "
                                ++ theType
                                ++ " is not supported."
            )


eventActionFailedDataDecoder : Decoder LevelFailedData
eventActionFailedDataDecoder =
    JDP.decode LevelFailedData
        |> JDP.required "description" Decode.string
        |> JDP.required "entityNames" (Decode.list Decode.string)
        |> JDP.required "animation" animationSetupDecoder


eventActionCompletedDataDecoder : Decoder LevelCompletedData
eventActionCompletedDataDecoder =
    JDP.decode LevelCompletedData
        |> JDP.required "description" Decode.string
        |> JDP.required "nextLevel" Decode.string
        |> JDP.required "entityNames" (Decode.list Decode.string)
        |> JDP.required "animation" animationSetupDecoder


animationSetupDecoder : Decoder AnimationSetup
animationSetupDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\theType ->
                case theType of
                    "readingDirection" ->
                        Decode.succeed ReadingDirectionAnimation.init

                    "pseudoRandomTraversal" ->
                        Decode.field "data" pseudoRandomTraversalAnimationSetupDecoder

                    "currentTick" ->
                        Decode.succeed CurrentTickAnimation.init

                    _ ->
                        Decode.fail <|
                            "Trying to decode animation, but the type "
                                ++ theType
                                ++ " is not supported."
            )


pseudoRandomTraversalAnimationSetupDecoder : Decoder AnimationSetup
pseudoRandomTraversalAnimationSetupDecoder =
    Decode.field "coefficients" coefficientsDecoder
        |> Decode.andThen
            (\coefficients ->
                Decode.succeed <| PseudoRandomTraversalAnimation.init coefficients
            )


coefficientsDecoder : Decoder PrimeSearch.Coefficients
coefficientsDecoder =
    JDP.decode PrimeSearch.Coefficients
        |> JDP.required "a" Decode.int
        |> JDP.required "b" Decode.int
        |> JDP.required "c" Decode.int


spawnNeverRepeat : SpawnRepeat
spawnNeverRepeat =
    { times = RepeatNever
    , delayTicks = 0
    }