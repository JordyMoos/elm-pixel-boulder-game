module Actor.Decoder exposing (defaultBackground, levelConfigDecoder)

import Actor.Actor as Actor
    exposing
        ( AdventAiData
        , AiComponentData
        , AiType(..)
        , AnimationSetup
        , AreaComponentData
        , AttackComponentData
        , BecomeActorLifetimeActionData
        , CameraComponentData
        , CollectibleComponentData
        , CollectorComponentData
        , Component(..)
        , Components
        , ControlComponentData
        , ControlSettings
        , ControlType(..)
        , CounterComponentData
        , DamageComponentData
        , Entities
        , EventAction(..)
        , GameOfLifeAiAction
        , GameOfLifeAiData
        , HealthComponentData
        , ImageRenderComponentData
        , Images
        , ImagesData
        , Inventory
        , InventoryUpdatedSubscriberData
        , KeyedComponent
        , LevelCompletedData
        , LevelConfig
        , LevelFailedData
        , LevelFinishedDescriptionProvider(..)
        , LifetimeAction(..)
        , LifetimeComponentData
        , MovementComponentData
        , MovingDownState(..)
        , MovingState(..)
        , PhysicsComponentData
        , PixelRenderComponentData
        , RenderComponentData(..)
        , Scene
        , Shape(..)
        , Signs
        , SpawnComponentData
        , SpawnRepeat
        , SpawnRepeatTimes(..)
        , Subscriber
        , TagComponentData
        , TagDiedSubscriberData
        , TriggerExplodableComponentData
        , WalkAroundAiControlData
        )
import Color exposing (Color)
import Data.Coordinate exposing (Coordinate)
import Data.Direction as Direction exposing (Direction)
import Data.Position as Position exposing (Position)
import Dict exposing (Dict)
import GameState.PlayingLevel.Animation.CurrentTick as CurrentTickAnimation
import GameState.PlayingLevel.Animation.PseudoRandomTraversal as PseudoRandomTraversalAnimation
import GameState.PlayingLevel.Animation.ReadingDirection as ReadingDirectionAnimation
import GameState.PlayingLevel.Animation.Skip as SkipAnimation
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Maybe.Extra
import Util.PrimeSearch as PrimeSearch


defaultCameraBorderSize : Int
defaultCameraBorderSize =
    3


levelConfigDecoder : Decoder LevelConfig
levelConfigDecoder =
    Decode.succeed LevelConfig
        |> JDP.required "entities" entitiesDecoder
        |> JDP.optional "signLength" Decode.int 1
        |> JDP.required "signs" signsDecoder
        |> JDP.required "scene" sceneDecoder
        |> JDP.optional "viewCoordinate" coordinateDecoder defaultViewCoordinate
        |> JDP.optional "updateBorder" Decode.int defaultUpdateBorder
        |> JDP.optional "images" imagesDecoder Dict.empty
        |> JDP.optional "background" renderDataDecoder defaultBackground
        |> JDP.optional "subscribers" (Decode.list subscriberDecoder) []


defaultBackground : RenderComponentData
defaultBackground =
    PixelRenderComponent
        { colors = [ Color.white ]
        , ticksPerColor = 1
        , layer = 0
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
                    "ai" ->
                        Decode.map AiComponent <| Decode.field "data" aiDataDecoder

                    "zarea" ->
                        Decode.map AreaComponent <| Decode.field "data" areaDataDecoder

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

                    "health" ->
                        Decode.map HealthComponent <| Decode.field "data" healthDataDecoder

                    "attack" ->
                        Decode.map AttackComponent <| Decode.field "data" attackDataDecoder

                    "counter" ->
                        Decode.map CounterComponent <| Decode.field "data" counterDataDecoder

                    "movement" ->
                        Decode.map MovementComponent <| Decode.field "data" movementDataDecoder

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
    Decode.succeed PixelRenderComponentData
        |> JDP.required "colors" (Decode.list colorDecoder)
        |> JDP.optional "ticksPerColor" Decode.int 1
        |> JDP.optional "layer" Decode.int 1


renderImageDataDecoder : Decoder ImageRenderComponentData
renderImageDataDecoder =
    Decode.succeed ImageRenderComponentData
        |> JDP.required "default" imagesDataDecoder
        |> JDP.optional "direction" decodeDirectionImagesData Dict.empty
        |> JDP.optional "layer" Decode.int 1


type alias DirectionNames =
    { directionId : Int
    , entityNames : List String
    }


imagesDataDecoder : Decoder ImagesData
imagesDataDecoder =
    Decode.succeed ImagesData
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
    Decode.succeed DirectionNames
        |> JDP.required "direction" directionIdDecoder
        |> JDP.required "names" (Decode.list Decode.string)


tagDataDecoder : Decoder TagComponentData
tagDataDecoder =
    Decode.succeed TagComponentData
        |> JDP.required "name" Decode.string


healthDataDecoder : Decoder HealthComponentData
healthDataDecoder =
    Decode.succeed HealthComponentData
        |> JDP.required "health" Decode.int
        |> JDP.custom (Decode.at [ "health" ] Decode.int)


attackDataDecoder : Decoder AttackComponentData
attackDataDecoder =
    Decode.succeed AttackComponentData
        |> JDP.required "power" Decode.int


counterDataDecoder : Decoder CounterComponentData
counterDataDecoder =
    Decode.succeed CounterComponentData
        |> JDP.optional "count" Decode.int 0


movementDataDecoder : Decoder MovementComponentData
movementDataDecoder =
    Decode.succeed MovementComponentData
        |> JDP.optional "movingTicks" Decode.int 0
        |> JDP.hardcoded 0
        |> JDP.hardcoded NotMoving


spawnDataDecoder : Decoder SpawnComponentData
spawnDataDecoder =
    Decode.succeed SpawnComponentData
        |> JDP.required "entityName" Decode.string
        |> JDP.required "position" positionDecoder
        |> JDP.optional "delayTicks" Decode.int 0
        |> JDP.optional "repeat" spawnRepeatDecoder spawnNeverRepeat


spawnRepeatDecoder : Decoder SpawnRepeat
spawnRepeatDecoder =
    Decode.succeed SpawnRepeat
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
                                Just timesInt ->
                                    Decode.succeed <| RepeatTimes timesInt

                                _ ->
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


coordinateDecoder : Decoder Coordinate
coordinateDecoder =
    Decode.succeed Coordinate
        |> JDP.required "x" Decode.int
        |> JDP.required "y" Decode.int


positionDecoder : Decoder Position
positionDecoder =
    Decode.succeed Position
        |> JDP.required "x" Decode.int
        |> JDP.required "y" Decode.int


cameraDataDecoder : Decoder CameraComponentData
cameraDataDecoder =
    Decode.succeed CameraComponentData
        |> JDP.optional "borderSize" Decode.int defaultCameraBorderSize


physicsDataDecoder : Decoder PhysicsComponentData
physicsDataDecoder =
    Decode.succeed PhysicsComponentData
        |> JDP.required "strength" Decode.int
        |> JDP.required "shape" physicsShapeDecoder


lifetimeDataDecoder : Decoder LifetimeComponentData
lifetimeDataDecoder =
    Decode.succeed LifetimeComponentData
        |> JDP.required "remainingTicks" Decode.int
        |> JDP.optional "action" lifetimeAction RemoveActorLifetimeAction


lifetimeAction : Decoder LifetimeAction
lifetimeAction =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\action ->
                case action of
                    "remove" ->
                        Decode.succeed RemoveActorLifetimeAction

                    "become" ->
                        Decode.map BecomeActorLifetimeAction <| Decode.field "data" becomeActorLifetimeDecoder

                    _ ->
                        Decode.fail <|
                            "Trying to decode a lifetime action, but the action "
                                ++ action
                                ++ " is not supported."
            )


becomeActorLifetimeDecoder : Decoder BecomeActorLifetimeActionData
becomeActorLifetimeDecoder =
    Decode.succeed BecomeActorLifetimeActionData
        |> JDP.required "entityName" Decode.string


damageDataDecoder : Decoder DamageComponentData
damageDataDecoder =
    Decode.succeed DamageComponentData
        |> JDP.required "damageStrength" Decode.int


triggerExplodableDataDecoder : Decoder TriggerExplodableComponentData
triggerExplodableDataDecoder =
    Decode.succeed TriggerExplodableComponentData
        |> JDP.required "triggerStrength" Decode.int


collectibleDecoder : Decoder CollectibleComponentData
collectibleDecoder =
    Decode.succeed CollectibleComponentData
        |> JDP.required "name" Decode.string
        |> JDP.optional "quantity" Decode.int 1


collectorDecoder : Decoder CollectorComponentData
collectorDecoder =
    Decode.succeed CollectorComponentData
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


aiDataDecoder : Decoder AiComponentData
aiDataDecoder =
    Decode.succeed AiComponentData
        |> JDP.required "ai" aiTypeDecoder


aiTypeDecoder : Decoder AiType
aiTypeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\theType ->
                case theType of
                    "gameOfLifeAi" ->
                        Decode.map GameOfLifeAi <| Decode.field "data" gameOfLifeAiDataDecoder

                    "advent" ->
                        Decode.map AdventAi <| Decode.field "data" adventAiDataDecoder

                    _ ->
                        Decode.fail <|
                            "Trying to decode ai components ai type, but the type "
                                ++ theType
                                ++ " is not supported."
            )


gameOfLifeAiDataDecoder : Decoder GameOfLifeAiData
gameOfLifeAiDataDecoder =
    Decode.succeed GameOfLifeAiData
        |> JDP.required "tagToSearch" Decode.string
        |> JDP.optional "delayTicks" Decode.int 8
        |> JDP.custom
            (Decode.oneOf
                [ Decode.at [ "delayTicks" ] Decode.int
                , Decode.succeed 8
                ]
            )
        |> JDP.required "actions" (Decode.list gameOfLifeAiActionDecoder)


gameOfLifeAiActionDecoder : Decoder GameOfLifeAiAction
gameOfLifeAiActionDecoder =
    Decode.succeed GameOfLifeAiAction
        |> JDP.required "count" Decode.int
        |> JDP.required "become" Decode.string


adventAiDataDecoder : Decoder AdventAiData
adventAiDataDecoder =
    Decode.succeed AdventAiData
        |> JDP.required "target" Decode.string


areaDataDecoder : Decoder AreaComponentData
areaDataDecoder =
    Decode.succeed AreaComponentData
        |> JDP.required "width" Decode.int
        |> JDP.required "height" Decode.int
        |> JDP.required "direction" directionDecoder


controlDataDecoder : Decoder ControlComponentData
controlDataDecoder =
    Decode.succeed ControlComponentData
        |> JDP.optional "settings" controlSettingsDecoder emptyControlSettings
        |> JDP.required "control" controlTypeDecoder


controlSettingsDecoder : Decoder ControlSettings
controlSettingsDecoder =
    Decode.succeed ControlSettings
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
    Decode.succeed WalkAroundAiControlData
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



-- @TODO Implement


colorDecoder : Decoder Color
colorDecoder =
    Decode.succeed (\r g b -> Color.rgb255 r g b)
        |> JDP.required "red" Decode.int
        |> JDP.required "green" Decode.int
        |> JDP.required "blue" Decode.int


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
                        Decode.succeed Actor.TagDiedSubscriber
                            |> JDP.required "eventActionData" eventActionDecoder
                            |> JDP.required "tagDiedData" onTagDiedSubscriberDecoder

                    "onInventoryUpdated" ->
                        Decode.succeed Actor.InventoryUpdatedSubscriber
                            |> JDP.required "eventActionData" eventActionDecoder
                            |> JDP.required "inventoryUpdatedData" onInventoryUpdatedSubscriberDecoder

                    _ ->
                        Decode.fail <|
                            "Trying to decode subscriber, but the type "
                                ++ theType
                                ++ " is not supported."
            )


onTagDiedSubscriberDecoder : Decoder TagDiedSubscriberData
onTagDiedSubscriberDecoder =
    Decode.succeed TagDiedSubscriberData
        |> JDP.required "tagName" Decode.string
        |> JDP.optional "limit" Decode.int 1
        |> JDP.hardcoded 0


onInventoryUpdatedSubscriberDecoder : Decoder InventoryUpdatedSubscriberData
onInventoryUpdatedSubscriberDecoder =
    Decode.succeed InventoryUpdatedSubscriberData
        |> JDP.required "interestedIn" Decode.string
        |> JDP.required "minimumQuantity" Decode.int


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
    Decode.succeed LevelFailedData
        |> JDP.required "descriptionProvider" descriptionProviderDecoder
        |> JDP.required "entityNames" (Decode.list Decode.string)
        |> JDP.required "animation" animationSetupDecoder


eventActionCompletedDataDecoder : Decoder LevelCompletedData
eventActionCompletedDataDecoder =
    Decode.succeed LevelCompletedData
        |> JDP.required "descriptionProvider" descriptionProviderDecoder
        |> JDP.required "nextLevel" Decode.string
        |> JDP.required "entityNames" (Decode.list Decode.string)
        |> JDP.required "animation" animationSetupDecoder


descriptionProviderDecoder : Decoder LevelFinishedDescriptionProvider
descriptionProviderDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\theType ->
                case theType of
                    "static" ->
                        Decode.map StaticDescriptionProvider <| Decode.field "data" staticDescriptionProviderDecoder

                    "advent" ->
                        Decode.succeed AdventOfCodeDescriptionProvider

                    _ ->
                        Decode.fail <|
                            "Trying to decode description, but the type "
                                ++ theType
                                ++ " is not supported."
            )


staticDescriptionProviderDecoder : Decoder String
staticDescriptionProviderDecoder =
    Decode.field "text" Decode.string


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

                    "skip" ->
                        Decode.succeed SkipAnimation.init

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
    Decode.succeed PrimeSearch.Coefficients
        |> JDP.required "a" Decode.int
        |> JDP.required "b" Decode.int
        |> JDP.required "c" Decode.int


spawnNeverRepeat : SpawnRepeat
spawnNeverRepeat =
    { times = RepeatNever
    , delayTicks = 0
    }


defaultViewCoordinate : Coordinate
defaultViewCoordinate =
    { x = 0
    , y = 0
    }


defaultUpdateBorder : Int
defaultUpdateBorder =
    5
