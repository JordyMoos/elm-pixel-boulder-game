module Actor
    exposing
        ( ActorId
        , Actor
        , Level
        , CanvasImages
          -- Initialization
        , LevelConfig
        , levelConfigDecoder
        , init
        , Msg
        , update
          -- Actor
        , getActorById
        , getActorsByPosition
        , getActorIdsByPosition
        , getActorIdsByXY
        , getActorsThatAffect
          -- Components
        , Component(..)
        , getRenderComponent
        , getTransformComponent
        , TransformComponentData
        , MovingTowardsData
        , MovingState(..)
        , RenderComponentData(..)
        , PixelRenderComponentData
        , ImageRenderComponentData
          -- Updates
        , updateCollectorComponent
        , updateControlComponent
        , updateCameraComponent
        , updateTransformComponent
        , updateDownSmashComponent
        , updateDamageComponent
        , updateLifetimeComponent
        , updateTriggerExplodableComponent
        , updateSpawnComponent
        )

import Dict exposing (Dict)
import Data.Common exposing (Position, Direction, Tick, addPosition, addPositions)
import Color exposing (Color)
import Maybe.Extra
import List.Extra
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Color.Convert
import Char
import Canvas
import Task


defaultCameraBorderSize : Int
defaultCameraBorderSize =
    3


type alias ActorId =
    Int


type alias Components =
    Dict String Component


type alias Actor =
    { id : ActorId
    , components : Components
    }


type alias Actors =
    Dict ActorId Actor


type alias View =
    { position : Position
    , width : Int
    , height : Int
    }


type alias PositionIndex =
    Dict ( Int, Int ) (List ActorId)


type alias EntityName =
    String


type alias KeyedComponent =
    ( String, Component )


type alias Entities =
    Dict EntityName Components


type alias Signs =
    Dict String EntityName


type alias Scene =
    List String


type alias CanvasImages =
    Dict String Canvas.Canvas


type alias Images =
    Dict String String


type alias LevelConfig =
    { entities : Entities
    , signs : Signs
    , scene : Scene
    , images : Images
    , background : RenderComponentData
    }


type alias Level =
    { entities : Entities
    , signs : Signs
    , images : CanvasImages
    , actors : Actors
    , positionIndex : PositionIndex
    , nextActorId : Int
    , view : View
    , background : RenderComponentData
    }


type Component
    = TransformComponent TransformComponentData
    | RenderComponent RenderComponentData
    | CollectorComponent CollectorComponentData
    | CollectibleComponent CollectibleComponentData
    | PhysicsComponent PhysicsComponentData
    | RigidComponent
    | ControlComponent ControlComponentData
    | CameraComponent CameraComponentData
    | ExplodableComponent
    | DownSmashComponent DownSmashComponentData
    | LifetimeComponent LifetimeComponentData
    | DamageComponent DamageComponentData
    | TriggerExplodableComponent TriggerExplodableComponentData
    | SpawnComponent SpawnComponentData


type Msg
    = ImageLoaded String (Result Canvas.Error Canvas.Canvas)


update : Msg -> Level -> Level
update msg level =
    case msg of
        ImageLoaded name (Ok canvas) ->
            { level | images = Dict.insert name canvas level.images }

        ImageLoaded name (Err error) ->
            let
                _ =
                    Debug.log "Error loading image" (toString error)
            in
                level



{-

   Initialization

-}


init : LevelConfig -> Int -> Int -> ( Level, Cmd Msg )
init config width height =
    emptyLevel width height
        |> setBackground config.background
        |> setEntities config.entities
        |> setSigns config.signs
        |> setActors config.scene
        |> setImages config.images
        |> addImageCommands config.images


addImageCommands : Images -> Level -> ( Level, Cmd Msg )
addImageCommands images level =
    ( level
    , Dict.toList images
        |> List.map
            (\( name, src ) ->
                Task.attempt (ImageLoaded name) (Canvas.loadImage src)
            )
        |> Cmd.batch
    )


emptyLevel : Int -> Int -> Level
emptyLevel width height =
    { entities = Dict.fromList []
    , signs = Dict.fromList []
    , images = Dict.fromList []
    , actors = Dict.fromList []
    , positionIndex = Dict.fromList []
    , nextActorId = 1
    , view =
        { position = { x = 0, y = 0 }
        , width = width
        , height = height
        }
    , background = defaultBackground
    }


setBackground : RenderComponentData -> Level -> Level
setBackground background level =
    { level | background = background }


setEntities : Entities -> Level -> Level
setEntities entities level =
    { level | entities = entities }


setSigns : Signs -> Level -> Level
setSigns signs level =
    { level | signs = signs }


setImages : Images -> Level -> Level
setImages images level =
    { level
        | images =
            Dict.map
                (\name src ->
                    emptyImage
                )
                images
    }


setActors : Scene -> Level -> Level
setActors scene level =
    List.indexedMap
        (,)
        scene
        |> List.foldr
            (\( y, line ) level ->
                List.indexedMap
                    (,)
                    (String.toList line)
                    |> List.foldr
                        (\( x, char ) level ->
                            Dict.get
                                (String.fromChar char)
                                level.signs
                                |> Maybe.andThen
                                    (\entityName ->
                                        Dict.get entityName level.entities
                                    )
                                |> Maybe.andThen
                                    (\entity ->
                                        addActor
                                            (Dict.insert
                                                "transform"
                                                (TransformComponent { position = { x = x, y = y }, movingState = NotMoving })
                                                entity
                                            )
                                            level
                                            |> Just
                                    )
                                |> Maybe.withDefault level
                        )
                        level
            )
            level



{-
   }

      Actor Mutation

-}


updateComponents : Actor -> Components -> Actor
updateComponents actor components =
    { actor | components = components }


updateActor : Actors -> Actor -> Actors
updateActor actors actor =
    Dict.insert
        actor.id
        actor
        actors


updateActors : Level -> Actors -> Level
updateActors level actors =
    { level | actors = actors }



{-

   Actor Query

-}


getActorById : ActorId -> Level -> Maybe Actor
getActorById actorId level =
    Dict.get
        actorId
        level.actors


getActorsByPosition : Position -> Level -> List Actor
getActorsByPosition position level =
    getActorIdsByPosition position level
        |> List.map
            (\actorId ->
                getActorById actorId level
            )
        |> Maybe.Extra.values


getActorIdsByPosition : Position -> Level -> List ActorId
getActorIdsByPosition position =
    getActorIdsByXY position.x position.y


getActorIdsByXY : Int -> Int -> Level -> List ActorId
getActorIdsByXY x y level =
    Dict.get
        ( x, y )
        level.positionIndex
        |> Maybe.withDefault []


getActorsThatAffectNeighborPosition : Actor -> Direction -> Level -> List Actor
getActorsThatAffectNeighborPosition actor direction level =
    getPosition actor
        |> Maybe.map
            (\position ->
                addPosition position (getOffsetFromDirection direction)
            )
        |> Maybe.map (flip getActorsThatAffect level)
        |> Maybe.withDefault []


getActorsThatAffect : Position -> Level -> List Actor
getActorsThatAffect position level =
    List.map
        (\position ->
            getActorIdsByPosition position level
        )
        [ position
        , addPosition position <| getOffsetFromDirection Data.Common.Left
        , addPosition position <| getOffsetFromDirection Data.Common.Up
        , addPosition position <| getOffsetFromDirection Data.Common.Right
        , addPosition position <| getOffsetFromDirection Data.Common.Down
        ]
        |> List.concat
        |> List.map
            (\actorId ->
                getActorById actorId level
            )
        |> Maybe.Extra.values
        |> List.filter
            (\actor ->
                getTransformComponent actor
                    |> Maybe.andThen
                        (\transformData ->
                            if transformData.position == position then
                                Just True
                            else
                                case transformData.movingState of
                                    MovingTowards towardsData ->
                                        Just <| towardsData.position == position

                                    NotMoving ->
                                        Nothing
                        )
                    |> Maybe.withDefault False
            )


isEmpty : Position -> Level -> Bool
isEmpty position level =
    getActorsThatAffect position level
        |> List.isEmpty


isDestinationEmpty : Actor -> Direction -> Level -> Bool
isDestinationEmpty actor direction level =
    isDestinationEmptyByOffset actor (getOffsetFromDirection direction) level


isDestinationEmptyByOffset : Actor -> Position -> Level -> Bool
isDestinationEmptyByOffset actor offset level =
    getPosition actor
        |> Maybe.map
            (\position ->
                addPosition position offset
            )
        |> Maybe.map
            (\targetPosition ->
                isEmpty targetPosition level
            )
        |> Maybe.withDefault True



{-

   Actor Remove

-}


removeActor : Actor -> Level -> Level
removeActor actor level =
    level
        |> (\level ->
                getPosition actor
                    |> Maybe.andThen
                        (\position ->
                            Just <| removeActorFromIndex position actor.id level
                        )
                    |> Maybe.withDefault level
           )
        |> (\level ->
                Dict.remove actor.id level.actors
                    |> updateActors level
           )


removeActorWithPosition : Position -> ActorId -> Level -> Level
removeActorWithPosition position actorId level =
    level
        |> removeActorFromIndex position actorId
        |> (\level ->
                Dict.remove actorId level.actors
                    |> updateActors level
           )


removeActorFromIndex : Position -> ActorId -> Level -> Level
removeActorFromIndex position actorId level =
    Dict.update
        ( position.x, position.y )
        (\maybeActorIds ->
            case maybeActorIds of
                Just actorIds ->
                    List.Extra.remove
                        actorId
                        actorIds
                        |> Just

                Nothing ->
                    Nothing
        )
        level.positionIndex
        |> updatePositionIndex level


addActorToIndex : Position -> ActorId -> Level -> Level
addActorToIndex position actorId level =
    Dict.update
        ( position.x, position.y )
        (\maybeActorIds ->
            case maybeActorIds of
                Just actorIds ->
                    actorId
                        :: actorIds
                        |> List.Extra.unique
                        |> Just

                Nothing ->
                    Just [ actorId ]
        )
        level.positionIndex
        |> updatePositionIndex level


updatePositionIndex : Level -> PositionIndex -> Level
updatePositionIndex level positionIndex =
    { level | positionIndex = positionIndex }



{-

   TransformComponent

-}


type alias TransformComponentData =
    { position : Position
    , movingState : MovingState
    }


type alias MovingTowardsData =
    { position : Position
    , totalTickCount : Int
    , tickCountLeft : Int
    , completionPercentage : Float
    }


type MovingState
    = NotMoving
    | MovingTowards MovingTowardsData


movingTicks : Tick
movingTicks =
    4


updateTransformComponent : TransformComponentData -> Actor -> Level -> Level
updateTransformComponent transformData actor level =
    getMovingTowardsData transformData
        |> Maybe.andThen
            (\towardsData ->
                if towardsData.tickCountLeft > 0 then
                    -- Still moving
                    Dict.insert
                        "transform"
                        (TransformComponent
                            { transformData
                                | movingState =
                                    MovingTowards
                                        { towardsData
                                            | tickCountLeft = towardsData.tickCountLeft - 1
                                            , completionPercentage = calculateCompletionPercentage towardsData.totalTickCount towardsData.tickCountLeft
                                        }
                            }
                        )
                        actor.components
                        |> updateComponents actor
                        |> updateActor level.actors
                        |> updateActors level
                        |> Just
                else
                    -- Finished moving
                    Dict.insert
                        "transform"
                        (TransformComponent
                            { position = towardsData.position
                            , movingState = NotMoving
                            }
                        )
                        actor.components
                        |> updateComponents actor
                        |> updateActor level.actors
                        |> updateActors level
                        |> removeActorFromIndex transformData.position actor.id
                        |> addActorToIndex towardsData.position actor.id
                        |> Just
            )
        |> Maybe.withDefault level


calculateCompletionPercentage : Int -> Int -> Float
calculateCompletionPercentage totalTickCount tickCountLeft =
    100 / (toFloat (totalTickCount)) * (toFloat (totalTickCount - tickCountLeft))


getTransformComponent : Actor -> Maybe TransformComponentData
getTransformComponent actor =
    Dict.get "transform" actor.components
        |> Maybe.andThen
            (\component ->
                case component of
                    TransformComponent data ->
                        Just data

                    _ ->
                        Nothing
            )


getPosition : Actor -> Maybe Position
getPosition actor =
    getTransformComponent actor
        |> Maybe.map .position


getMovingTowardsData : TransformComponentData -> Maybe MovingTowardsData
getMovingTowardsData transformData =
    case transformData.movingState of
        MovingTowards towardsData ->
            Just towardsData

        NotMoving ->
            Nothing


isActorMoving : Actor -> Bool
isActorMoving actor =
    getTransformComponent actor
        |> Maybe.map isMoving
        |> Maybe.withDefault False


isMoving : TransformComponentData -> Bool
isMoving transformData =
    getMovingTowardsData transformData
        |> Maybe.Extra.isJust


isNotMoving : TransformComponentData -> Bool
isNotMoving transformComponent =
    isMoving transformComponent |> not


isMovingAt : Position -> Level -> Bool
isMovingAt position level =
    getActorsByPosition position level
        |> List.map getTransformComponent
        |> Maybe.Extra.values
        |> List.filter isMoving
        |> List.isEmpty
        |> not


isNotMovingAt : Position -> Level -> Bool
isNotMovingAt position level =
    isMovingAt position level
        |> not


getNewPosition : Direction -> TransformComponentData -> ( TransformComponentData, Position )
getNewPosition direction transformData =
    ( transformData, addPosition transformData.position (getOffsetFromDirection direction) )


isMovingDown : TransformComponentData -> Bool
isMovingDown transformData =
    getMovingTowardsData transformData
        |> Maybe.Extra.filter
            (\towardsData ->
                addPosition transformData.position (getOffsetFromDirection Data.Common.Down) == towardsData.position
            )
        |> Maybe.Extra.isJust


startMovingTowards : Actor -> TransformComponentData -> Position -> Level -> Level
startMovingTowards actor transformData newPosition level =
    Dict.insert
        "transform"
        (TransformComponent
            { transformData
                | movingState =
                    MovingTowards
                        { position = newPosition
                        , totalTickCount = movingTicks
                        , tickCountLeft = movingTicks
                        , completionPercentage = 0.0
                        }
            }
        )
        actor.components
        |> updateComponents actor
        |> updateActor level.actors
        |> updateActors level



{-

   PhysicsComponent

-}


type alias PhysicsComponentData =
    { strength : Int
    , shape : Shape
    }


type Shape
    = Circle
    | Square


isCircleAt : Position -> Level -> Bool
isCircleAt position level =
    getActorsByPosition position level
        |> List.map getPhysicsComponent
        |> Maybe.Extra.values
        |> List.filter isCircle
        |> List.isEmpty
        |> not


getPhysicsComponent : Actor -> Maybe PhysicsComponentData
getPhysicsComponent actor =
    Dict.get "physics" actor.components
        |> Maybe.andThen
            (\component ->
                case component of
                    PhysicsComponent data ->
                        Just data

                    _ ->
                        Nothing
            )


getPhysicsStrength : Actor -> Int
getPhysicsStrength actor =
    getPhysicsComponent actor
        |> Maybe.map .strength
        |> Maybe.withDefault 0


isCircle : PhysicsComponentData -> Bool
isCircle physicsData =
    case physicsData.shape of
        Circle ->
            True

        _ ->
            False


isActorCircle : Actor -> Bool
isActorCircle actor =
    getPhysicsComponent actor
        |> Maybe.map isCircle
        |> Maybe.withDefault False



{-

   CollectibleComponent

-}


type alias CollectibleComponentData =
    { name : String
    , quantity : Int
    }


hasCollectibleComponent : Actor -> Bool
hasCollectibleComponent actor =
    Dict.member "collectible" actor.components


getCollectibleComponent : Actor -> Maybe CollectibleComponentData
getCollectibleComponent actor =
    Dict.get "collectible" actor.components
        |> Maybe.andThen
            (\component ->
                case component of
                    CollectibleComponent data ->
                        Just data

                    _ ->
                        Nothing
            )



{-

   CollectorComponent

-}


type alias Inventory =
    Dict String Int


type alias CollectorComponentData =
    { interestedIn : List String
    , inventory : Inventory
    }


getCollectibleDataIfCanCollect : Actor -> List String -> Maybe CollectibleComponentData
getCollectibleDataIfCanCollect targetActor interestedIn =
    getCollectibleComponent targetActor
        |> Maybe.Extra.filter
            (\collectibleData ->
                List.member collectibleData.name interestedIn
            )


canCollect : CollectibleComponentData -> List String -> Bool
canCollect collectibleData =
    List.member collectibleData.name


updateCollectorComponent : CollectorComponentData -> Actor -> Level -> Level
updateCollectorComponent collectorData collectorActor level =
    getTransformComponent collectorActor
        |> Maybe.Extra.toList
        |> List.map .position
        |> List.concatMap
            (\position ->
                getActorsByPosition position level
                    |> List.map
                        (\actor ->
                            ( position, actor )
                        )
            )
        |> List.filterMap
            (\( position, actor ) ->
                getCollectibleComponent actor
                    |> Maybe.andThen
                        (\collectibleData ->
                            Just ( position, actor, collectibleData )
                        )
            )
        |> List.filter
            (\( position, actor, collectibleData ) ->
                canCollect collectibleData collectorData.interestedIn
            )
        |> List.foldr
            (\( position, actor, collectibleData ) level ->
                updateCollectorComponentData collectorData collectibleData
                    |> setCollectorComponent collectorActor.components
                    |> updateComponents collectorActor
                    |> updateActor level.actors
                    |> updateActors level
                    |> removeActorWithPosition position actor.id
            )
            level


updateCollectorComponentData : CollectorComponentData -> CollectibleComponentData -> CollectorComponentData
updateCollectorComponentData collector collectible =
    collector.inventory
        |> Dict.update
            collectible.name
            (\maybeCurrentQuantity ->
                Just <| (Maybe.withDefault 0 maybeCurrentQuantity) + collectible.quantity
            )
        |> updateCollectorInventory collector


updateCollectorInventory : CollectorComponentData -> Inventory -> CollectorComponentData
updateCollectorInventory collector newInventory =
    { collector | inventory = newInventory }


setCollectorComponent : Components -> CollectorComponentData -> Components
setCollectorComponent components collectorData =
    Dict.insert
        "collector"
        (CollectorComponent collectorData)
        components



{-

   RigidComponent

-}


hasRigidComponent : Actor -> Bool
hasRigidComponent actor =
    Dict.member "rigid" actor.components



{-

   ControlComponent

-}


type alias ControlComponentData =
    { settings : ControlSettings
    , control : ControlType
    }


type alias ControlSettings =
    { pushStrength : Int
    , walkOverStrength : Int
    }


type ControlType
    = InputControl
    | WalkAroundAiControl WalkAroundAiControlData
    | GravityAiControl


type alias WalkAroundAiControlData =
    { previousDirection : Direction
    , nextDirectionOffsets : List Int
    }


updateControlComponent : Maybe Direction -> ControlComponentData -> Actor -> Level -> Level
updateControlComponent inputControllerDirection controlData actor level =
    if isActorMoving actor |> not then
        getControlDirection inputControllerDirection controlData actor level
            |> Maybe.map
                (\( direction, actor ) ->
                    handleDirection direction actor level
                )
            |> Maybe.withDefault level
    else
        level


getControlComponent : Actor -> Maybe ControlComponentData
getControlComponent actor =
    Dict.get "control" actor.components
        |> Maybe.andThen
            (\component ->
                case component of
                    ControlComponent data ->
                        Just data

                    _ ->
                        Nothing
            )


getPushStrength : Actor -> Int
getPushStrength actor =
    getControlComponent actor
        |> Maybe.map (\data -> data.settings.pushStrength)
        |> Maybe.withDefault 0


getWalkOverStrength : Actor -> Int
getWalkOverStrength actor =
    getControlComponent actor
        |> Maybe.map (\data -> data.settings.walkOverStrength)
        |> Maybe.withDefault 0


getControlDirection : Maybe Direction -> ControlComponentData -> Actor -> Level -> Maybe ( Direction, Actor )
getControlDirection inputControllerDirection controlData actor level =
    case ( controlData.control, inputControllerDirection ) of
        ( InputControl, Just direction ) ->
            Just ( direction, actor )

        ( InputControl, Nothing ) ->
            Nothing

        ( WalkAroundAiControl aiData, _ ) ->
            getWalkAroundAiDirection controlData aiData actor level

        ( GravityAiControl, _ ) ->
            getGravityAiDirection controlData actor level


getWalkAroundAiDirection : ControlComponentData -> WalkAroundAiControlData -> Actor -> Level -> Maybe ( Direction, Actor )
getWalkAroundAiDirection controlData aiData actor level =
    aiData.nextDirectionOffsets
        |> List.map
            (\directionOffset ->
                getDirectionFromID <| (getIDFromDirection aiData.previousDirection) - directionOffset
            )
        |> List.Extra.find
            (\direction ->
                canGoInDirection actor direction level
            )
        |> Maybe.map
            (\direction ->
                ( direction
                , Dict.insert
                    "control"
                    (ControlComponent
                        { controlData
                            | control = WalkAroundAiControl <| { aiData | previousDirection = direction }
                        }
                    )
                    actor.components
                    |> updateComponents actor
                )
            )


getGravityAiDirection : ControlComponentData -> Actor -> Level -> Maybe ( Direction, Actor )
getGravityAiDirection controlData actor level =
    getPosition actor
        |> Maybe.map
            (\position ->
                [ ( Data.Common.Down
                  , [ \() -> canGoInDirection actor Data.Common.Down level ]
                  )
                , ( Data.Common.Left
                  , [ \() ->
                        isEmpty
                            (addPositions [ position, getOffsetFromDirection Data.Common.Left, getOffsetFromDirection Data.Common.Down ])
                            level
                    , \() -> isCircleAt (addPositions [ position, getOffsetFromDirection Data.Common.Down ]) level
                    , \() -> canGoInDirection actor Data.Common.Left level
                    ]
                  )
                , ( Data.Common.Right
                  , [ \() ->
                        isEmpty
                            (addPositions [ position, getOffsetFromDirection Data.Common.Right, getOffsetFromDirection Data.Common.Down ])
                            level
                    , \() -> isCircleAt (addPositions [ position, getOffsetFromDirection Data.Common.Down ]) level
                    , \() -> canGoInDirection actor Data.Common.Right level
                    ]
                  )
                ]
            )
        |> Maybe.Extra.toList
        |> List.concat
        |> List.Extra.find
            (\( _, predicates ) ->
                lazyAll predicates
            )
        |> Maybe.map
            (\( direction, _ ) ->
                ( direction, actor )
            )


isAllowedToBePushedByAi : Direction -> Actor -> Bool
isAllowedToBePushedByAi direction actor =
    getControlComponent actor
        |> Maybe.map
            (\controlData ->
                case controlData.control of
                    InputControl ->
                        False

                    WalkAroundAiControl data ->
                        False

                    GravityAiControl ->
                        direction /= Data.Common.Up
            )
        |> Maybe.withDefault True


handleDirection : Direction -> Actor -> Level -> Level
handleDirection direction actor level =
    getTransformComponent actor
        |> Maybe.map
            (\transformData ->
                case getActorsThatAffectNeighborPosition actor direction level of
                    -- No one there
                    [] ->
                        startMovingTowards actor transformData (addPositions [ transformData.position, getOffsetFromDirection direction ]) level

                    -- Only one actor
                    [ otherActor ] ->
                        if canBeWalkedOver actor otherActor then
                            startMovingTowards actor transformData (addPositions [ transformData.position, getOffsetFromDirection direction ]) level
                        else if canPush actor otherActor direction level then
                            getTransformComponent otherActor
                                |> Maybe.map
                                    (\otherTransformData ->
                                        startMovingTowards otherActor otherTransformData (addPositions [ otherTransformData.position, getOffsetFromDirection direction ]) level
                                            |> startMovingTowards actor transformData (addPositions [ transformData.position, getOffsetFromDirection direction ])
                                    )
                                |> Maybe.withDefault level
                        else
                            level

                    -- Multiple actors. There is no implementation for that scenario
                    _ ->
                        level
            )
        |> Maybe.withDefault level


canGoInDirection : Actor -> Direction -> Level -> Bool
canGoInDirection actor direction level =
    case getActorsThatAffectNeighborPosition actor direction level of
        -- No one there
        [] ->
            True

        -- Only one actor
        [ otherActor ] ->
            lazyAny
                [ \() -> canBeWalkedOver actor otherActor
                , \() -> canPush actor otherActor direction level
                ]

        -- Multiple actors. There is no implementation for that scenario
        _ ->
            False


canPush : Actor -> Actor -> Direction -> Level -> Bool
canPush pushingActor toBePushedActor direction level =
    lazyAll
        [ \() -> hasRigidComponent pushingActor
        , \() -> hasRigidComponent toBePushedActor
        , \() -> isActorMoving toBePushedActor |> not
        , \() -> isAllowedToBePushedByAi direction toBePushedActor
        , \() -> isDestinationEmpty toBePushedActor direction level
        , \() -> hasEnoughPushStrength pushingActor toBePushedActor
        ]


canBeWalkedOver : Actor -> Actor -> Bool
canBeWalkedOver initiatingActor destinationActor =
    lazyAll
        [ \() -> hasRigidComponent destinationActor |> not
        , \() -> hasEnoughWalkOverStrength initiatingActor destinationActor
        ]


hasEnoughPushStrength : Actor -> Actor -> Bool
hasEnoughPushStrength initiatingActor destinationActor =
    (getPushStrength initiatingActor) > (getPhysicsStrength destinationActor)


hasEnoughWalkOverStrength : Actor -> Actor -> Bool
hasEnoughWalkOverStrength initiatingActor destinationActor =
    (getWalkOverStrength initiatingActor) > (getPhysicsStrength destinationActor)


lazyAll : List (() -> Bool) -> Bool
lazyAll =
    List.all
        (\p -> p ())


lazyAny : List (() -> Bool) -> Bool
lazyAny =
    List.any
        (\p -> p ())



{-

   CameraComponent

-}


type alias CameraComponentData =
    { borderSize : Int
    }


updateCameraComponent : CameraComponentData -> Actor -> Level -> Level
updateCameraComponent camera actor level =
    getTransformComponent actor
        |> Maybe.andThen
            (\transformData ->
                let
                    view =
                        level.view

                    viewPosition =
                        view.position

                    position =
                        transformData.position

                    x =
                        if position.x - camera.borderSize <= viewPosition.x then
                            position.x - camera.borderSize
                        else if position.x - view.width + camera.borderSize > viewPosition.x - 1 then
                            position.x - view.width + camera.borderSize + 1
                        else
                            viewPosition.x

                    y =
                        if position.y - camera.borderSize <= viewPosition.y then
                            position.y - camera.borderSize
                        else if position.y - view.height + camera.borderSize >= viewPosition.y - 1 then
                            position.y - view.height + camera.borderSize + 1
                        else
                            viewPosition.y

                    newViewPosition =
                        { viewPosition
                            | x = x
                            , y = y
                        }
                in
                    Just { level | view = updateViewPosition newViewPosition view }
            )
        |> Maybe.withDefault level


getCameraComponent : Actor -> Maybe CameraComponentData
getCameraComponent actor =
    Dict.get "camera" actor.components
        |> Maybe.andThen
            (\component ->
                case component of
                    CameraComponent data ->
                        Just data

                    _ ->
                        Nothing
            )


updateView : View -> Level -> Level
updateView view level =
    { level | view = view }


updateViewPosition : Position -> View -> View
updateViewPosition position view =
    { view | position = position }



{-

   TriggerExplodableComponent

-}


type alias TriggerExplodableComponentData =
    { triggerStrength : Int
    }


updateTriggerExplodableComponent : TriggerExplodableComponentData -> Actor -> Level -> Level
updateTriggerExplodableComponent triggerData actor level =
    getTransformComponent actor
        |> Maybe.Extra.toList
        |> List.concatMap
            (\transformData ->
                [ addPosition transformData.position (getOffsetFromDirection Data.Common.Left)
                , addPosition transformData.position (getOffsetFromDirection Data.Common.Up)
                , addPosition transformData.position (getOffsetFromDirection Data.Common.Right)
                , addPosition transformData.position (getOffsetFromDirection Data.Common.Down)
                ]
            )
        |> List.filterMap
            (\position ->
                getActorsThatAffect position level
                    |> List.filter (willTriggerBy triggerData.triggerStrength)
                    |> List.map
                        (\explodableActor ->
                            ( position, explodableActor )
                        )
                    |> Just
            )
        |> List.concat
        |> List.foldr
            (\( position, explodableActor ) level ->
                level
                    |> addBigExplosion position
                    |> removeActorWithPosition position explodableActor.id
            )
            level


willTriggerBy : Int -> Actor -> Bool
willTriggerBy triggerStrength actor =
    [ actor ]
        |> List.filter hasExplodableComponent
        |> List.map getPhysicsComponent
        |> Maybe.Extra.values
        |> List.filter
            (\physics ->
                physics.strength < triggerStrength
            )
        |> List.isEmpty
        |> not



{-

   LifetimeComponent

-}


type alias LifetimeComponentData =
    { remainingTicks : Int
    }


updateLifetimeComponent : LifetimeComponentData -> Actor -> Level -> Level
updateLifetimeComponent lifetimeData actor level =
    if lifetimeData.remainingTicks > 0 then
        Dict.insert
            "lifetime"
            (LifetimeComponent { lifetimeData | remainingTicks = lifetimeData.remainingTicks - 1 })
            actor.components
            |> updateComponents actor
            |> updateActor level.actors
            |> updateActors level
    else
        removeActor actor level



{-

   DamageComponent

-}


type alias DamageComponentData =
    { damageStrength : Int
    }


updateDamageComponent : DamageComponentData -> Actor -> Level -> Level
updateDamageComponent damageData damageDealingActor level =
    getTransformComponent damageDealingActor
        |> Maybe.Extra.toList
        |> List.concatMap
            (\transformData ->
                getActorsThatAffect transformData.position level
            )
        |> List.filter
            (\actor ->
                actor.id /= damageDealingActor.id
            )
        |> List.filter
            (\actor ->
                getPhysicsComponent actor
                    |> Maybe.andThen
                        (\physics ->
                            Just <| physics.strength < damageData.damageStrength
                        )
                    |> Maybe.withDefault True
            )
        |> List.foldr
            (\actor level ->
                removeActor actor level
            )
            level



{-

   SpawnComponent

-}


type alias SpawnComponentData =
    { entityName : String
    , position : Position
    , delayTicks : Int
    , repeat : SpawnRepeat
    }


type alias SpawnRepeat =
    { times : SpawnRepeatTimes
    , delayTicks : Int
    }


type SpawnRepeatTimes
    = RepeatNever
    | RepeatForever
    | RepeatTimes Int


spawnNeverRepeat : SpawnRepeat
spawnNeverRepeat =
    { times = RepeatNever
    , delayTicks = 0
    }


updateSpawnComponent : SpawnComponentData -> Actor -> Level -> Level
updateSpawnComponent data actor level =
    if data.delayTicks > 0 then
        spawnDecrementDelayTicks data
            |> setSpawnComponentData actor level
            |> Tuple.second
    else
        spawnActor data level
            |> updateSpawnComponentAfterSpawn data actor


setSpawnComponentData : Actor -> Level -> SpawnComponentData -> ( Actor, Level )
setSpawnComponentData actor level data =
    data
        |> SpawnComponent
        |> (\component ->
                Dict.insert "spawn" component actor.components
           )
        |> updateComponents actor
        |> (\actor ->
                ( actor
                , updateActor level.actors actor
                    |> updateActors level
                )
           )


spawnActor : SpawnComponentData -> Level -> Level
spawnActor data level =
    if getActorsThatAffect data.position level |> List.isEmpty then
        Dict.get
            data.entityName
            level.entities
            |> Maybe.map
                (\entity ->
                    addActor
                        (Dict.insert
                            "transform"
                            (TransformComponent { position = data.position, movingState = NotMoving })
                            entity
                        )
                        level
                )
            |> Maybe.withDefault level
    else
        level


updateSpawnComponentAfterSpawn : SpawnComponentData -> Actor -> Level -> Level
updateSpawnComponentAfterSpawn data actor level =
    case data.repeat.times of
        RepeatNever ->
            removeSpawnComponent actor level

        RepeatForever ->
            spawnResetDelayTicks data
                |> setSpawnComponentData actor level
                |> Tuple.second

        RepeatTimes count ->
            if count > 0 then
                RepeatTimes (count - 1)
                    |> flip spawnUpdateRepeatTimes data.repeat
                    |> flip spawnUpdateRepeat data
                    |> spawnResetDelayTicks
                    |> setSpawnComponentData actor level
                    |> Tuple.second
            else
                removeSpawnComponent actor level


removeSpawnComponent : Actor -> Level -> Level
removeSpawnComponent actor level =
    Dict.remove "spawn" actor.components
        |> updateComponents actor
        |> updateActor level.actors
        |> updateActors level


spawnUpdateRepeatTimes : SpawnRepeatTimes -> SpawnRepeat -> SpawnRepeat
spawnUpdateRepeatTimes times repeat =
    { repeat | times = times }


spawnUpdateRepeat : SpawnRepeat -> SpawnComponentData -> SpawnComponentData
spawnUpdateRepeat repeat data =
    { data | repeat = repeat }


spawnDecrementDelayTicks : SpawnComponentData -> SpawnComponentData
spawnDecrementDelayTicks data =
    { data | delayTicks = data.delayTicks - 1 }


spawnResetDelayTicks : SpawnComponentData -> SpawnComponentData
spawnResetDelayTicks data =
    { data | delayTicks = data.repeat.delayTicks }



{-

   DownSmashComponent

-}


type alias DownSmashComponentData =
    { movingDownState : MovingDownState
    }


type MovingDownState
    = IsMovingDown Int
    | NotMovingDown


updateDownSmashComponent : DownSmashComponentData -> Actor -> Level -> Level
updateDownSmashComponent downSmashData actor level =
    getTransformComponent actor
        |> Maybe.andThen
            (\transformData ->
                Just ( transformData.position, isMovingDown transformData )
            )
        |> Maybe.andThen
            (\( position, movingDown ) ->
                (case ( movingDown, downSmashData.movingDownState ) of
                    ( False, IsMovingDown _ ) ->
                        getActorsThatAffect
                            (addPosition position <| getOffsetFromDirection Data.Common.Down)
                            level
                            |> List.filter hasExplodableComponent
                            |> List.foldr
                                (\downActor level ->
                                    level
                                        |> addBigExplosion (addPosition position <| getOffsetFromDirection Data.Common.Down)
                                        |> removeActorWithPosition (addPosition position <| getOffsetFromDirection Data.Common.Down) downActor.id
                                )
                                level

                    _ ->
                        level
                )
                    -- Update the WasMovingDown
                    |> (\level ->
                            updateMovingDownState movingDown downSmashData
                                |> updateDownSmash
                                    actor
                                    level
                       )
                    |> Just
            )
        |> Maybe.withDefault level


updateMovingDownState : Bool -> DownSmashComponentData -> DownSmashComponentData
updateMovingDownState movingDown downSmashData =
    case movingDown of
        True ->
            { downSmashData | movingDownState = IsMovingDown 3 }

        False ->
            { downSmashData | movingDownState = lowerMovingDownCount downSmashData.movingDownState }


lowerMovingDownCount : MovingDownState -> MovingDownState
lowerMovingDownCount movingDownState =
    case movingDownState of
        IsMovingDown 0 ->
            NotMovingDown

        IsMovingDown x ->
            IsMovingDown (x - 1)

        NotMovingDown ->
            NotMovingDown


updateDownSmash : Actor -> Level -> DownSmashComponentData -> Level
updateDownSmash actor level downSmashData =
    Dict.insert
        "downsmash"
        (DownSmashComponent downSmashData)
        actor.components
        |> updateComponents actor
        |> updateActor level.actors
        |> updateActors level



{-

   RenderComponent

-}


type RenderComponentData
    = PixelRenderComponent PixelRenderComponentData
    | ImageRenderComponent ImageRenderComponentData


type alias PixelRenderComponentData =
    { colors : List Color
    , ticksPerColor : Int
    }


type alias ImageRenderComponentData =
    { name : String }


getRenderComponent : Actor -> Maybe RenderComponentData
getRenderComponent actor =
    Dict.get "render" actor.components
        |> Maybe.andThen
            (\component ->
                case component of
                    RenderComponent data ->
                        Just data

                    _ ->
                        Nothing
            )



{-

   ExplodableComponent

-}


hasExplodableComponent : Actor -> Bool
hasExplodableComponent actor =
    Dict.member "explodable" actor.components



{-

   Add Actors

-}


addActor : Components -> Level -> Level
addActor components level =
    level
        |> incrementNextActorId
        |> (\level ->
                ( level
                , { id = level.nextActorId
                  , components = components
                  }
                )
           )
        -- Add actor to the index
        |> (\( level, actor ) ->
                getPosition actor
                    |> Maybe.andThen
                        (\position ->
                            Just <| ( addActorToIndex position actor.id level, actor )
                        )
                    |> Maybe.withDefault
                        ( level, actor )
           )
        -- Update view if needed
        |> (\( level, actor ) ->
                getCameraComponent actor
                    |> Maybe.andThen
                        (\camera ->
                            getTransformComponent actor
                        )
                    |> Maybe.andThen
                        (\transform ->
                            updateViewPosition
                                { x = transform.position.x - (round ((toFloat level.view.width) / 2))
                                , y = transform.position.y - (round ((toFloat level.view.height) / 2))
                                }
                                level.view
                                |> (flip updateView) level
                                |> Just
                        )
                    |> Maybe.andThen
                        (\level ->
                            Just ( level, actor )
                        )
                    |> Maybe.withDefault ( level, actor )
           )
        -- Add actor to the actors
        |> (\( level, actor ) ->
                updateActor level.actors actor
                    |> updateActors level
           )


incrementNextActorId : Level -> Level
incrementNextActorId level =
    { level | nextActorId = level.nextActorId + 1 }


addExplosion : Int -> Int -> Level -> Level
addExplosion x y level =
    addActor
        (Dict.fromList
            [ ( "transform", TransformComponent { position = { x = x, y = y }, movingState = NotMoving } )
            , ( "render", RenderComponent <| PixelRenderComponent { colors = [ Color.red, Color.darkOrange, Color.yellow ], ticksPerColor = 2 } )
            , ( "lifetime", LifetimeComponent { remainingTicks = 8 } )
            , ( "damage", DamageComponent { damageStrength = 80 } )
            ]
        )
        level


addBigExplosion : Position -> Level -> Level
addBigExplosion position level =
    List.foldr
        (\position level ->
            level |> addExplosion position.x position.y
        )
        level
        [ addPositions [ position, getOffsetFromDirection Data.Common.Left, getOffsetFromDirection Data.Common.Up ]
        , addPositions [ position, getOffsetFromDirection Data.Common.Up ]
        , addPositions [ position, getOffsetFromDirection Data.Common.Right, getOffsetFromDirection Data.Common.Up ]
        , addPositions [ position, getOffsetFromDirection Data.Common.Left ]
        , position
        , addPositions [ position, getOffsetFromDirection Data.Common.Right ]
        , addPositions [ position, getOffsetFromDirection Data.Common.Left, getOffsetFromDirection Data.Common.Down ]
        , addPositions [ position, getOffsetFromDirection Data.Common.Down ]
        , addPositions [ position, getOffsetFromDirection Data.Common.Right, getOffsetFromDirection Data.Common.Down ]
        ]



{-

   Position Helpers

-}


getOffsetFromDirection : Direction -> Position
getOffsetFromDirection direction =
    case direction of
        Data.Common.Left ->
            { x = -1, y = 0 }

        Data.Common.Up ->
            { x = 0, y = -1 }

        Data.Common.Right ->
            { x = 1, y = 0 }

        Data.Common.Down ->
            { x = 0, y = 1 }



-- @todo we might be able to combine this with InputController.keyCodeToDirection


getDirectionFromID : Int -> Direction
getDirectionFromID id =
    case id % 4 of
        0 ->
            Data.Common.Left

        1 ->
            Data.Common.Up

        2 ->
            Data.Common.Right

        _ ->
            Data.Common.Down


getIDFromDirection : Direction -> Int
getIDFromDirection direction =
    case direction of
        Data.Common.Left ->
            0

        Data.Common.Up ->
            1

        Data.Common.Right ->
            2

        Data.Common.Down ->
            3



{-

   Json

-}


levelConfigDecoder : Decoder LevelConfig
levelConfigDecoder =
    JDP.decode LevelConfig
        |> JDP.required "entities" entitiesDecoder
        |> JDP.required "signs" signsDecoder
        |> JDP.required "scene" sceneDecoder
        |> JDP.optional "images" imagesDecoder Dict.empty
        |> JDP.optional "background" renderDataDecoder defaultBackground


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
        |> JDP.optional "previousDirection" directionDecoder Data.Common.Left
        |> JDP.required "nextDirectionOffsets" (Decode.list Decode.int)


directionDecoder : Decoder Direction
directionDecoder =
    Decode.string
        |> Decode.andThen
            (\direction ->
                case direction of
                    "left" ->
                        Decode.succeed Data.Common.Left

                    "up" ->
                        Decode.succeed Data.Common.Up

                    "right" ->
                        Decode.succeed Data.Common.Right

                    "down" ->
                        Decode.succeed Data.Common.Down

                    _ ->
                        Decode.fail <|
                            "Trying to decode direction, but the direction "
                                ++ direction
                                ++ " is not supported. Supported directions are: left, up, right, down."
            )


colorDecoder : Decoder Color
colorDecoder =
    Decode.string
        |> Decode.andThen
            (\stringColor ->
                case Color.Convert.hexToColor stringColor of
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


emptyImage : Canvas.Canvas
emptyImage =
    Canvas.Size 32 32
        |> Canvas.initialize
