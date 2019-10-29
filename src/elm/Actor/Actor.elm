module Actor.Actor exposing
    ( Actor
    , ActorId
    , ActorType(..)
    , Actors
    , AdventAiData
    , AframeCamera
    , AframeRendererData
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
    , DownSmashComponentData
    , Entities
    , Event(..)
    , EventAction(..)
    , EventManager
    , Events
    , GameOfLifeAiAction
    , GameOfLifeAiData
    , Health
    , HealthComponentData
    , Image
    , ImageType(..)
    , ImageTypeData
    , Images
    , ImagesData
    , InputControlData
    , Inventory
    , InventoryUpdatedSubscriberData
    , KeyedComponent
    , Level
    , LevelCompletedData
    , LevelConfig
    , LevelFailedData
    , LevelFinishedDescriptionProvider(..)
    , LifetimeAction(..)
    , LifetimeComponentData
    , LinkImageData
    , LoadLevelData
    , MovementComponentData
    , MovingDownState(..)
    , MovingState(..)
    , MovingTowardsData
    , ObjectAssets
    , ObjectPresetData
    , ObjectPresetName
    , ObjectPresets
    , ObjectSettings
    , ObjectTypeData
    , Objects
    , OffsetType(..)
    , PatternImageData
    , PhysicsComponentData
    , PixelTypeData
    , PositionIndex
    , PositionIndices
    , PositionOffsets
    , RenderComponentData
    , RenderType(..)
    , Renderer(..)
    , Scene
    , Shape(..)
    , Signs
    , SpawnComponentData
    , SpawnRepeat
    , SpawnRepeatTimes(..)
    , Subscriber(..)
    , Subscribers
    , TagComponentData
    , TagDiedSubscriberData
    , TransformComponentData
    , TriggerAction(..)
    , TriggerActivatorComponentData
    , TriggerComponentData
    , TriggerExplodableComponentData
    , TriggerSendTextData
    , Vec3
    , View
    , WalkAroundAiControlData
    , emptyEventManager
    , emptyVec3
    )

import Color exposing (Color)
import Data.Config exposing (Config)
import Data.Coordinate exposing (Coordinate)
import Data.Direction exposing (Direction)
import Data.Position exposing (Position)
import Data.Rotation as Rotation
import Dict exposing (Dict)
import GameState.PlayingLevel.Msg as PlayingMsg


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


type ActorType
    = EnvironmentActor
    | StaticActor
    | DynamicActor


type alias View =
    { coordinate : Coordinate
    }


type alias PositionIndex =
    Dict ( Int, Int ) (List ActorId)


type alias PositionIndices =
    { environment : PositionIndex
    , static : PositionIndex
    , dynamic : PositionIndex
    }


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


type alias Images =
    Dict String Image


type alias Image =
    { path : String
    , width : Int
    , height : Int
    , imageType : ImageType
    , xOffset : Int
    , yOffset : Int
    }


type alias PositionOffsets =
    { x : List OffsetType
    , y : List OffsetType
    , z : List OffsetType
    }


type alias Vec3 =
    { x : Float
    , y : Float
    , z : Float
    }


emptyVec3 : Vec3
emptyVec3 =
    { x = 0.0
    , y = 0.0
    , z = 0.0
    }


type alias Objects =
    { assets : ObjectAssets
    , presets : ObjectPresets
    }


type alias ObjectAssets =
    Dict String String


type alias ObjectPresets =
    Dict String ObjectPresetData


type alias ObjectPresetData =
    { settings : ObjectSettings
    , offsets : PositionOffsets
    }


type alias ObjectSettings =
    Dict String String


type ImageType
    = RegularImage
    | PatternImage PatternImageData
    | LinkImage LinkImageData


type alias PatternImageData =
    { offsets : PositionOffsets
    }


type OffsetType
    = FixedOffset Float
    | MultipliedByViewX Float
    | MultipliedByViewY Float
    | ViewOffsetX
    | ViewOffsetY


type alias LinkImageData =
    { href : String
    }


type alias LevelConfig =
    { entities : Entities
    , signLength : Int
    , signs : Signs
    , scene : Scene
    , viewCoordinate : Coordinate
    , updateBorder : Int
    , images : Images
    , objects : Objects
    , backgrounds : List RenderComponentData
    , subscribers : Subscribers
    , config : Maybe Config
    , renderer : Renderer
    }


type Renderer
    = SvgRenderer
    | AframeRenderer AframeRendererData


type alias AframeRendererData =
    { camera : AframeCamera
    }


type alias AframeCamera =
    { rotation : Rotation.Rotation
    , offsets : PositionOffsets
    }


type alias Level =
    { nextActorId : Int
    , actors : Actors
    , positionIndices : PositionIndices
    , view : View
    , backgrounds : List RenderComponentData
    , eventManager : EventManager
    , events : Events
    , config : Config
    }


type Component
    = TransformComponent TransformComponentData
    | MovementComponent MovementComponentData
    | RenderComponent RenderComponentData
    | CollectorComponent CollectorComponentData
    | CollectibleComponent CollectibleComponentData
    | PhysicsComponent PhysicsComponentData
    | RigidComponent
    | AiComponent AiComponentData
    | ControlComponent ControlComponentData
    | CameraComponent CameraComponentData
    | ExplodableComponent
    | DownSmashComponent DownSmashComponentData
    | LifetimeComponent LifetimeComponentData
    | DamageComponent DamageComponentData
    | TriggerExplodableComponent TriggerExplodableComponentData
    | SpawnComponent SpawnComponentData
    | TagComponent TagComponentData
    | HealthComponent HealthComponentData
    | AttackComponent AttackComponentData
    | CounterComponent CounterComponentData
    | AreaComponent AreaComponentData
    | TriggerComponent TriggerComponentData
    | TriggerActivatorComponent TriggerActivatorComponentData



{-

   TransformComponent

-}


type alias TransformComponentData =
    { position : Position
    }



{-

   MovementComponent

-}


type alias MovementComponentData =
    { movingTicks : Int
    , lastHandledTick : Int
    , movingState : MovingState
    }


type alias MovingTowardsData =
    { position : Position
    , totalTickCount : Int
    , tickCountLeft : Int
    , completionPercentage : Float
    , direction : Direction
    }


type MovingState
    = NotMoving
    | MovingTowards MovingTowardsData



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



{-

   CollectibleComponent

-}


type alias CollectibleComponentData =
    { name : String
    , quantity : Int
    }



{-

   CollectorComponent

-}


type alias Inventory =
    Dict String Int


type alias CollectorComponentData =
    { interestedIn : List String
    , inventory : Inventory
    }



{-

   AiComponent

-}


type alias AiComponentData =
    { ai : AiType
    }


type AiType
    = GameOfLifeAi GameOfLifeAiData
    | AdventAi AdventAiData


type alias GameOfLifeAiData =
    { tagToSearch : String
    , delayTicks : Int
    , delayTicksInitially : Int
    , actions : List GameOfLifeAiAction
    }


type alias GameOfLifeAiAction =
    { count : Int
    , become : String
    }


type alias AdventAiData =
    { target : String
    }



{-

   ControlComponent

-}


type alias ControlComponentData =
    { settings : ControlSettings
    , control : ControlType
    , steps : Int
    , queue : List Direction
    }


type alias ControlSettings =
    { pushStrength : Int
    , walkOverStrength : Int
    }


type ControlType
    = InputControl InputControlData
    | WalkAroundAiControl WalkAroundAiControlData
    | GravityAiControl


type alias InputControlData =
    { allowedDirections : List Direction
    }


type alias WalkAroundAiControlData =
    { previousDirection : Direction
    , nextDirectionOffsets : List Int
    }



{-

   CameraComponent

-}


type alias CameraComponentData =
    { borderLeft : Int
    , borderUp : Int
    , borderRight : Int
    , borderDown : Int
    }



{-

   TriggerExplodableComponent

-}


type alias TriggerExplodableComponentData =
    { triggerStrength : Int
    }



{-

   LifetimeComponent

-}


type alias LifetimeComponentData =
    { remainingTicks : Int
    , action : LifetimeAction
    }


type LifetimeAction
    = RemoveActorLifetimeAction
    | BecomeActorLifetimeAction BecomeActorLifetimeActionData


type alias BecomeActorLifetimeActionData =
    { entityName : String
    }



{-

   AreaComponent

-}


type alias AreaComponentData =
    { width : Int
    , height : Int
    , direction : Direction
    , tags : List String
    }



{-

   TriggerComponent

-}


type alias TriggerComponentData =
    { action : TriggerAction
    }


type TriggerAction
    = TriggerSendText TriggerSendTextData


type alias TriggerSendTextData =
    { message : String }



{-

   TriggerActivatorComponent

-}


type alias TriggerActivatorComponentData =
    {}



{-

   DamageComponent

-}


type alias DamageComponentData =
    { damageStrength : Int
    }



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



{-

   DownSmashComponent

-}


type alias DownSmashComponentData =
    { movingDownState : MovingDownState
    }


type MovingDownState
    = IsMovingDown Int
    | NotMovingDown



{-

   RenderComponent

-}


type alias RenderComponentData =
    { renderType : RenderType
    , layer : Int
    }


type RenderType
    = PixelRenderType PixelTypeData
    | ImageRenderType ImageTypeData
    | ObjectRenderType ObjectTypeData


type alias PixelTypeData =
    { colors : List Color
    , ticksPerColor : Int
    }


type alias ImageTypeData =
    { default : ImagesData
    , direction : Dict Int ImagesData
    }


type alias ImagesData =
    { names : List String
    , ticksPerImage : Int
    }


type alias ObjectPresetName =
    String


type alias ObjectTypeData =
    { default : ObjectPresetName
    , direction : Dict Int ObjectPresetName
    }



{-

   TagComponent

-}


type alias TagComponentData =
    { name : String
    }



{-

   HealthComponent

-}


type alias Health =
    Int


type alias HealthComponentData =
    { health : Health
    , maxHealth : Health
    }



{-

   AttackComponent

-}


type alias AttackComponentData =
    { power : Int
    }



{-

   CounterComponent

-}


type alias CounterComponentData =
    { count : Int
    }



{-

   Animation

-}


type alias AnimationSetup =
    Config -> Int -> List Position



{-

   EventManager

-}


type Event
    = ActorAdded Actor
    | ActorRemoved Actor
    | InventoryUpdated Inventory
    | TriggerActivated TriggerComponentData


type EventAction
    = LevelContinue (List (Cmd PlayingMsg.Msg))
    | LevelFailed LevelFailedData
    | LevelCompleted LevelCompletedData
    | LoadLevel LoadLevelData


type alias LevelFailedData =
    { descriptionProvider : LevelFinishedDescriptionProvider
    , entityNames : List String
    , animationSetup : AnimationSetup
    }


type alias LevelCompletedData =
    { descriptionProvider : LevelFinishedDescriptionProvider
    , nextLevel : String
    , entityNames : List String
    , animationSetup : AnimationSetup
    }


type alias LoadLevelData =
    { nextLevel : String
    }


type LevelFinishedDescriptionProvider
    = StaticDescriptionProvider String
    | AdventOfCodeDescriptionProvider


type alias Events =
    List Event


type Subscriber
    = TagDiedSubscriber EventAction TagDiedSubscriberData
    | InventoryUpdatedSubscriber EventAction InventoryUpdatedSubscriberData
    | TriggerActivatedSubscriber


type alias TagDiedSubscriberData =
    { tag : String
    , limit : Int
    , counter : Int
    }


type alias InventoryUpdatedSubscriberData =
    { interestedIn : String
    , minimumQuantity : Int
    }


type alias Subscribers =
    List Subscriber


type alias EventManager =
    { subscribers : Subscribers
    }


emptyEventManager : EventManager
emptyEventManager =
    { subscribers = []
    }
