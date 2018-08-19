module Actor.Actor
    exposing
        ( ActorId
        , Actor
        , Actors
        , Level
        , Scene
        , Entities
        , View
        , Signs
        , Images
        , CanvasImages
        , LevelConfig
        , Component(..)
        , Components
        , PositionIndex
        , KeyedComponent
          -- TransformComponent
        , TransformComponentData
        , MovingTowardsData
        , MovingState(..)
          -- PhysicsComponent
        , PhysicsComponentData
        , Shape(..)
          -- CollectibleComponent
        , CollectibleComponentData
          -- CollectorComponent
        , CollectorComponentData
        , Inventory
          -- ControlComponent
        , ControlComponentData
        , ControlSettings
        , ControlType(..)
        , WalkAroundAiControlData
          -- CameraComponent
        , CameraComponentData
          -- TriggerExplodableComponent
        , TriggerExplodableComponentData
          -- LifetimeComponent
        , LifetimeComponentData
          -- DamageComponent
        , DamageComponentData
          -- SpawnComponent
        , SpawnComponentData
        , SpawnRepeat
        , SpawnRepeatTimes(..)
          -- DownSmashComponent
        , DownSmashComponentData
        , MovingDownState(..)
          -- RenderComponent
        , RenderComponentData(..)
        , PixelRenderComponentData
        , ImageRenderComponentData
        , ImagesData
          -- TagComponent
        , TagComponentData
          -- Animation
        , AnimationSetup
          -- EventManager
        , Event(..)
        , Events
        , EventAction(..)
        , LevelFailedData
        , LevelCompletedData
        , Subscriber
        , Subscribers
        , EventManager
        , emptyEventManager
        )

import Dict exposing (Dict)
import Data.Config exposing (Config)
import Data.Direction exposing (Direction)
import Data.Position exposing (Position)
import Data.Coordinate exposing (Coordinate)
import Canvas exposing (Canvas)
import Color exposing (Color)


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
    { coordinate : Coordinate
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
    Dict String Canvas


type alias Images =
    Dict String String


type alias LevelConfig =
    { entities : Entities
    , signs : Signs
    , scene : Scene
    , images : Images
    , background : RenderComponentData
    , subscribers : Subscribers
    }


type alias Level =
    { nextActorId : Int
    , actors : Actors
    , positionIndex : PositionIndex
    , view : View
    , background : RenderComponentData
    , events : Events
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
    | TagComponent TagComponentData



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



{-

   CameraComponent

-}


type alias CameraComponentData =
    { borderSize : Int
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
    }



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


type RenderComponentData
    = PixelRenderComponent PixelRenderComponentData
    | ImageRenderComponent ImageRenderComponentData


type alias PixelRenderComponentData =
    { colors : List Color
    , ticksPerColor : Int
    }


type alias ImagesData =
    { names : List String
    , ticksPerImage : Int
    }


type alias ImageRenderComponentData =
    { default : ImagesData
    , direction : Dict Int ImagesData
    }



{-

   TagComponent

-}


type alias TagComponentData =
    { name : String
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


type EventAction
    = LevelContinue Level
    | LevelFailed LevelFailedData
    | LevelCompleted LevelCompletedData


type alias LevelFailedData =
    { description : String
    , entityNames : List String
    , animationSetup : AnimationSetup
    }


type alias LevelCompletedData =
    { description : String
    , nextLevel : String
    , entityNames : List String
    , animationSetup : AnimationSetup
    }


type alias Events =
    List Event


type alias Subscriber =
    Event -> Level -> EventAction


type alias Subscribers =
    List Subscriber


type alias EventManager =
    { subscribers : Subscribers
    }


emptyEventManager : EventManager
emptyEventManager =
    { subscribers = []
    }
