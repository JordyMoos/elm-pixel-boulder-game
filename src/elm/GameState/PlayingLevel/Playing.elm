module GameState.PlayingLevel.Playing
    exposing
        ( Model
        , Action(..)
        , init
        , resume
        , updateTick
        , view
        )

import Data.Config exposing (Config)
import Data.Direction as Direction exposing (Direction)
import Actor.Actor as Actor
import Actor.Common as Common
import Dict
import LevelInitializer
import InputController
import Renderer.Canvas.LevelRenderer as LevelRenderer
import Html exposing (Html)
import Actor.Component.TransformComponent as Transform
import Actor.Component.CollectorComponent as Collector
import Actor.Component.ControlComponent as Control
import Actor.Component.CameraComponent as Camera
import Actor.Component.DownSmashComponent as DownSmash
import Actor.Component.LifetimeComponent as Lifetime
import Actor.Component.DamageComponent as Damage
import Actor.Component.TriggerExplodableComponent as TriggerExplodable
import Actor.Component.SpawnComponent as Spawn
import Actor.EventManager as EventManager


updateBorder : Int
updateBorder =
    5


type alias Model =
    { config : Config
    , levelConfig : Actor.LevelConfig
    , images : Actor.CanvasImages
    , level : Actor.Level
    , eventManager : Actor.EventManager
    }


type Action
    = Stay Model
    | GotoPauseMenu Actor.Level
    | Failed Actor.Level Actor.LevelFailedData
    | Completed Actor.Level


init : Config -> Actor.LevelConfig -> Actor.CanvasImages -> Model
init config levelConfig images =
    { config = config
    , levelConfig = levelConfig
    , images = images
    , level = LevelInitializer.initLevel config levelConfig
    , eventManager =
        { subscribers = levelConfig.subscribers
        }
    }


resume : Config -> Actor.LevelConfig -> Actor.CanvasImages -> Actor.Level -> Model
resume config levelConfig images level =
    { config = config
    , levelConfig = levelConfig
    , images = images
    , level = level
    , eventManager =
        { subscribers = levelConfig.subscribers
        }
    }


updateTick : Int -> InputController.Model -> Model -> Action
updateTick currentTick inputModel model =
    case InputController.getOrderedPressedKeys inputModel |> List.head of
        Just InputController.StartKey ->
            GotoPauseMenu model.level

        _ ->
            updateLevel
                (InputController.getCurrentDirection inputModel)
                model.level
                model.levelConfig
                |> setLevel model
                |> processEvents


updateLevel : Maybe Direction -> Actor.Level -> Actor.LevelConfig -> Actor.Level
updateLevel maybeDirection level levelConfig =
    List.foldr
        (\y level ->
            List.foldr
                (\x level ->
                    Common.getActorIdsByXY x y level
                        |> List.foldr
                            (\actorId level ->
                                Common.getActorById actorId level
                                    |> Maybe.andThen
                                        (\actor ->
                                            Dict.foldr
                                                (\_ component level ->
                                                    Common.getActorById actorId level
                                                        |> Maybe.andThen
                                                            (\actor ->
                                                                let
                                                                    updatedLevel =
                                                                        case component of
                                                                            Actor.TransformComponent transformData ->
                                                                                Transform.updateTransformComponent transformData actor level

                                                                            Actor.CollectorComponent data ->
                                                                                Collector.updateCollectorComponent data actor level

                                                                            Actor.ControlComponent control ->
                                                                                Control.updateControlComponent maybeDirection control actor level

                                                                            Actor.CameraComponent camera ->
                                                                                Camera.updateCameraComponent camera actor level

                                                                            Actor.DownSmashComponent downSmash ->
                                                                                DownSmash.updateDownSmashComponent downSmash actor level

                                                                            Actor.LifetimeComponent lifetimeData ->
                                                                                Lifetime.updateLifetimeComponent lifetimeData actor level

                                                                            Actor.DamageComponent damageData ->
                                                                                Damage.updateDamageComponent damageData actor level

                                                                            Actor.TriggerExplodableComponent triggerData ->
                                                                                TriggerExplodable.updateTriggerExplodableComponent triggerData actor level

                                                                            Actor.SpawnComponent spawnData ->
                                                                                Spawn.updateSpawnComponent levelConfig.entities spawnData actor level

                                                                            _ ->
                                                                                level
                                                                in
                                                                    Just updatedLevel
                                                            )
                                                        |> Maybe.withDefault level
                                                )
                                                level
                                                actor.components
                                                |> Just
                                        )
                                    |> Maybe.withDefault level
                            )
                            level
                )
                level
                (List.range (level.view.position.x - updateBorder) (level.view.position.x + level.view.width + updateBorder))
        )
        level
        (List.range (level.view.position.y - updateBorder) (level.view.position.y + level.view.height + updateBorder))


processEvents : Model -> Action
processEvents model =
    List.foldr
        (handleEvent model.eventManager)
        (Actor.LevelContinue model.level)
        model.level.events
        |> mapEventActionToAction model


mapEventActionToAction : Model -> Actor.EventAction -> Action
mapEventActionToAction model eventAction =
    case eventAction of
        Actor.LevelContinue level ->
            level
                |> clearEvents
                |> setLevel model
                |> Stay

        Actor.LevelFailed data ->
            Failed model.level data

        Actor.LevelCompleted ->
            Completed model.level


handleEvent : Actor.EventManager -> Actor.Event -> Actor.EventAction -> Actor.EventAction
handleEvent eventManager event accumulatedAction =
    List.foldr
        (\subscriber action ->
            case action of
                Actor.LevelContinue level ->
                    subscriber event level

                -- Void events if action is already decided
                _ ->
                    action
        )
        accumulatedAction
        eventManager.subscribers


clearEvents : Actor.Level -> Actor.Level
clearEvents level =
    { level | events = [] }


setLevel : Model -> Actor.Level -> Model
setLevel model level =
    { model | level = level }


view : Int -> Model -> Html msg
view currentTick model =
    LevelRenderer.renderLevel currentTick model.level model.images
