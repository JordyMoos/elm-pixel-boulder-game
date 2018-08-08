module GameState.PlayingLevel exposing (..)

import Data.Config exposing (Config)
import Data.Common exposing (Tick, Direction)
import Actor
import Dict
import LevelInitializer
import InputController
import Renderer.Canvas.LevelRenderer as LevelRenderer
import Html exposing (Html)


updateBorder : Int
updateBorder =
    5


type alias Model =
    { config : Config
    , levelConfig : Actor.LevelConfig
    , images : Actor.CanvasImages
    , level : Actor.Level
    }


type Action
    = Stay Model
    | GotoMainMenu


init : Config -> Actor.LevelConfig -> Actor.CanvasImages -> Model
init config levelConfig images =
    { config = config
    , levelConfig = levelConfig
    , images = images
    , level = LevelInitializer.initLevel config levelConfig
    }


updateTick : Tick -> InputController.Model -> Model -> Action
updateTick currentTick inputModel model =
    case InputController.getOrderedPressedKeys inputModel |> List.head of
        Just InputController.StartKey ->
            GotoMainMenu

        _ ->
            updateLevel
                (InputController.getCurrentDirection inputModel)
                model.level
                |> setLevel model
                |> Stay


updateLevel : Maybe Direction -> Actor.Level -> Actor.Level
updateLevel maybeDirection level =
    List.foldr
        (\y level ->
            List.foldr
                (\x level ->
                    Actor.getActorIdsByXY x y level
                        |> List.foldr
                            (\actorId level ->
                                Actor.getActorById actorId level
                                    |> Maybe.andThen
                                        (\actor ->
                                            Dict.foldr
                                                (\_ component level ->
                                                    Actor.getActorById actorId level
                                                        |> Maybe.andThen
                                                            (\actor ->
                                                                let
                                                                    updatedLevel =
                                                                        case component of
                                                                            Actor.TransformComponent transformData ->
                                                                                Actor.updateTransformComponent transformData actor level

                                                                            Actor.CollectorComponent data ->
                                                                                Actor.updateCollectorComponent data actor level

                                                                            Actor.ControlComponent control ->
                                                                                Actor.updateControlComponent maybeDirection control actor level

                                                                            Actor.CameraComponent camera ->
                                                                                Actor.updateCameraComponent camera actor level

                                                                            Actor.DownSmashComponent downSmash ->
                                                                                Actor.updateDownSmashComponent downSmash actor level

                                                                            Actor.LifetimeComponent lifetimeData ->
                                                                                Actor.updateLifetimeComponent lifetimeData actor level

                                                                            Actor.DamageComponent damageData ->
                                                                                Actor.updateDamageComponent damageData actor level

                                                                            Actor.TriggerExplodableComponent triggerData ->
                                                                                Actor.updateTriggerExplodableComponent triggerData actor level

                                                                            Actor.SpawnComponent spawnData ->
                                                                                Actor.updateSpawnComponent spawnData actor level

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


setLevel : Model -> Actor.Level -> Model
setLevel model level =
    { model | level = level }


view : Tick -> Model -> Html msg
view currentTick model =
    LevelRenderer.renderLevel currentTick model.level model.images
