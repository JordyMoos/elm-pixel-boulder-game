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
import Actor.EventManager as EventManager
import Actor.LevelUpdate as LevelUpdate


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
    | Completed Actor.Level Actor.LevelCompletedData


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
            LevelUpdate.update
                (InputController.getCurrentDirection inputModel)
                model.level
                model.levelConfig
                |> setLevel model
                |> processEvents


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

        Actor.LevelCompleted data ->
            Completed model.level data


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
