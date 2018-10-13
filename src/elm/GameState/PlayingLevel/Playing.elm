module GameState.PlayingLevel.Playing exposing
    ( Action(..)
    , Model
    , init
    , resume
    , updateTick
    , view
    )

import Actor.Actor as Actor
import Actor.Common as Common
import Actor.EventManager as EventManager
import Actor.LevelUpdate as LevelUpdate
import Data.Config exposing (Config)
import Data.Direction as Direction exposing (Direction)
import Dict
import Html exposing (Html)
import InputController
import LevelInitializer
import Renderer.Svg.LevelRenderer as LevelRenderer


type alias Model =
    { config : Config
    , levelConfig : Actor.LevelConfig
    , level : Actor.Level
    , eventManager : Actor.EventManager
    }


type Action
    = Stay Model
    | GotoPauseMenu Actor.Level
    | Failed Actor.Level Actor.LevelFailedData
    | Completed Actor.Level Actor.LevelCompletedData


init : Config -> Actor.LevelConfig -> Model
init config levelConfig =
    { config = config
    , levelConfig = levelConfig
    , level = LevelInitializer.initLevel config levelConfig
    , eventManager =
        { subscribers = levelConfig.subscribers
        }
    }


resume : Config -> Actor.LevelConfig -> Actor.Level -> Model
resume config levelConfig level =
    { config = config
    , levelConfig = levelConfig
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
    LevelRenderer.renderLevel currentTick model.config model.level model.levelConfig.images
