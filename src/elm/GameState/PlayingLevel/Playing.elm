module GameState.PlayingLevel.Playing exposing
    ( Action(..)
    , Model
    , init
    , resume
    , updateTick
    , view
    )

import Actor.Actor as Actor
import Actor.EventManager as EventManager
import Actor.LevelUpdate as LevelUpdate
import Data.Config exposing (Config)
import Html exposing (Html)
import InputController
import LevelInitializer
import Renderer.Aframe.LevelRenderer as AframeLevelRenderer
import Renderer.Svg.LevelRenderer as SvgLevelRenderer


type alias Model =
    { config : Config
    , levelConfig : Actor.LevelConfig
    , level : Actor.Level
    }


type Action
    = Stay Model
    | GotoPauseMenu Actor.Level
    | Failed Actor.Level Actor.LevelFailedData
    | Completed Actor.Level Actor.LevelCompletedData
    | GotoLevel String


init : Config -> Actor.LevelConfig -> Model
init config levelConfig =
    { config = config
    , levelConfig = levelConfig
    , level = LevelInitializer.initLevel config levelConfig
    }


resume : Config -> Actor.LevelConfig -> Actor.Level -> Model
resume config levelConfig level =
    { config = config
    , levelConfig = levelConfig
    , level = level
    }


updateTick : Int -> InputController.Model -> Model -> Action
updateTick currentTick inputModel model =
    case InputController.getOrderedPressedKeys inputModel |> List.head of
        Just InputController.StartKey ->
            GotoPauseMenu model.level

        _ ->
            LevelUpdate.update
                currentTick
                inputModel
                model.level
                model.levelConfig
                |> setLevel model
                |> processEvents


processEvents : Model -> Action
processEvents model =
    List.foldr
        (handleEvent model.level)
        ( model.level.eventManager, Actor.LevelContinue )
        model.level.events
        |> mapEventActionToAction model


handleEvent : Actor.Level -> Actor.Event -> ( Actor.EventManager, Actor.EventAction ) -> ( Actor.EventManager, Actor.EventAction )
handleEvent level event ( accumulatedEventManager, accumulatedAction ) =
    accumulatedEventManager.subscribers
        |> List.foldr
            (\subscriber ( updatedSubscribers, accAction ) ->
                case accAction of
                    Actor.LevelContinue ->
                        let
                            ( updatedSubscriber, eventAction ) =
                                case subscriber of
                                    Actor.TagDiedSubscriber onResolveAction data ->
                                        EventManager.onTagDiedSubscriber onResolveAction data event level

                                    Actor.InventoryUpdatedSubscriber onResolveAction data ->
                                        EventManager.onInventoryUpdatedSubscriber onResolveAction data event level
                        in
                        ( updatedSubscriber :: updatedSubscribers, eventAction )

                    -- Void events if action is already decided
                    _ ->
                        ( subscriber :: updatedSubscribers, accAction )
            )
            ( [], accumulatedAction )
        |> (\( subscribers, eventAction ) ->
                ( { subscribers = subscribers }, eventAction )
           )


mapEventActionToAction : Model -> ( Actor.EventManager, Actor.EventAction ) -> Action
mapEventActionToAction model ( eventManager, eventAction ) =
    case eventAction of
        Actor.LevelContinue ->
            model.level
                |> clearEvents
                |> setEventManager eventManager
                |> setLevel model
                |> Stay

        Actor.LevelFailed data ->
            Failed
                (setEventManager eventManager model.level)
                data

        Actor.LevelCompleted data ->
            Completed
                (setEventManager eventManager model.level)
                data

        Actor.LoadLevel data ->
            GotoLevel data.nextLevel


clearEvents : Actor.Level -> Actor.Level
clearEvents level =
    { level | events = [] }


setEventManager : Actor.EventManager -> Actor.Level -> Actor.Level
setEventManager eventManager level =
    { level | eventManager = eventManager }


setLevel : Model -> Actor.Level -> Model
setLevel model level =
    { model | level = level }


view : Int -> Model -> Html msg
view currentTick model =
    case model.levelConfig.renderer of
        Actor.SvgRenderer ->
            SvgLevelRenderer.renderLevel currentTick model.level model.levelConfig

        Actor.AframeRenderer aframeData ->
            AframeLevelRenderer.renderLevel aframeData currentTick model.level model.levelConfig
