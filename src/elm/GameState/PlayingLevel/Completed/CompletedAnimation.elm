module GameState.PlayingLevel.Completed.CompletedAnimation exposing
    ( Action(..)
    , Model
    , init
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
import GameState.PlayingLevel.Animation.Animation as Animation
import Html exposing (Html)
import InputController
import LevelInitializer
import Renderer.Svg.LevelRenderer as LevelRenderer


type alias Model =
    { config : Config
    , levelConfig : Actor.LevelConfig
    , level : Actor.Level
    , animationModel : Animation.Model
    , description : String
    , nextLevel : String
    }


type Action
    = Stay Model
    | Finished String String


init : Config -> Actor.LevelConfig -> Animation.Model -> String -> String -> Actor.Level -> Model
init config levelConfig animationModel description nextLevel level =
    { config = config
    , levelConfig = levelConfig
    , level = level
    , animationModel = animationModel
    , description = description
    , nextLevel = nextLevel
    }


updateTick : Int -> InputController.Model -> Model -> Action
updateTick currentTick inputModel model =
    case InputController.getOrderedPressedKeys inputModel |> List.head of
        Just InputController.StartKey ->
            Finished model.description model.nextLevel

        _ ->
            case Animation.updateTick currentTick model.animationModel model.level of
                Animation.Stay animationModel level ->
                    model
                        |> (\a -> setAnimation a animationModel)
                        |> (\a -> setLevel a level)
                        |> (\model ->
                                LevelUpdate.update
                                    (InputController.getCurrentDirection inputModel)
                                    model.level
                                    model.levelConfig
                                    -- We won't handle events
                                    |> EventManager.clearEvents
                                    -- Prevents view movement
                                    |> Common.setView model.level.view
                                    |> setLevel model
                           )
                        |> Stay

                Animation.Finished ->
                    Finished model.description model.nextLevel


setAnimation : Model -> Animation.Model -> Model
setAnimation model animationModel =
    { model | animationModel = animationModel }


setLevel : Model -> Actor.Level -> Model
setLevel model level =
    { model | level = level }


view : Int -> Model -> Html msg
view currentTick model =
    LevelRenderer.renderLevel currentTick model.config model.level model.levelConfig.images
