module GameState.PlayingLevel.Failed.FailedAnimation exposing
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
import GameState.PlayingLevel.Animation.Animation as Animation
import Html exposing (Html)
import InputController
import Renderer.Svg.LevelRenderer as LevelRenderer


type alias Model =
    { config : Config
    , levelConfig : Actor.LevelConfig
    , level : Actor.Level
    , animationModel : Animation.Model
    , description : String
    }


type Action
    = Stay Model
    | Finished String


init : Config -> Actor.LevelConfig -> Animation.Model -> String -> Actor.Level -> Model
init config levelConfig animationModel description level =
    { config = config
    , levelConfig = levelConfig
    , level = level
    , animationModel = animationModel
    , description = description
    }


updateTick : Int -> InputController.Model -> Model -> Action
updateTick currentTick inputModel model =
    case InputController.getOrderedPressedKeys inputModel |> List.head of
        Just InputController.StartKey ->
            Finished model.description

        _ ->
            case Animation.updateTick currentTick model.animationModel model.level of
                Animation.Stay animationModel level ->
                    model
                        |> (\a -> setAnimation a animationModel)
                        |> (\a -> setLevel a level)
                        |> (\updatedModel ->
                                LevelUpdate.update inputModel updatedModel.level updatedModel.levelConfig
                                    -- We won't handle events
                                    |> EventManager.clearEvents
                                    -- Prevents view movement
                                    |> Common.setView updatedModel.level.view
                                    |> setLevel updatedModel
                           )
                        |> Stay

                Animation.Finished ->
                    Finished model.description


setAnimation : Model -> Animation.Model -> Model
setAnimation model animationModel =
    { model | animationModel = animationModel }


setLevel : Model -> Actor.Level -> Model
setLevel model level =
    { model | level = level }


view : Int -> Model -> Html msg
view currentTick model =
    LevelRenderer.renderLevel currentTick model.config model.level model.levelConfig.images
