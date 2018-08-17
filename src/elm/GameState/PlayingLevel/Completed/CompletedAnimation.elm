module GameState.PlayingLevel.Completed.CompletedAnimation
    exposing
        ( Model
        , Action(..)
        , init
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
import GameState.PlayingLevel.Animation.Animation as Animation
import Actor.LevelUpdate as LevelUpdate


type alias Model =
    { config : Config
    , levelConfig : Actor.LevelConfig
    , images : Actor.CanvasImages
    , level : Actor.Level
    , animationModel : Animation.Model
    , description : String
    , nextLevel : String
    }


type Action
    = Stay Model
    | Finished String String


init : Config -> Actor.LevelConfig -> Actor.CanvasImages -> Animation.Model -> String -> String -> Actor.Level -> Model
init config levelConfig images animationModel description nextLevel level =
    { config = config
    , levelConfig = levelConfig
    , images = images
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
                        |> flip setAnimation animationModel
                        |> flip setLevel level
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
    LevelRenderer.renderLevel currentTick model.level model.images
