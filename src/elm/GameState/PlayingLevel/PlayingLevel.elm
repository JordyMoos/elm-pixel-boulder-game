module GameState.PlayingLevel.PlayingLevel
    exposing
        ( Model
        , Action(..)
        , init
        , updateTick
        , view
        )

import Data.Config exposing (Config)
import Actor.Actor as Actor
import GameState.PlayingLevel.Playing as Playing
import GameState.PlayingLevel.PauseMenu as PauseMenu
import InputController
import Html exposing (Html)


type alias Model =
    { config : Config
    , levelConfig : Actor.LevelConfig
    , images : Actor.CanvasImages
    , state : State
    }


type State
    = PlayingState Playing.Model
    | PauseMenuState PauseMenu.Model


type Action
    = Stay Model
    | GotoMainMenu


init : Config -> Actor.LevelConfig -> Actor.CanvasImages -> Model
init config levelConfig images =
    { config = config
    , levelConfig = levelConfig
    , images = images
    , state = PlayingState <| Playing.init config levelConfig images
    }


updateTick : Int -> InputController.Model -> Model -> Action
updateTick currentTick inputModel model =
    case model.state of
        PlayingState subModel ->
            case Playing.updateTick currentTick inputModel subModel of
                Playing.Stay playingModel ->
                    Stay { model | state = PlayingState playingModel }

                Playing.GotoPauseMenu level ->
                    Stay
                        { model
                            | state = PauseMenuState <| PauseMenu.init model.config level
                        }

                -- @todo fix failed screen
                Playing.Failed error ->
                    GotoMainMenu

                -- @todo fix completed screen
                Playing.Completed ->
                    GotoMainMenu

        PauseMenuState subModel ->
            case PauseMenu.updateTick inputModel subModel of
                PauseMenu.Stay menuModel ->
                    Stay { model | state = PauseMenuState menuModel }

                PauseMenu.Resume level ->
                    Stay
                        { model
                            | state =
                                PlayingState <|
                                    Playing.resume
                                        model.config
                                        model.levelConfig
                                        model.images
                                        level
                        }

                PauseMenu.Restart ->
                    Stay
                        { model
                            | state =
                                PlayingState <|
                                    Playing.init
                                        model.config
                                        model.levelConfig
                                        model.images
                        }

                PauseMenu.GotoMainMenu ->
                    GotoMainMenu


view : Int -> Model -> Html msg
view currentTick model =
    case model.state of
        PlayingState subModel ->
            Playing.view currentTick subModel

        PauseMenuState subModel ->
            PauseMenu.view subModel
