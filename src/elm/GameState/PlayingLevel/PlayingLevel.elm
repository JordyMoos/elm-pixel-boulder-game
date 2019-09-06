module GameState.PlayingLevel.PlayingLevel exposing
    ( Action(..)
    , Model
    , init
    , updateTick
    , view
    )

import Actor.Actor as Actor
import Data.Config exposing (Config)
import GameState.PlayingLevel.Animation.Animation as Animation
import GameState.PlayingLevel.Completed.CompletedAnimation as CompletedAnimation
import GameState.PlayingLevel.Completed.CompletedDescription as CompletedDescription
import GameState.PlayingLevel.Completed.CompletedMenu as CompletedMenu
import GameState.PlayingLevel.DescriptionProvider as DescriptionProvider
import GameState.PlayingLevel.Failed.FailedAnimation as FailedAnimation
import GameState.PlayingLevel.Failed.FailedDescription as FailedDescription
import GameState.PlayingLevel.Failed.FailedMenu as FailedMenu
import GameState.PlayingLevel.Msg as PlayingMsg
import GameState.PlayingLevel.PauseMenu as PauseMenu
import GameState.PlayingLevel.Playing as Playing
import Html exposing (Html)
import InputController


type alias Model =
    { config : Config
    , levelConfig : Actor.LevelConfig
    , state : State
    }


type State
    = PlayingState Playing.Model
    | PauseMenuState PauseMenu.Model
    | FailedAnimationState FailedAnimation.Model
    | FailedDescriptionState FailedDescription.Model
    | FailedMenuState FailedMenu.Model
    | CompletedAnimationState CompletedAnimation.Model
    | CompletedDescriptionState CompletedDescription.Model
    | CompletedMenuState CompletedMenu.Model


type Action
    = Stay Model (Cmd PlayingMsg.Msg)
    | LoadLevel String
    | GotoMainMenu


init : Config -> Actor.LevelConfig -> Model
init config levelConfig =
    { config = config
    , levelConfig = levelConfig
    , state = PlayingState <| Playing.init config levelConfig
    }


updateTick : Int -> InputController.Model -> Model -> Action
updateTick currentTick inputModel model =
    case model.state of
        PlayingState subModel ->
            case Playing.updateTick currentTick inputModel subModel of
                Playing.Stay playingModel cmd ->
                    Stay { model | state = PlayingState playingModel } cmd

                Playing.GotoPauseMenu level ->
                    Stay
                        { model
                            | state = PauseMenuState <| PauseMenu.init model.config level
                        }
                        Cmd.none

                Playing.GotoLevel levelName ->
                    LoadLevel levelName

                Playing.Failed level data ->
                    Stay
                        { model
                            | state =
                                FailedAnimationState <|
                                    FailedAnimation.init
                                        model.config
                                        model.levelConfig
                                        (Animation.init
                                            (data.animationSetup
                                                model.config
                                                currentTick
                                            )
                                            model.levelConfig.entities
                                            data.entityNames
                                        )
                                        (DescriptionProvider.createDescription data.descriptionProvider level)
                                        level
                        }
                        Cmd.none

                Playing.Completed level data ->
                    Stay
                        { model
                            | state =
                                CompletedAnimationState <|
                                    CompletedAnimation.init
                                        model.config
                                        model.levelConfig
                                        (Animation.init
                                            (data.animationSetup
                                                model.config
                                                currentTick
                                            )
                                            model.levelConfig.entities
                                            data.entityNames
                                        )
                                        (DescriptionProvider.createDescription data.descriptionProvider level)
                                        data.nextLevel
                                        level
                        }
                        Cmd.none

        PauseMenuState subModel ->
            case PauseMenu.updateTick inputModel subModel of
                PauseMenu.Stay menuModel ->
                    Stay { model | state = PauseMenuState menuModel } Cmd.none

                PauseMenu.Resume level ->
                    Stay
                        { model
                            | state =
                                PlayingState <|
                                    Playing.resume
                                        model.config
                                        model.levelConfig
                                        level
                        }
                        Cmd.none

                PauseMenu.Restart ->
                    Stay
                        { model
                            | state =
                                PlayingState <|
                                    Playing.init
                                        model.config
                                        model.levelConfig
                        }
                        Cmd.none

                PauseMenu.GotoMainMenu ->
                    GotoMainMenu

        FailedAnimationState subModel ->
            case FailedAnimation.updateTick currentTick inputModel subModel of
                FailedAnimation.Stay animationModel ->
                    Stay { model | state = FailedAnimationState animationModel } Cmd.none

                FailedAnimation.Finished description ->
                    Stay { model | state = FailedDescriptionState <| FailedDescription.init model.config description } Cmd.none

        FailedDescriptionState subModel ->
            case FailedDescription.updateTick inputModel subModel of
                FailedDescription.Stay descriptionModel ->
                    Stay { model | state = FailedDescriptionState descriptionModel } Cmd.none

                FailedDescription.Finished ->
                    Stay { model | state = FailedMenuState <| FailedMenu.init model.config } Cmd.none

        FailedMenuState subModel ->
            case FailedMenu.updateTick inputModel subModel of
                FailedMenu.Stay menuModel ->
                    Stay { model | state = FailedMenuState menuModel } Cmd.none

                FailedMenu.Restart ->
                    Stay { model | state = PlayingState <| Playing.init model.config model.levelConfig } Cmd.none

                FailedMenu.GotoMainMenu ->
                    GotoMainMenu

        CompletedAnimationState subModel ->
            case CompletedAnimation.updateTick currentTick inputModel subModel of
                CompletedAnimation.Stay animationModel ->
                    Stay { model | state = CompletedAnimationState animationModel } Cmd.none

                CompletedAnimation.Finished description nextLevel ->
                    Stay { model | state = CompletedDescriptionState <| CompletedDescription.init model.config description nextLevel } Cmd.none

        CompletedDescriptionState subModel ->
            case CompletedDescription.updateTick inputModel subModel of
                CompletedDescription.Stay descriptionModel ->
                    Stay { model | state = CompletedDescriptionState descriptionModel } Cmd.none

                CompletedDescription.Finished nextLevel ->
                    Stay { model | state = CompletedMenuState <| CompletedMenu.init model.config nextLevel } Cmd.none

        CompletedMenuState subModel ->
            case CompletedMenu.updateTick inputModel subModel of
                CompletedMenu.Stay menuModel ->
                    Stay { model | state = CompletedMenuState menuModel } Cmd.none

                CompletedMenu.GotoNextLevel name ->
                    LoadLevel name

                CompletedMenu.Restart ->
                    Stay { model | state = PlayingState <| Playing.init model.config model.levelConfig } Cmd.none

                CompletedMenu.GotoMainMenu ->
                    GotoMainMenu


view : Int -> Model -> Html msg
view currentTick model =
    case model.state of
        PlayingState subModel ->
            Playing.view currentTick subModel

        PauseMenuState subModel ->
            PauseMenu.view subModel

        FailedAnimationState subModel ->
            FailedAnimation.view currentTick subModel

        FailedDescriptionState subModel ->
            FailedDescription.view subModel

        FailedMenuState subModel ->
            FailedMenu.view subModel

        CompletedAnimationState subModel ->
            CompletedAnimation.view currentTick subModel

        CompletedDescriptionState subModel ->
            CompletedDescription.view subModel

        CompletedMenuState subModel ->
            CompletedMenu.view subModel
