module GameState.PlayingLevel.Completed.CompletedMenu
    exposing
        ( Model
        , Action(..)
        , init
        , updateTick
        , view
        )

import Text
import Data.Menu as Menu
import Data.Config exposing (Config)
import InputController
import Html exposing (Html, div)
import Renderer.Canvas.MenuRenderer as MenuRenderer
import List.Extra
import Maybe.Extra
import Color
import Actor.Actor as Actor
import Menu.TextMenu as TextMenu


type alias Model =
    { textMenu : TextMenu.Model Action
    }


type Action
    = Stay Model
    | GotoNextLevel String
    | Restart
    | GotoMainMenu


init : Config -> String -> Model
init config nextLevel =
    { textMenu =
        TextMenu.init config
            { items =
                { before = []
                , selected =
                    { text = Text.stringToLetters "Next Level"
                    , action = GotoNextLevel nextLevel
                    }
                , after =
                    [ { text = Text.stringToLetters "Restart"
                      , action = Restart
                      }
                    , { text = Text.stringToLetters "Quit"
                      , action = GotoMainMenu
                      }
                    ]
                }
            }
    }


updateTick : InputController.Model -> Model -> Action
updateTick inputModel model =
    case TextMenu.updateTick inputModel model.textMenu of
        TextMenu.Stay textMenu ->
            Stay { model | textMenu = textMenu }

        TextMenu.Invoke action ->
            action


view : Model -> Html msg
view model =
    TextMenu.view model.textMenu
