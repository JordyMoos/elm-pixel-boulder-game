module GameState.PlayingLevel.Completed.CompletedMenu exposing
    ( Action(..)
    , Model
    , init
    , updateTick
    , view
    )

import Data.Config exposing (Config)
import Html exposing (Html)
import InputController
import Menu.TextMenu as TextMenu
import Text


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
