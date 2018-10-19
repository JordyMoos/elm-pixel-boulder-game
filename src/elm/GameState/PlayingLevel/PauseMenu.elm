module GameState.PlayingLevel.PauseMenu exposing
    ( Action(..)
    , Model
    , init
    , updateTick
    , view
    )

import Actor.Actor as Actor
import Color
import Data.Config exposing (Config)
import Data.Menu as Menu
import Html exposing (Html, div)
import InputController
import List.Extra
import Maybe.Extra
import Menu.TextMenu as TextMenu
import Text


type alias Model =
    { textMenu : TextMenu.Model Action
    }


type Action
    = Stay Model
    | Resume Actor.Level
    | Restart
    | GotoMainMenu


init : Config -> Actor.Level -> Model
init config level =
    { textMenu =
        TextMenu.init config
            { items =
                { before = []
                , selected =
                    { text = Text.stringToLetters "Resume"
                    , action = Resume level
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
