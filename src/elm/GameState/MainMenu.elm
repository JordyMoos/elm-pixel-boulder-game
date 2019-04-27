module GameState.MainMenu exposing
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
    | LoadLevel String
    | LoadFlags
    | ShowCredits


init : Config -> Model
init config =
    { textMenu =
        TextMenu.init config
            { items =
                { before = []
                , selected =
                    { text = Text.stringToLetters "Json"
                    , action = LoadFlags
                    }
                , after =
                    [ { text = Text.stringToLetters "Official levels"
                      , action = LoadLevel "official/001"
                      }
                    , { text = Text.stringToLetters "Advent levels"
                      , action = LoadLevel "advent/sample-01"
                      }
                    , { text = Text.stringToLetters "Credits"
                      , action = ShowCredits
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
