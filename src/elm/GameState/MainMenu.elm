module GameState.MainMenu
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
import List.Extra
import Maybe.Extra
import Color
import Menu.TextMenu as TextMenu


type alias Model =
    { textMenu : TextMenu.Model Action
    }


type Action
    = Stay Model
    | LoadLevel String
    | LoadFlags


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
                    [ { text = Text.stringToLetters "Pixel"
                      , action = LoadLevel "test/pixel"
                      }
                    , { text = Text.stringToLetters "Nes"
                      , action = LoadLevel "test/nes-small"
                      }
                    , { text = Text.stringToLetters "Images"
                      , action = LoadLevel "test/images"
                      }
                    , { text = Text.stringToLetters "Pac-Man"
                      , action = LoadLevel "test/pacman"
                      }
                    , { text = Text.stringToLetters "Tank"
                      , action = LoadLevel "test/tank"
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
